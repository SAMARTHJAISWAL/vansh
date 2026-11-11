(* Recursive-descent SQL parser with precedence climbing for expressions.

   It aims to accept real analytics SQL across dialects rather than to
   validate it: anything that cannot carry a value between columns (type
   names, frame clauses, hints, LIMIT) is skipped with balanced-paren
   scanning. A statement that fails to parse is reported and skipped; the
   rest of the file is still analysed. *)

open Ast
module L = Lexer

exception Error of string * int * int

type state = { toks : L.token array; mutable pos : int }

let peek st = st.toks.(st.pos).kind
let peek_at st k = st.toks.(min (st.pos + k) (Array.length st.toks - 1)).kind
let advance st = if st.pos < Array.length st.toks - 1 then st.pos <- st.pos + 1

let fail st msg =
  let t = st.toks.(st.pos) in
  raise (Error (Printf.sprintf "%s (found %s)" msg (L.describe t.kind), t.line, t.col))

let is_kw st w = match peek st with L.Ident s -> s = w | _ -> false
let is_kw_at st k w = match peek_at st k with L.Ident s -> s = w | _ -> false
let accept_kw st w = if is_kw st w then (advance st; true) else false
let expect_kw st w = if not (accept_kw st w) then fail st (Printf.sprintf "expected %s" (String.uppercase_ascii w))
let accept st k = if peek st = k then (advance st; true) else false
let expect st k what = if not (accept st k) then fail st ("expected " ^ what)
let is_op st o = peek st = L.Op o

(* Words that end an expression, so they can never be an implicit alias. *)
let reserved =
  [ "select"; "from"; "where"; "group"; "by"; "having"; "qualify"; "order"; "limit"; "offset"; "fetch"; "union"; "intersect";
    "except"; "minus"; "join"; "inner"; "left"; "right"; "full"; "outer"; "cross"; "natural"; "on"; "using"; "as"; "and";
    "or"; "not"; "in"; "is"; "null"; "like"; "ilike"; "rlike"; "between"; "case"; "when"; "then"; "else"; "end"; "with";
    "over"; "partition"; "window"; "lateral"; "values"; "into"; "semi"; "anti"; "asof"; "positional"; "sample";
    "tablesample"; "returning"; "for"; "pivot"; "unpivot"; "exclude"; "replace"; "rename"; "distinct"; "all"; "set";
    "connect"; "start"; "match_recognize"; "array"; "filter"; "within"; "escape"; "similar"; "regexp"; Jinja.placeholder ]

let is_reserved w = List.mem w reserved

(* Skip a balanced (...) group, starting on the opening paren. *)
let skip_parens st =
  expect st L.Lparen "(";
  let depth = ref 1 in
  while !depth > 0 do
    (match peek st with
    | L.Lparen -> incr depth
    | L.Rparen -> decr depth
    | L.Eof -> fail st "unbalanced parentheses"
    | _ -> ());
    advance st
  done

let ident_opt st =
  match peek st with
  | L.Ident s ->
      advance st;
      Some s
  | L.Quoted s ->
      advance st;
      Some s
  | _ -> None

let ident st what = match ident_opt st with Some s -> s | None -> fail st ("expected " ^ what)

(* Any identifier, including reserved words (column names like "end"). *)
let dotted_name st =
  let first = ident st "a name" in
  let parts = ref [ first ] in
  while peek st = L.Dot && (match peek_at st 1 with L.Ident _ | L.Quoted _ -> true | _ -> false) do
    advance st;
    parts := ident st "a name" :: !parts
  done;
  List.rev !parts

(* A data type after :: or in CAST(x AS type). Its spelling never matters. *)
let skip_type st =
  ignore (ident_opt st);
  let multiword = [ "precision"; "varying"; "with"; "without"; "time"; "zone"; "unsigned"; "local" ] in
  let rec more () =
    match peek st with
    | L.Ident w when List.mem w multiword -> advance st; more ()
    | L.Lparen -> skip_parens st; more ()
    | L.Lbracket when peek_at st 1 = L.Rbracket -> advance st; advance st; more ()
    | L.Op "<" ->
        (* array<struct<a int>> *)
        let depth = ref 0 in
        let continue = ref true in
        while !continue do
          (match peek st with
          | L.Op "<" -> incr depth
          | L.Op ">" -> decr depth
          | L.Op ">>" -> depth := !depth - 2
          | L.Eof -> continue := false
          | _ -> ());
          advance st;
          if !depth <= 0 then continue := false
        done;
        more ()
    | _ -> ()
  in
  more ()

let alias_opt st =
  if accept_kw st "as" then
    match peek st with
    | L.String s -> advance st; Some s
    | _ -> Some (ident st "an alias")
  else
    match peek st with
    | L.Ident w when not (is_reserved w) -> advance st; Some w
    | L.Quoted w -> advance st; Some w
    | _ -> None

let ident_list_in_parens st =
  expect st L.Lparen "(";
  let rec go acc =
    let name = ident st "a column name" in
    (* tolerate "col type" in column lists of CREATE ... AS *)
    while (match peek st with L.Comma | L.Rparen | L.Eof -> false | _ -> true) do
      if peek st = L.Lparen then skip_parens st else advance st
    done;
    if accept st L.Comma then go (name :: acc) else List.rev (name :: acc)
  in
  let names = go [] in
  expect st L.Rparen ")";
  names

let is_macro st = is_kw st Jinja.placeholder

(* Macros that expand to whole clauses (a WHERE, a GROUP BY) between the
   clauses vansh understands are skipped. *)
let skip_macros st = while is_macro st do advance st done
let starts_query st = is_kw st "select" || is_kw st "with" || is_kw st "values"

(* A relation produced by a macro: columns unknown, sources unknown. *)
let opaque_from = Table_func { fname = Jinja.placeholder; args = []; alias = None; col_aliases = [] }
let opaque_select = { items = [ All { qual = []; except = [] } ]; from = [ opaque_from ]; where = None; group_by = []; having = None; qualify = None }

(* ------------------------------------------------------------ expressions *)

let rec parse_expr st = parse_or st

and parse_or st =
  let l = parse_and st in
  if accept_kw st "or" then Op ("or", [ l; parse_or st ]) else l

and parse_and st =
  let l = parse_not st in
  if accept_kw st "and" then Op ("and", [ l; parse_and st ]) else l

and parse_not st = if accept_kw st "not" then Op ("not", [ parse_not st ]) else parse_comparison st

and parse_comparison st =
  let l = parse_additive st in
  let rec loop l =
    match peek st with
    | L.Op (("=" | "<>" | "!=" | "<" | ">" | "<=" | ">=" | "==" | "~" | "~*" | "!~" | "!~*") as o) ->
        advance st;
        loop (Op (o, [ l; parse_additive st ]))
    | L.Ident "is" ->
        advance st;
        ignore (accept_kw st "not");
        if accept_kw st "distinct" then (
          expect_kw st "from";
          loop (Op ("is distinct from", [ l; parse_additive st ])))
        else (
          ignore (ident st "NULL, TRUE or FALSE");
          loop (Op ("is", [ l ])))
    | L.Ident ("isnull" | "notnull") -> advance st; loop (Op ("is", [ l ]))
    | L.Ident "not" when is_kw_at st 1 "in" || is_kw_at st 1 "between" || is_kw_at st 1 "like" || is_kw_at st 1 "ilike"
                         || is_kw_at st 1 "rlike" || is_kw_at st 1 "regexp" || is_kw_at st 1 "similar" ->
        advance st;
        loop (Op ("not", [ suffix st l ]))
    | L.Ident ("in" | "between" | "like" | "ilike" | "rlike" | "regexp" | "similar") -> loop (suffix st l)
    | _ -> l
  in
  loop l

(* IN / BETWEEN / LIKE after the left operand. *)
and suffix st l =
  match peek st with
  | L.Ident "in" ->
      advance st;
      if peek st = L.Lparen && (is_kw_at st 1 "select" || is_kw_at st 1 "with") then (
        advance st;
        let q = parse_query st in
        expect st L.Rparen ")";
        Op ("in", [ l; Subquery q ]))
      else if accept st L.Lparen then (
        let items = if peek st = L.Rparen then [] else parse_expr_list st in
        expect st L.Rparen ")";
        Op ("in", l :: items))
      else Op ("in", [ l; parse_primary st ]) (* IN UNNEST(x), IN @param *)
  | L.Ident "between" ->
      advance st;
      ignore (accept_kw st "symmetric");
      let lo = parse_additive st in
      expect_kw st "and";
      Op ("between", [ l; lo; parse_additive st ])
  | L.Ident ("like" | "ilike" | "rlike" | "regexp" | "similar" as w) ->
      advance st;
      if w = "similar" then expect_kw st "to";
      if is_kw st "any" || is_kw st "all" then advance st;
      let r = parse_additive st in
      if accept_kw st "escape" then ignore (parse_additive st);
      Op (w, [ l; r ])
  | _ -> fail st "expected IN, BETWEEN or LIKE"

and parse_additive st =
  let l = parse_multiplicative st in
  let rec loop l =
    match peek st with
    | L.Op (("+" | "-" | "||" | "&" | "|" | "^" | "->" | "->>" | "#" | "@") as o) ->
        advance st;
        loop (Op (o, [ l; parse_multiplicative st ]))
    | _ -> l
  in
  loop l

and parse_multiplicative st =
  let l = parse_unary st in
  let rec loop l =
    match peek st with
    | L.Op (("*" | "/" | "%") as o) ->
        advance st;
        loop (Op (o, [ l; parse_unary st ]))
    | L.Ident ("div" | "mod") when not (peek_at st 1 = L.Lparen) ->
        advance st;
        loop (Op ("/", [ l; parse_unary st ]))
    | _ -> l
  in
  loop l

and parse_unary st =
  match peek st with
  | L.Op (("-" | "+" | "~" | "!") as o) ->
      advance st;
      Op (o, [ parse_unary st ])
  | _ -> parse_postfix st

and parse_postfix st =
  let e = parse_primary st in
  let rec loop e =
    match peek st with
    | L.Op "::" ->
        advance st;
        skip_type st;
        loop (Op ("cast", [ e ]))
    | L.Lbracket ->
        advance st;
        let i = if peek st = L.Rbracket then Literal else parse_expr st in
        ignore (accept st (L.Op ":") && (ignore (if peek st <> L.Rbracket then parse_expr st else Literal); true));
        expect st L.Rbracket "]";
        loop (Op ("[]", [ e; i ]))
    | L.Op ":" when (match peek_at st 1 with L.Ident _ | L.Quoted _ | L.String _ -> true | _ -> false) ->
        (* Snowflake semi-structured path: payload:customer.id *)
        advance st;
        advance st;
        while peek st = L.Dot || (match peek st with L.Lbracket -> true | _ -> false) do
          if peek st = L.Dot then (advance st; advance st)
          else (advance st; ignore (parse_expr st); expect st L.Rbracket "]")
        done;
        loop (Op ("path", [ e ]))
    | L.Dot when (match e with Column _ -> false | _ -> true) ->
        (* (struct_expr).field *)
        advance st;
        ignore (ident st "a field name");
        loop (Op ("field", [ e ]))
    | L.Ident "collate" -> advance st; ignore (ident_opt st); loop e
    | L.Ident "at" when is_kw_at st 1 "time" ->
        advance st; advance st; expect_kw st "zone";
        loop (Op ("at time zone", [ e; parse_additive st ]))
    | _ -> e
  in
  loop e

and parse_expr_list st =
  let e = parse_expr st in
  if accept st L.Comma then e :: parse_expr_list st else [ e ]

and parse_primary st : expr =
  match peek st with
  | L.Number _ | L.String _ -> advance st; Literal
  | L.Op ("?" | "@" | "$") -> advance st; ignore (ident_opt st); Literal
  | L.Op ":" -> advance st; ignore (ident_opt st); Literal (* :bind_param *)
  | L.Op "*" -> advance st; Star []
  | L.Lparen ->
      advance st;
      if starts_query st then (
        let q = parse_query st in
        expect st L.Rparen ")";
        Subquery q)
      else
        let items = parse_expr_list st in
        expect st L.Rparen ")";
        (match items with [ e ] -> e | es -> Op ("row", es))
  | L.Lbracket ->
      advance st;
      let items = if peek st = L.Rbracket then [] else parse_expr_list st in
      expect st L.Rbracket "]";
      Op ("array", items)
  | L.Ident "case" -> parse_case st
  | L.Ident ("cast" | "try_cast" | "safe_cast") when peek_at st 1 = L.Lparen ->
      advance st;
      advance st;
      let e = parse_expr st in
      if accept_kw st "as" then skip_type st else if accept st L.Comma then ignore (parse_expr st);
      ignore (accept_kw st "format" && (ignore (parse_expr st); true));
      expect st L.Rparen ")";
      Op ("cast", [ e ])
  | L.Ident "exists" when peek_at st 1 = L.Lparen ->
      advance st;
      advance st;
      let q = parse_query st in
      expect st L.Rparen ")";
      Subquery q
  | L.Ident ("any" | "some" | "all") when peek_at st 1 = L.Lparen ->
      advance st;
      advance st;
      let e = if starts_query st then Subquery (parse_query st) else Op ("row", parse_expr_list st) in
      expect st L.Rparen ")";
      e
  | L.Ident ("date" | "time" | "timestamp" | "timestamptz" | "datetime" | "interval" | "json" | "numeric" | "decimal")
    when (match peek_at st 1 with L.String _ | L.Number _ -> true | _ -> false) ->
      advance st;
      advance st;
      (* INTERVAL '1' DAY [TO SECOND] *)
      let units = [ "year"; "years"; "month"; "months"; "week"; "weeks"; "day"; "days"; "hour"; "hours"; "minute";
                    "minutes"; "second"; "seconds"; "to"; "quarter" ] in
      while (match peek st with L.Ident u -> List.mem u units | _ -> false) do advance st done;
      Literal
  | L.Ident "interval" ->
      advance st;
      let e = parse_additive st in
      ignore (ident_opt st);
      Op ("interval", [ e ])
  | L.Ident ("array" | "struct" | "map" | "row" | "list") when peek_at st 1 = L.Lbracket || peek_at st 1 = L.Lparen
                                                              || peek_at st 1 = L.Op "<" ->
      let fname = (match peek st with L.Ident w -> w | _ -> "") in
      advance st;
      if is_op st "<" then skip_type_args st;
      if peek st = L.Lbracket then (
        advance st;
        let items = if peek st = L.Rbracket then [] else parse_expr_list st in
        expect st L.Rbracket "]";
        Op (fname, items))
      else (
        advance st;
        let items =
          if starts_query st then [ Subquery (parse_query st) ]
          else if peek st = L.Rparen then []
          else parse_call_args st
        in
        expect st L.Rparen ")";
        Func { fname; args = items; agg_filter = None; over = None })
  | L.Ident ("null" | "true" | "false" | "current_date" | "current_timestamp" | "current_time" | "localtime"
            | "localtimestamp" | "current_user" | "session_user" | "default")
    when peek_at st 1 <> L.Lparen ->
      advance st;
      Literal
  | L.Ident w when w = Jinja.placeholder -> advance st; Literal
  | L.Ident _ | L.Quoted _ -> parse_name_or_call st
  | L.Op "{" -> fail st "unexpected Jinja (is this a dbt model?)"
  | _ -> fail st "expected an expression"

and skip_type_args st =
  let depth = ref 0 in
  let continue = ref true in
  while !continue do
    (match peek st with
    | L.Op "<" -> incr depth
    | L.Op ">" -> decr depth
    | L.Op ">>" -> depth := !depth - 2
    | L.Eof -> continue := false
    | _ -> ());
    advance st;
    if !depth <= 0 then continue := false
  done

and parse_case st =
  expect_kw st "case";
  let operand = if is_kw st "when" then None else Some (parse_expr st) in
  let rec branches acc =
    if accept_kw st "when" then (
      let c = parse_expr st in
      expect_kw st "then";
      let v = parse_expr st in
      branches ((c, v) :: acc))
    else List.rev acc
  in
  let branches = branches [] in
  let else_ = if accept_kw st "else" then Some (parse_expr st) else None in
  expect_kw st "end";
  Case { operand; branches; else_ }

and parse_name_or_call st =
  let name = dotted_name st in
  if peek st = L.Dot && peek_at st 1 = L.Op "*" then (
    advance st;
    advance st;
    Star name)
  else if peek st = L.Lparen then parse_call st (String.lowercase_ascii (dotted name))
  else Column name

(* Function arguments, including the keyword-separated forms
   SUBSTRING(x FROM 1 FOR 2), TRIM(BOTH ' ' FROM x), POSITION(a IN b),
   EXTRACT(year FROM d), named arguments (input => x), DISTINCT, ORDER BY
   inside aggregates and IGNORE NULLS. *)
and parse_call_args st =
  let rec go acc =
    ignore (accept_kw st "distinct" || accept_kw st "all");
    List.iter (fun w -> ignore (accept_kw st w)) [ "both"; "leading"; "trailing" ];
    let arg =
      if is_op st "*" && (peek_at st 1 = L.Rparen || peek_at st 1 = L.Comma) then (advance st; Star [])
      else if (match peek st with L.Ident _ -> true | _ -> false) && peek_at st 1 = L.Op "=>" then (
        advance st;
        advance st;
        parse_expr st)
      else if is_kw st "from" || is_kw st "order" || is_kw st "separator" then Literal
      else parse_expr st
    in
    let acc = arg :: acc in
    if accept st L.Comma then go acc
    else if accept_kw st "from" || accept_kw st "for" || accept_kw st "in" then go acc
    else if accept_kw st "separator" then (ignore (parse_expr st); go acc)
    else if is_kw st "order" then (
      advance st;
      expect_kw st "by";
      let keys = parse_order_list st in
      List.rev_append acc keys)
    else (
      if accept_kw st "ignore" || accept_kw st "respect" then expect_kw st "nulls";
      if accept_kw st "limit" then ignore (parse_expr st);
      List.rev acc)
  in
  go []

and parse_call st fname =
  expect st L.Lparen "(";
  let args = if peek st = L.Rparen then [] else parse_call_args st in
  expect st L.Rparen ")";
  (* WITHIN GROUP (ORDER BY x) *)
  let args =
    if is_kw st "within" && is_kw_at st 1 "group" then (
      advance st;
      advance st;
      expect st L.Lparen "(";
      expect_kw st "order";
      expect_kw st "by";
      let keys = parse_order_list st in
      expect st L.Rparen ")";
      args @ keys)
    else args
  in
  let agg_filter =
    if is_kw st "filter" && peek_at st 1 = L.Lparen then (
      advance st;
      advance st;
      expect_kw st "where";
      let e = parse_expr st in
      expect st L.Rparen ")";
      Some e)
    else None
  in
  if accept_kw st "ignore" || accept_kw st "respect" then expect_kw st "nulls";
  let over =
    if accept_kw st "over" then
      if peek st = L.Lparen then Some (parse_window_spec st)
      else (
        ignore (ident st "a window name");
        Some { partition = []; order = [] })
    else None
  in
  Func { fname; args; agg_filter; over }

and parse_window_spec st =
  expect st L.Lparen "(";
  (* optional base window name *)
  (match peek st with
  | L.Ident w when not (List.mem w [ "partition"; "order"; "rows"; "range"; "groups" ]) -> advance st
  | _ -> ());
  let partition =
    if accept_kw st "partition" then (expect_kw st "by"; parse_expr_list st) else []
  in
  let order = if accept_kw st "order" then (expect_kw st "by"; parse_order_list st) else [] in
  (* frame clause: nothing in it moves values *)
  let depth = ref 1 in
  while !depth > 0 do
    (match peek st with
    | L.Lparen -> incr depth
    | L.Rparen -> decr depth
    | L.Eof -> fail st "unterminated OVER (...)"
    | _ -> ());
    if !depth > 0 then advance st
  done;
  expect st L.Rparen ")";
  { partition; order }

and parse_order_list st =
  let e = parse_expr st in
  ignore (accept_kw st "asc" || accept_kw st "desc");
  if accept_kw st "nulls" then ignore (accept_kw st "first" || accept_kw st "last");
  if accept st L.Comma then e :: parse_order_list st else [ e ]

(* ------------------------------------------------------------ queries *)

and parse_query st : query =
  let ctes =
    if accept_kw st "with" then (
      ignore (accept_kw st "recursive");
      let rec go acc =
        let cte_name = ident st "a CTE name" in
        let cte_columns = if peek st = L.Lparen then ident_list_in_parens st else [] in
        expect_kw st "as";
        ignore (accept_kw st "not");
        ignore (accept_kw st "materialized");
        expect st L.Lparen "(";
        let cte_query = parse_query st in
        expect st L.Rparen ")";
        let acc = { cte_name; cte_columns; cte_query } :: acc in
        if accept st L.Comma then go acc else List.rev acc
      in
      go [])
    else []
  in
  let body = parse_setop st in
  let order_by = if is_kw st "order" && is_kw_at st 1 "by" then (advance st; advance st; parse_order_list st) else [] in
  skip_limit st;
  { ctes; body; order_by }

and skip_limit st =
  let rec go () =
    if accept_kw st "limit" || accept_kw st "offset" then (
      if not (is_kw st "all") then ignore (parse_expr st) else advance st;
      ignore (accept st L.Comma && (ignore (parse_expr st); true));
      ignore (accept_kw st "rows" || accept_kw st "row");
      go ())
    else if accept_kw st "fetch" then (
      while not (accept_kw st "only" || accept_kw st "ties" || peek st = L.Eof || peek st = L.Semicolon || peek st = L.Rparen) do
        advance st
      done;
      go ())
    else if is_kw st "for" && (is_kw_at st 1 "update" || is_kw_at st 1 "share") then (
      while not (peek st = L.Eof || peek st = L.Semicolon || peek st = L.Rparen) do advance st done)
  in
  go ()

and parse_setop st =
  let left = parse_set_primary st in
  let rec loop left =
    match peek st with
    | L.Ident (("union" | "intersect" | "except" | "minus") as op) ->
        advance st;
        ignore (accept_kw st "all" || accept_kw st "distinct");
        if accept_kw st "by" then expect_kw st "name";
        let right = parse_set_primary st in
        loop (Setop (op, left, right))
    | _ -> left
  in
  loop left

and parse_set_primary st =
  if peek st = L.Lparen then (
    advance st;
    let q = parse_query st in
    expect st L.Rparen ")";
    Nested q)
  else if is_kw st "values" then (
    advance st;
    let row () =
      expect st L.Lparen "(";
      let r = parse_expr_list st in
      expect st L.Rparen ")";
      r
    in
    let rec rows acc = let acc = row () :: acc in if accept st L.Comma then rows acc else List.rev acc in
    Values (rows []))
  else if is_macro st then (
    advance st;
    Select opaque_select)
  else Select (parse_select st)

and parse_select st : select =
  expect_kw st "select";
  if accept_kw st "distinct" then (
    if accept_kw st "on" then skip_parens st)
  else ignore (accept_kw st "all");
  if accept_kw st "top" then (if peek st = L.Lparen then skip_parens st else advance st; ignore (accept_kw st "percent"));
  let items = parse_select_items st in
  if accept_kw st "into" then ignore (dotted_name st);
  let from = if accept_kw st "from" then parse_from_list st else [] in
  let macro_where = is_macro st in
  skip_macros st;
  let where =
    if accept_kw st "where" then Some (parse_expr st)
    else if macro_where && (accept_kw st "and" || accept_kw st "or") then Some (parse_expr st) (* macro emitted WHERE ... *)
    else None
  in
  skip_macros st;
  let group_by =
    if is_kw st "group" && is_kw_at st 1 "by" then (
      advance st;
      advance st;
      ignore (accept_kw st "distinct");
      if is_kw st "all" then (advance st; [])
      else if is_kw st "grouping" && is_kw_at st 1 "sets" then (
        advance st;
        advance st;
        expect st L.Lparen "(";
        let es = parse_expr_list st in
        expect st L.Rparen ")";
        es)
      else (
        let rec items () =
          let e =
            match (peek st, peek_at st 1) with
            | L.Number n, (L.Comma | L.Eof | L.Semicolon | L.Rparen | L.Ident _) when int_of_string_opt n <> None ->
                advance st;
                Ordinal (int_of_string n)
            | _ -> parse_expr st
          in
          if accept st L.Comma then e :: items () else [ e ]
        in
        let es = items () in
        if accept_kw st "with" then ignore (ident_opt st);
        es))
    else []
  in
  skip_macros st;
  let having = if accept_kw st "having" then Some (parse_expr st) else None in
  skip_macros st;
  if accept_kw st "window" then (
    let rec go () =
      ignore (ident st "a window name");
      expect_kw st "as";
      skip_parens st;
      if accept st L.Comma then go ()
    in
    go ());
  let qualify = if accept_kw st "qualify" then Some (parse_expr st) else None in
  skip_macros st;
  { items; from; where; group_by; having; qualify }

and parse_select_items st =
  let item () =
    (* *, t.*, schema.t.* with optional EXCLUDE/EXCEPT (...) and REPLACE (...) *)
    let star_qual =
      if is_op st "*" then (advance st; Some [])
      else
        let rec lookahead k =
          match (peek_at st k, peek_at st (k + 1)) with
          | (L.Ident _ | L.Quoted _), L.Dot -> (
              match peek_at st (k + 2) with L.Op "*" -> Some (k + 3) | L.Ident _ | L.Quoted _ -> lookahead (k + 2) | _ -> None)
          | _ -> None
        in
        match lookahead 0 with
        | Some _ ->
            let name = dotted_name st in
            expect st L.Dot ".";
            advance st;
            Some name
        | None -> None
    in
    (* A macro standing alone in the select list expands to columns; one
       followed by AS or an operator is part of an expression. *)
    let macro_columns =
      is_macro st && (match peek_at st 1 with L.Ident "as" | L.Op _ -> false | _ -> true)
    in
    if macro_columns then (
      advance st;
      All { qual = []; except = [] })
    else
    match star_qual with
    | Some qual ->
        let except =
          if (is_kw st "exclude" || is_kw st "except") then (
            advance st;
            if peek st = L.Lparen then ident_list_in_parens st else [ ident st "a column name" ])
          else []
        in
        if (is_kw st "replace" || is_kw st "rename") && peek_at st 1 = L.Lparen then (advance st; skip_parens st);
        All { qual; except }
    | None ->
        let e = parse_expr st in
        Item (e, alias_opt st)
  in
  let can_start_item () =
    match peek st with
    | L.Ident w -> not (is_reserved w) || w = Jinja.placeholder
    | L.Quoted _ | L.Number _ | L.String _ | L.Lparen -> true
    | L.Op "*" -> true
    | _ -> false
  in
  let rec go acc =
    let last_macro = is_macro st in
    let acc = item () :: acc in
    (* macros often emit their own commas, so one may be missing next to them *)
    if accept st L.Comma && not (is_kw st "from") then go acc
    else if is_macro st || (last_macro && can_start_item ()) then go acc
    else List.rev acc
  in
  go []

and parse_from_list st =
  let first = parse_join_chain st in
  if accept st L.Comma then first :: parse_from_list st else [ first ]

and parse_join_chain st =
  let left = parse_table_primary st in
  let rec loop left =
    let save = st.pos in
    let natural = accept_kw st "natural" in
    let kind_words = [ "inner"; "left"; "right"; "full"; "outer"; "cross"; "semi"; "anti"; "asof"; "positional"; "any" ] in
    let cross = ref false in
    while (match peek st with L.Ident w -> List.mem w kind_words | _ -> false) do
      if is_kw st "cross" then cross := true;
      advance st
    done;
    if accept_kw st "join" then (
      let right = parse_table_primary st in
      let cond =
        if accept_kw st "on" then On (parse_expr st)
        else if accept_kw st "using" then Using (ident_list_in_parens st)
        else if natural then Natural
        else Cross
      in
      ignore !cross;
      loop (Join { left; right; cond }))
    else if is_kw st "lateral" && is_kw_at st 1 "view" then (
      (* Spark: LATERAL VIEW [OUTER] explode(x) t AS a, b *)
      advance st;
      advance st;
      ignore (accept_kw st "outer");
      let fname = ident st "a function" in
      expect st L.Lparen "(";
      let args = if peek st = L.Rparen then [] else parse_call_args st in
      expect st L.Rparen ")";
      let alias = match peek st with L.Ident w when w <> "as" -> advance st; Some w | _ -> None in
      let col_aliases =
        if accept_kw st "as" then (
          let rec go acc = let a = ident st "a column alias" in if accept st L.Comma then go (a :: acc) else List.rev (a :: acc) in
          go [])
        else []
      in
      loop (Join { left; right = Table_func { fname; args; alias; col_aliases }; cond = Cross }))
    else (
      st.pos <- save;
      left)
  in
  loop left

and parse_table_primary st : from_item =
  ignore (accept_kw st "lateral");
  if peek st = L.Lparen then (
    advance st;
    if starts_query st || (peek st = L.Lparen && (is_kw_at st 1 "select" || is_kw_at st 1 "with")) then (
      let query = parse_query st in
      expect st L.Rparen ")";
      let alias = alias_opt st in
      let col_aliases = if alias <> None && peek st = L.Lparen then ident_list_in_parens st else [] in
      Derived { query; alias; col_aliases })
    else
      let inner = parse_join_chain st in
      expect st L.Rparen ")";
      inner)
  else if is_macro st then (
    advance st;
    let alias = alias_opt st in
    Table_func { fname = Jinja.placeholder; args = []; alias; col_aliases = [] })
  else
    let name = dotted_name st in
    if peek st = L.Lparen then (
      (* table function: UNNEST(...), FLATTEN(input => ...), generate_series(...) *)
      advance st;
      let args = if peek st = L.Rparen then [] else parse_call_args st in
      expect st L.Rparen ")";
      let with_offset () =
        if is_kw st "with" && is_kw_at st 1 "offset" then (
          advance st;
          advance st;
          ignore (alias_opt st))
      in
      with_offset ();
      let alias = alias_opt st in
      let col_aliases = if alias <> None && peek st = L.Lparen then ident_list_in_parens st else [] in
      with_offset ();
      Table_func { fname = String.lowercase_ascii (dotted name); args; alias; col_aliases })
    else (
      (* time travel / sampling clauses carry no lineage *)
      if (is_kw st "at" || is_kw st "before") && peek_at st 1 = L.Lparen then (advance st; skip_parens st);
      if is_kw st "for" && is_kw_at st 1 "system_time" then (
        advance st; advance st; ignore (accept_kw st "as"); ignore (accept_kw st "of"); ignore (parse_additive st));
      let alias = alias_opt st in
      let col_aliases = if alias <> None && peek st = L.Lparen then ident_list_in_parens st else [] in
      if accept_kw st "tablesample" || accept_kw st "sample" then (
        ignore (ident_opt st);
        if peek st = L.Lparen then skip_parens st);
      Table { tname = name; alias; col_aliases })

(* ------------------------------------------------------------ statements *)

let skip_statement st =
  while not (peek st = L.Semicolon || peek st = L.Eof) do
    advance st
  done

let parse_create st =
  expect_kw st "create";
  if accept_kw st "or" then expect_kw st "replace";
  let modifiers =
    [ "temporary"; "temp"; "transient"; "secure"; "materialized"; "local"; "global"; "external"; "recursive"; "volatile";
      "unlogged"; "dynamic"; "iceberg"; "hybrid" ]
  in
  while (match peek st with L.Ident w -> List.mem w modifiers | _ -> false) do advance st done;
  if not (accept_kw st "table" || accept_kw st "view") then (skip_statement st; Other)
  else (
    if accept_kw st "if" then (expect_kw st "not"; expect_kw st "exists");
    let target = dotted_name st in
    let columns = if peek st = L.Lparen then ident_list_in_parens st else [] in
    let columns =
      List.filter (fun c -> not (List.mem (String.lowercase_ascii c) [ "primary"; "foreign"; "constraint"; "unique"; "check"; "key"; "index" ])) columns
    in
    (* options between the name and AS: COMMENT, CLUSTER BY, WITH (...), PARTITION BY ... *)
    let rec to_as () =
      match peek st with
      | L.Ident "as" -> advance st; true
      | L.Semicolon | L.Eof -> false
      | L.Lparen -> skip_parens st; to_as ()
      | _ -> advance st; to_as ()
    in
    if to_as () then (
      let query =
        if peek st = L.Lparen && not (starts_query st) then (
          advance st;
          let q = parse_query st in
          expect st L.Rparen ")";
          q)
        else parse_query st
      in
      Create_as { target; columns; query })
    else Create_table { target; columns })

let parse_insert st =
  expect_kw st "insert";
  ignore (accept_kw st "into" || accept_kw st "overwrite");
  ignore (accept_kw st "table");
  let target = dotted_name st in
  let columns =
    if peek st = L.Lparen && not (is_kw_at st 1 "select" || is_kw_at st 1 "with") then ident_list_in_parens st else []
  in
  if is_kw st "partition" then (advance st; skip_parens st);
  if starts_query st || peek st = L.Lparen then Insert { target; columns; query = parse_query st }
  else (skip_statement st; Other)

let parse_statement st =
  match peek st with
  | L.Ident ("select" | "with" | "values") | L.Lparen -> Query (parse_query st)
  | L.Ident w when w = Jinja.placeholder -> Query (parse_query st)
  | L.Ident "create" -> parse_create st
  | L.Ident "insert" -> parse_insert st
  | _ -> skip_statement st; Other

type parsed = { statements : (statement, string * int * int) result list }

(* Parse a whole script. Each statement parses independently, so one bad
   statement does not hide the others. *)
let parse_script (src : string) : parsed =
  let toks = L.tokenize src in
  let st = { toks; pos = 0 } in
  let out = ref [] in
  while peek st <> L.Eof do
    if accept st L.Semicolon then ()
    else
      let start = st.pos in
      (try
         let s = parse_statement st in
         if not (peek st = L.Semicolon || peek st = L.Eof) then fail st "unexpected token after statement";
         out := Ok s :: !out
       with Error (msg, line, col) ->
         out := Error (msg, line, col) :: !out;
         if st.pos = start then advance st;
         skip_statement st)
  done;
  { statements = List.rev !out }
