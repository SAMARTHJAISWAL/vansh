(* dbt/Jinja preprocessing.

   vansh does not run Jinja. It rewrites the handful of constructs that
   decide lineage and neutralises the rest:

     {{ ref('stg_orders') }}            -> stg_orders
     {{ ref('pkg', 'stg_orders') }}     -> stg_orders
     {{ source('raw', 'orders') }}      -> raw.orders
     {{ config(...) }}, {# comments #}  -> removed
     {% if %}...{% else %}...{% endif %} -> the first branch is kept
                                          (is_incremental() counts as false)
     {{ anything_else(...) }}           -> NULL (a literal; no lineage)

   Character offsets change but line numbers are preserved, so parse errors
   still point at the right line of the original file. *)

(* Extract the quoted string arguments of a call such as ref('a', 'b'). *)
let string_args (s : string) : string list =
  let out = ref [] in
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    let c = s.[!i] in
    if c = '\'' || c = '"' then (
      let j = ref (!i + 1) in
      while !j < n && s.[!j] <> c do
        incr j
      done;
      out := String.sub s (!i + 1) (!j - !i - 1) :: !out;
      i := !j + 1)
    else incr i
  done;
  List.rev !out

(* As many newlines as [s] contains, so line numbers survive a rewrite. *)
let keep_newlines (s : string) : string =
  String.make (String.fold_left (fun k c -> if c = '\n' then k + 1 else k) 0 s) '\n'

let starts_with ~prefix s = String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix

(* Placeholder for a macro whose output vansh cannot know. The parser
   treats it as unknown columns in a select list, an opaque relation where a
   query or table is expected, and a constant inside an expression. *)
let placeholder = "__vansh_macro__"

(* Top-level arguments of a call, split on commas outside brackets and quotes. *)
let call_args (e : string) : string list =
  match String.index_opt e '(' with
  | None -> []
  | Some open_ ->
      let close = match String.rindex_opt e ')' with Some c when c > open_ -> c | _ -> String.length e in
      let body = String.sub e (open_ + 1) (close - open_ - 1) in
      let args = ref [] and cur = Buffer.create 16 and depth = ref 0 and quote = ref None in
      String.iter
        (fun c ->
          match !quote with
          | Some q ->
              Buffer.add_char cur c;
              if c = q then quote := None
          | None -> (
              match c with
              | '\'' | '"' -> quote := Some c; Buffer.add_char cur c
              | '(' | '[' | '{' -> incr depth; Buffer.add_char cur c
              | ')' | ']' | '}' -> decr depth; Buffer.add_char cur c
              | ',' when !depth = 0 -> args := Buffer.contents cur :: !args; Buffer.clear cur
              | c -> Buffer.add_char cur c))
        body;
      if String.trim (Buffer.contents cur) <> "" then args := Buffer.contents cur :: !args;
      List.rev_map String.trim !args

let type_name (macro : string) =
  match macro with
  | "type_string" -> "varchar"
  | "type_timestamp" -> "timestamp"
  | "type_int" | "type_bigint" -> "bigint"
  | "type_float" | "type_numeric" -> "double"
  | "type_boolean" -> "boolean"
  | _ -> "varchar"

(* Split on a top-level separator, ignoring quoted text and brackets. *)
let split_top (sep : char) (s : string) : string list =
  let parts = ref [] and cur = Buffer.create 16 and depth = ref 0 and quote = ref None in
  String.iter
    (fun c ->
      match !quote with
      | Some q -> Buffer.add_char cur c; if c = q then quote := None
      | None ->
          if c = '\'' || c = '"' then (quote := Some c; Buffer.add_char cur c)
          else if c = '(' || c = '[' then (incr depth; Buffer.add_char cur c)
          else if c = ')' || c = ']' then (decr depth; Buffer.add_char cur c)
          else if c = sep && !depth = 0 then (parts := Buffer.contents cur :: !parts; Buffer.clear cur)
          else Buffer.add_char cur c)
    s;
  List.rev (Buffer.contents cur :: !parts) |> List.map String.trim

(* Evaluate a Jinja string expression: 'a' ~ dbt.type_x() ~ 'b'. *)
let eval_concat (a : string) : string option =
  let parts = split_top '~' a in
  let piece p =
    let n = String.length p in
    if n >= 2 && (p.[0] = '\'' || p.[0] = '"') && p.[n - 1] = p.[0] then Some (String.sub p 1 (n - 2))
    else
      let fn = match String.index_opt p '(' with Some i -> String.sub p 0 i | None -> p in
      let short = Ast.last (String.split_on_char '.' fn) in
      if String.length short > 5 && String.sub short 0 5 = "type_" then Some (type_name short) else None
  in
  let pieces = List.map piece parts in
  if List.for_all Option.is_some pieces then Some (String.concat "" (List.map Option.get pieces)) else None

(* 'created_at' -> created_at; ['a', 'b'] -> [a; b]; keyword=value -> value. *)
let rec sql_of_arg (a : string) : string list =
  let a = String.trim a in
  let a = match String.index_opt a '=' with
    | Some i when i > 0 && String.for_all (fun c -> c = '_' || (c >= 'a' && c <= 'z')) (String.sub a 0 i) -> String.trim (String.sub a (i + 1) (String.length a - i - 1))
    | _ -> a in
  let n = String.length a in
  if n >= 2 && a.[0] = '[' && a.[n - 1] = ']' then List.concat_map sql_of_arg (call_args ("x(" ^ String.sub a 1 (n - 2) ^ ")"))
  else match eval_concat a with Some sql when sql <> "" -> [ sql ] | _ -> []

let macro_call (cols : string list) =
  if cols = [] then placeholder else Printf.sprintf "vansh_macro(%s)" (String.concat ", " cols)

let not_columns =
  [ "day"; "week"; "month"; "year"; "quarter"; "hour"; "minute"; "second"; "millisecond"; "microsecond"; "date";
    "dayofweek"; "isoweek"; "utc"; "string"; "int"; "integer"; "float"; "numeric"; "timestamp"; "boolean"; "bigint";
    "varchar"; "true"; "false"; "null"; "asc"; "desc" ]

let looks_like_column (s : string) =
  String.length s > 0
  && (let c = s.[0] in (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_')
  && String.for_all (fun c -> Lexer.is_ident_char c || c = '.') s
  && not (List.mem (String.lowercase_ascii s) not_columns)

(* Position of a top-level keyword such as " if " outside quotes. *)
let find_top (kw : string) (s : string) : int option =
  let n = String.length s and m = String.length kw in
  let quote = ref None and depth = ref 0 and found = ref None and i = ref 0 in
  while !found = None && !i < n do
    let c = s.[!i] in
    (match !quote with
    | Some q -> if c = q then quote := None
    | None ->
        if c = '\'' || c = '"' then quote := Some c
        else if c = '(' || c = '[' then incr depth
        else if c = ')' || c = ']' then decr depth
        else if !depth = 0 && !i + m <= n && String.sub s !i m = kw then found := Some !i);
    incr i
  done;
  !found

let rec rewrite_expression ?(in_expr = false) (inner : string) : string =
  let e = String.trim inner in
  match find_top " if " e with
  | Some i -> (
      (* {{ "col" if cond else 'null' }}: the first branch, as with {% if %} *)
      let value = String.sub e 0 i in
      match eval_concat value with Some sql -> sql | None -> rewrite_expression ~in_expr value)
  | None ->
  let fn =
    let b = Buffer.create 8 in
    (try String.iter (fun c -> if c = '(' || c = ' ' then raise Exit else Buffer.add_char b c) e with Exit -> ());
    Buffer.contents b
  in
  let short = Ast.last (String.split_on_char '.' fn) in
  let args = call_args e in
  let nth i = match List.nth_opt args i with Some a -> sql_of_arg a | None -> [] in
  match short with
  | "ref" -> (
      match List.rev (string_args e) with
      | model :: _ -> model
      | [] -> placeholder)
  | "source" -> (
      match string_args e with
      | [ src; table ] -> src ^ "." ^ table
      | [ table ] -> table
      | _ -> placeholder)
  | "config" | "set" -> ""
  | "this" -> "this"
  | "star" -> "*"
  | "group_by" -> (
      match nth 0 with
      | _ -> (
          let n = match args with [ a ] -> (match String.split_on_char '=' a with [ _; v ] | [ v ] -> int_of_string_opt (String.trim v) | _ -> None) | _ -> None in
          match n with
          | Some n when n > 0 -> "group by " ^ String.concat ", " (List.init n (fun i -> string_of_int (i + 1)))
          | _ -> placeholder))
  | t when String.length t > 5 && String.sub t 0 5 = "type_" -> type_name t
  (* Common dbt / dbt_utils macros whose column arguments decide lineage. *)
  | "generate_surrogate_key" | "surrogate_key" | "concat" | "safe_add" | "safe_subtract" | "safe_divide" | "greatest"
  | "least" | "hash" ->
      macro_call (List.concat_map sql_of_arg args)
  | "date_trunc" | "last_day" -> macro_call (nth 1)
  | "dateadd" -> macro_call (nth 2)
  | "datediff" -> macro_call (nth 0 @ nth 1)
  | "safe_cast" | "cast_bool_to_text" | "split_part" | "length" | "position" | "right" | "replace"
  | "escape_single_quotes" | "json_extract_scalar" | "to_timestamp" | "fivetran_convert_timezone" | "convert_timezone" ->
      macro_call (nth 0)
  | "var" | "env_var" | "target" | "run_started_at" | "invocation_id" -> placeholder
  | _ when in_expr ->
      (* An unknown macro used as a value: column names passed as strings are
         almost always its inputs, e.g. {{ cents_to_dollars('amount') }}.
         Keyword arguments that configure the macro are not. *)
      let config_key a =
        match String.index_opt a '=' with
        | Some i ->
            let key = String.lowercase_ascii (String.trim (String.sub a 0 i)) in
            List.exists
              (fun w ->
                let n = String.length key and m = String.length w in
                let rec go k = k + m <= n && (String.sub key k m = w || go (k + 1)) in
                go 0)
              [ "package"; "tz"; "type"; "name"; "format"; "part"; "var"; "schema"; "database"; "alias"; "default" ]
        | None -> false
      in
      macro_call (List.filter looks_like_column (List.concat_map sql_of_arg (List.filter (fun a -> not (config_key a)) args)))
  | _ -> placeholder

(* {% if %} blocks: vansh keeps exactly one branch, the first, except that
   is_incremental() is taken as false (a full refresh), which is the shape of
   the model most people mean when they ask about lineage. *)
type frame = { mutable taking : bool; mutable taken : bool; parent : bool }

let first_word s =
  let s = String.trim s in
  match String.index_opt s ' ' with Some i -> (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1)) | None -> (s, "")

let condition_holds (cond : string) =
  let c = String.trim cond in
  let contains sub =
    let n = String.length c and m = String.length sub in
    let rec go i = i + m <= n && (String.sub c i m = sub || go (i + 1)) in
    go 0
  in
  if contains "is_incremental" then String.length c >= 4 && String.sub c 0 4 = "not " else true

let preprocess (src : string) : string =
  let n = String.length src in
  let b = Buffer.create n in
  let stack : frame list ref = ref [] in
  let active () = match !stack with [] -> true | f :: _ -> f.parent && f.taking in
  let i = ref 0 in
  let find_close close from =
    let rec go j = if j + 1 >= n then None else if src.[j] = close.[0] && src.[j + 1] = close.[1] then Some j else go (j + 1) in
    go from
  in
  while !i < n do
    if !i + 1 < n && src.[!i] = '{' && (src.[!i + 1] = '{' || src.[!i + 1] = '%' || src.[!i + 1] = '#') then (
      let close = match src.[!i + 1] with '{' -> "}}" | '%' -> "%}" | _ -> "#}" in
      match find_close close (!i + 2) with
      | None ->
          Buffer.add_char b src.[!i];
          incr i
      | Some j ->
          let inner = String.sub src (!i + 2) (j - !i - 2) in
          let inner = if starts_with ~prefix:"-" inner then String.sub inner 1 (String.length inner - 1) else inner in
          let inner =
            if String.length inner > 0 && inner.[String.length inner - 1] = '-' then String.sub inner 0 (String.length inner - 1)
            else inner
          in
          (match src.[!i + 1] with
          | '{' ->
              if active () then (
                (* is the macro used as a value? look at what follows it *)
                let in_expr =
                  let k = ref (j + 2) in
                  while !k < n && (src.[!k] = ' ' || src.[!k] = '\t' || src.[!k] = '\n' || src.[!k] = '\r') do incr k done;
                  !k < n
                  && (List.mem src.[!k] [ ')'; '='; '<'; '>'; '+'; '-'; '*'; '/'; '|' ]
                     || (!k + 2 < n && (src.[!k] = 'a' || src.[!k] = 'A') && (src.[!k + 1] = 's' || src.[!k + 1] = 'S')
                         && not (Lexer.is_ident_char src.[!k + 2])))
                in
                let r = rewrite_expression ~in_expr inner in
                (* glued to an identifier (col{{ suffix }}): drop the unknown part *)
                let glued =
                  Buffer.length b > 0
                  && (let c = Buffer.nth b (Buffer.length b - 1) in Lexer.is_ident_char c)
                in
                (* ... or glued in front of one ({{ prefix }}_total): a readable stand-in *)
                let glued_after = j + 2 < n && Lexer.is_ident_char src.[j + 2] in
                if glued && r = placeholder then ()
                else if glued_after && r = placeholder then Buffer.add_string b (if glued then "" else " jinja")
                else Buffer.add_string b (if glued then r else " " ^ r ^ if glued_after then "" else " "))
          | '%' -> (
              let word, rest = first_word inner in
              match word with
              | "if" ->
                  let parent = active () in
                  let holds = condition_holds rest in
                  stack := { taking = holds; taken = holds; parent } :: !stack
              | "elif" -> (
                  match !stack with
                  | f :: _ ->
                      let holds = (not f.taken) && condition_holds rest in
                      f.taking <- holds;
                      if holds then f.taken <- true
                  | [] -> ())
              | "else" -> (
                  match !stack with
                  | f :: _ ->
                      f.taking <- not f.taken;
                      f.taken <- true
                  | [] -> ())
              | "endif" | "endset" | "endmacro" | "endcall" | "endmaterialization" -> (
                  match !stack with _ :: rest -> stack := rest | [] -> ())
              | "set" when not (String.contains rest '=') ->
                  (* {% set x %}...{% endset %}: compile-time SQL, not the model *)
                  stack := { taking = false; taken = true; parent = active () } :: !stack
              | "macro" | "call" | "materialization" ->
                  stack := { taking = false; taken = true; parent = active () } :: !stack
              | _ -> ())
          | _ -> ());
          Buffer.add_string b (keep_newlines inner);
          i := j + 2)
    else (
      let c = src.[!i] in
      if active () || c = '\n' then Buffer.add_char b c;
      incr i)
  done;
  Buffer.contents b

(* Whether a file looks like a dbt model at all. *)
let has_jinja (src : string) =
  let n = String.length src in
  let rec go i = i + 1 < n && ((src.[i] = '{' && (src.[i + 1] = '{' || src.[i + 1] = '%')) || go (i + 1)) in
  go 0
