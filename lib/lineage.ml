(* Column-level lineage for one statement.

   Every relation in scope (base table, CTE, subquery, upstream model, table
   function) is turned into a [relation]: an ordered list of columns, each
   carrying the set of external columns it is computed from. Resolving a
   SELECT therefore composes lineage through CTEs and subqueries, and the
   result for each output column points straight at tables outside the
   statement: source tables or other models.

   Tables whose columns are unknown (no schema, not a model) are "open": any
   column name can be read from them, and SELECT * from them yields a single
   "*" passthrough column. *)

open Ast

type kind = Direct | Transform | Aggregate

let kind_rank = function Direct -> 0 | Transform -> 1 | Aggregate -> 2
let stronger a b = if kind_rank a >= kind_rank b then a else b
let kind_name = function Direct -> "direct" | Transform -> "transform" | Aggregate -> "aggregate"

type src = { rel : string; col : string }

module Src = struct
  type t = src

  let compare a b =
    match String.compare a.rel b.rel with 0 -> String.compare (String.lowercase_ascii a.col) (String.lowercase_ascii b.col) | c -> c
end

module Deps = Map.Make (Src)

type deps = kind Deps.t

let union (a : deps) (b : deps) : deps = Deps.union (fun _ x y -> Some (stronger x y)) a b
let unions = List.fold_left union Deps.empty
let at_least k (d : deps) : deps = Deps.map (stronger k) d

type column = { name : string; deps : deps }

type openness =
  | Closed
  | External of string  (** unknown table: column c depends on (name, c) *)
  | Any of deps  (** table function without a column list: every column depends on its arguments *)
  | Passthrough of deps
      (** a SELECT * over an open table: column c depends on these deps,
          with every "*" source column read as c *)

type relation = { cols : column list; openness : openness }

let lc = String.lowercase_ascii
let same a b = lc a = lc b
let canonical (n : name) = lc (dotted n)

(* What the caller knows about relations outside the statement. *)
type catalog = {
  find : string -> (string * string list * bool) option;
      (** canonical or dotted name -> (canonical name, known columns, has a "*" passthrough) *)
}

type entry = {
  alias : string;
  rel_name : string;  (** canonical table name, or the alias for derived tables *)
  relation : relation;
  is_value : bool;  (** UNNEST(x) AS v: the alias names a value, not a table *)
}

type scope = { entries : entry list; using : string list; aliases : column list; outer : scope option }

type ctx = {
  catalog : catalog;
  mutable warnings : string list;
  mutable filters : deps;  (** row-level dependencies: WHERE, JOIN, GROUP BY, HAVING, QUALIFY *)
  mutable referenced : string list;  (** external relations read *)
}

let warn ctx msg = if not (List.mem msg ctx.warnings) then ctx.warnings <- msg :: ctx.warnings

let aggregates =
  [ "sum"; "count"; "avg"; "min"; "max"; "count_if"; "countif"; "array_agg"; "string_agg"; "listagg"; "group_concat";
    "median"; "mode"; "stddev"; "stddev_pop"; "stddev_samp"; "variance"; "var_pop"; "var_samp"; "any_value";
    "approx_count_distinct"; "approx_distinct"; "approx_percentile"; "percentile_cont"; "percentile_disc"; "bool_and";
    "bool_or"; "every"; "bit_and"; "bit_or"; "corr"; "covar_pop"; "covar_samp"; "arbitrary"; "first"; "last";
    "first_value"; "last_value"; "nth_value"; "hll_count"; "object_agg"; "json_agg"; "jsonb_agg"; "collect_list";
    "collect_set"; "sum_if"; "sumif"; "avg_if"; "max_by"; "min_by"; "arg_max"; "arg_min"; "product"; "kurtosis";
    "skewness"; "entropy"; "regr_slope"; "regr_intercept"; "uniq"; "uniqexact"; "array_union_agg" ]

let is_aggregate fname = List.mem (Ast.last (String.split_on_char '.' fname)) aggregates

(* ------------------------------------------------------------ relations *)

let substitute_star (c : string) (d : deps) : deps =
  Deps.fold (fun s k acc -> Deps.add (if s.col = "*" then { s with col = c } else s) k acc) d Deps.empty

let find_col (r : relation) (c : string) : deps option =
  match List.find_opt (fun col -> same col.name c) r.cols with
  | Some col -> Some col.deps
  | None -> (
      match r.openness with
      | Closed -> None
      | External name -> Some (Deps.singleton { rel = name; col = c } Direct)
      | Any d -> Some (at_least Transform d)
      | Passthrough d -> Some (substitute_star c d))

let rename_cols (r : relation) (names : string list) : relation =
  if names = [] then r
  else
    let rec go cols names =
      match (cols, names) with
      | c :: cs, n :: ns -> { c with name = n } :: go cs ns
      | cs, [] -> cs
      | [], _ -> []
    in
    { r with cols = go r.cols names }

(* A table outside the statement: a model or declared table if the catalog
   knows it, otherwise an open external table. *)
let external_relation ctx (n : name) : string * relation =
  let key = canonical n in
  match ctx.catalog.find key with
  | Some (cname, cols, passthrough) ->
      if not (List.mem cname ctx.referenced) then ctx.referenced <- cname :: ctx.referenced;
      ( cname,
        { cols = List.map (fun c -> { name = c; deps = Deps.singleton { rel = cname; col = c } Direct }) cols;
          openness = (if passthrough then External cname else Closed) } )
  | None ->
      if not (List.mem key ctx.referenced) then ctx.referenced <- key :: ctx.referenced;
      (key, { cols = []; openness = External key })

(* ------------------------------------------------------------ columns *)

let rec lookup_column ctx (sc : scope) (n : name) : deps option =
  let entry_named q = List.find_opt (fun e -> same e.alias q && not e.is_value) sc.entries in
  let entry_by_table parts =
    (* schema.table.col: match the table part against aliases and table names *)
    let t = Ast.last parts in
    List.find_opt
      (fun e -> (same e.alias t || same (Ast.last (String.split_on_char '.' e.rel_name)) t) && not e.is_value)
      sc.entries
  in
  let here =
    match n with
    | [ c ] -> resolve_unqualified ctx sc c
    | [ q; c ] -> (
        match entry_named q with
        | Some e -> (
            match find_col e.relation c with
            | Some d -> Some d
            | None ->
                warn ctx (Printf.sprintf "%s has no column %s" e.alias c);
                Some Deps.empty)
        | None -> (
            (* not a table alias: maybe struct_column.field *)
            match resolve_unqualified ctx sc q with Some d -> Some (at_least Transform d) | None -> None))
    | parts -> (
        let rev = List.rev parts in
        let c = List.hd rev in
        let table_parts = List.rev (List.tl rev) in
        match entry_by_table table_parts with
        | Some e -> Some (Option.value (find_col e.relation c) ~default:Deps.empty)
        | None -> (
            match resolve_unqualified ctx sc (List.hd parts) with Some d -> Some (at_least Transform d) | None -> None))
  in
  match here with
  | Some d -> Some d
  | None -> ( match sc.outer with Some o -> lookup_column ctx o n | None -> None)

and resolve_unqualified ctx sc c : deps option =
  (* 1. a value alias from UNNEST(...) AS v *)
  match List.find_opt (fun e -> e.is_value && same e.alias c) sc.entries with
  | Some e -> Some (match e.relation.openness with Any d -> at_least Transform d | _ -> Deps.empty)
  | None -> (
      (* 2. relations that definitely have the column *)
      let known = List.filter (fun e -> List.exists (fun col -> same col.name c) e.relation.cols) sc.entries in
      match known with
      | [ e ] -> find_col e.relation c
      | e :: rest ->
          if not (List.exists (same c) sc.using) then
            warn ctx
              (Printf.sprintf "column %s is ambiguous: it exists in %s" c
                 (String.concat " and " (List.map (fun e -> e.alias) (e :: rest))));
          find_col e.relation c
      | [] -> (
          (* 3. an earlier select-list alias (lateral column alias) *)
          match List.find_opt (fun col -> same col.name c) sc.aliases with
          | Some col -> Some col.deps
          | None -> (
              (* 4. open relations that might have it *)
              let opens = List.filter (fun e -> e.relation.openness <> Closed && not e.is_value) sc.entries in
              match opens with
              | [] -> None
              | [ e ] -> find_col e.relation c
              | many ->
                  if not (List.exists (same c) sc.using) then
                  warn ctx
                    (Printf.sprintf
                       "column %s could come from %s; add a table qualifier (or a schema) to make lineage exact" c
                       (String.concat ", " (List.map (fun e -> e.alias) many)));
                  Some (unions (List.filter_map (fun e -> find_col e.relation c) many)))))

(* ------------------------------------------------------------ expressions *)

let rec expr_deps ctx sc (k : kind) (e : expr) : deps =
  let sub k' e = expr_deps ctx sc (stronger k k') e in
  match e with
  | Literal | Star _ | Ordinal _ -> Deps.empty
  | Column n when List.exists (fun p -> p = Jinja.placeholder) n -> Deps.empty
  | Column n -> (
      match lookup_column ctx sc n with
      | Some d -> at_least k d
      | None ->
          warn ctx (Printf.sprintf "unknown column %s" (dotted n));
          Deps.empty)
  | Op ("cast", [ x ]) -> expr_deps ctx sc k x (* a type change is still the same value *)
  | Op (_, args) -> unions (List.map (sub Transform) args)
  | Case { operand; branches; else_ } ->
      let parts = (match operand with Some o -> [ o ] | None -> []) @ List.concat_map (fun (c, v) -> [ c; v ]) branches
                  @ (match else_ with Some e -> [ e ] | None -> []) in
      unions (List.map (sub Transform) parts)
  | Func { fname; args; agg_filter; over } ->
      let k' = if is_aggregate fname then Aggregate else Transform in
      let window = match over with Some w -> w.partition @ w.order | None -> [] in
      let extra = (match agg_filter with Some f -> [ f ] | None -> []) @ window in
      union (unions (List.map (sub k') args)) (unions (List.map (sub Transform) extra))
  | Subquery q ->
      let r = resolve_query ctx (Some sc) [] q in
      at_least (stronger k Transform) (unions (List.map (fun c -> c.deps) r.cols))

and add_filter ctx sc (e : expr) = ctx.filters <- union ctx.filters (expr_deps ctx sc Transform e)

(* ------------------------------------------------------------ FROM *)

and from_entries ctx outer ctes (prior : entry list) (f : from_item) : entry list * string list =
  let lateral_scope = { entries = prior; using = []; aliases = []; outer } in
  match f with
  | Table { tname; alias; col_aliases } ->
      let cte = match tname with [ t ] -> List.find_opt (fun (n, _) -> same n t) ctes | _ -> None in
      let rel_name, relation =
        match cte with Some (n, r) -> (n, r) | None -> external_relation ctx tname
      in
      let alias = match alias with Some a -> a | None -> Ast.last tname in
      ([ { alias; rel_name; relation = rename_cols relation col_aliases; is_value = false } ], [])
  | Derived { query; alias; col_aliases } ->
      let r = resolve_query ctx (Some lateral_scope) ctes query in
      let alias = Option.value alias ~default:"_subquery" in
      ([ { alias; rel_name = alias; relation = rename_cols r col_aliases; is_value = false } ], [])
  | Table_func { fname; args; alias; col_aliases } ->
      let d = unions (List.map (expr_deps ctx lateral_scope Transform) args) in
      let alias = Option.value alias ~default:fname in
      let relation =
        if col_aliases <> [] then { cols = List.map (fun n -> { name = n; deps = at_least Transform d }) col_aliases; openness = Closed }
        else { cols = []; openness = Any d }
      in
      let is_value = col_aliases = [] && List.mem (lc fname) [ "unnest"; "explode"; "posexplode"; "explode_outer" ] in
      ([ { alias; rel_name = alias; relation; is_value } ], [])
  | Join { left; right; cond } ->
      let l, lu = from_entries ctx outer ctes prior left in
      let r, ru = from_entries ctx outer ctes (prior @ l) right in
      let entries = l @ r in
      let sc = { entries = prior @ entries; using = []; aliases = []; outer } in
      let using =
        match cond with
        | On e ->
            add_filter ctx sc e;
            []
        | Using cols ->
            List.iter (fun c -> add_filter ctx { sc with using = cols } (Column [ c ])) cols;
            cols
        | Natural | Cross -> []
      in
      (entries, lu @ ru @ using)

(* ------------------------------------------------------------ SELECT *)

and output_name (i : int) (e : expr) : string =
  match e with
  | Column n -> Ast.last n
  | Op ("cast", [ x ]) -> output_name i x
  | Func { fname; _ } -> Ast.last (String.split_on_char '.' fname)
  | _ -> Printf.sprintf "_col%d" (i + 1)

and resolve_select ctx outer ctes (s : select) : relation =
  let entries, using =
    List.fold_left
      (fun (acc, us) f ->
        let es, u = from_entries ctx outer ctes acc f in
        (acc @ es, us @ u))
      ([], []) s.from
  in
  let sc = { entries; using; aliases = []; outer } in
  let cols = ref [] in
  List.iteri
    (fun i item ->
      let sc = { sc with aliases = List.rev !cols } in
      match item with
      | Item (e, alias) ->
          let deps = expr_deps ctx sc Direct e in
          let name = match alias with Some a -> a | None -> output_name i e in
          cols := { name; deps } :: !cols
      | All { qual; except } ->
          let targets =
            match qual with
            | [] -> List.filter (fun e -> not e.is_value) entries
            | q -> (
                let t = Ast.last q in
                match List.filter (fun e -> same e.alias t) entries with
                | [] ->
                    warn ctx (Printf.sprintf "%s.* refers to an unknown table" (dotted q));
                    []
                | es -> es)
          in
          let seen_using = ref [] in
          List.iter
            (fun e ->
              List.iter
                (fun col ->
                  let dup_using = List.exists (same col.name) using && List.exists (same col.name) !seen_using in
                  if not (List.exists (same col.name) except || dup_using) then (
                    if List.exists (same col.name) using then seen_using := col.name :: !seen_using;
                    cols := { name = col.name; deps = col.deps } :: !cols))
                e.relation.cols;
              match e.relation.openness with
              | Closed -> ()
              | External rel ->
                  cols := { name = "*"; deps = Deps.singleton { rel; col = "*" } Direct } :: !cols
              | Any d ->
                  cols := { name = "*"; deps = at_least Transform d } :: !cols
              | Passthrough d ->
                  cols := { name = "*"; deps = d } :: !cols)
            targets)
    s.items;
  let cols = List.rev !cols in
  let sc = { sc with aliases = cols } in
  Option.iter (add_filter ctx sc) s.where;
  List.iter
    (fun g ->
      match g with
      | Ordinal i -> (
          (* GROUP BY 2 groups by the second select item *)
          match List.nth_opt cols (i - 1) with
          | Some c -> ctx.filters <- union ctx.filters (at_least Transform c.deps)
          | None -> warn ctx (Printf.sprintf "GROUP BY %d refers past the select list" i))
      | e -> add_filter ctx sc e)
    s.group_by;
  Option.iter (add_filter ctx sc) s.having;
  Option.iter (add_filter ctx sc) s.qualify;
  (* Merge duplicate "*" passthrough columns from several open tables. *)
  let stars, named = List.partition (fun c -> c.name = "*") cols in
  match stars with
  | [] -> { cols = named; openness = Closed }
  | s ->
      let d = unions (List.map (fun c -> c.deps) s) in
      { cols = named @ [ { name = "*"; deps = d } ]; openness = Passthrough d }

and resolve_body ctx outer ctes (b : body) : relation =
  match b with
  | Select s -> resolve_select ctx outer ctes s
  | Nested q -> resolve_query ctx outer ctes q
  | Values rows ->
      let sc = { entries = []; using = []; aliases = []; outer } in
      let width = List.fold_left (fun m r -> max m (List.length r)) 0 rows in
      let cols =
        List.init width (fun i ->
            let d = unions (List.filter_map (fun r -> Option.map (expr_deps ctx sc Direct) (List.nth_opt r i)) rows) in
            { name = Printf.sprintf "column%d" (i + 1); deps = d })
      in
      { cols; openness = Closed }
  | Setop (op, a, b) ->
      let ra = resolve_body ctx outer ctes a in
      let rb = resolve_body ctx outer ctes b in
      let na = List.length ra.cols and nb = List.length rb.cols in
      if na <> nb && not (List.exists (fun c -> c.name = "*") (ra.cols @ rb.cols)) then
        warn ctx (Printf.sprintf "%s branches have %d and %d columns" (String.uppercase_ascii op) na nb);
      (* EXCEPT/INTERSECT: the right side only filters rows *)
      let combine = op = "union" in
      if not combine then ctx.filters <- union ctx.filters (at_least Transform (unions (List.map (fun c -> c.deps) rb.cols)));
      let cols =
        List.mapi
          (fun i c ->
            match (combine, List.nth_opt rb.cols i) with
            | true, Some c2 -> { c with deps = union c.deps c2.deps }
            | _ -> c)
          ra.cols
      in
      { cols; openness = ra.openness }

and resolve_query ctx outer ctes (q : query) : relation =
  let ctes =
    List.fold_left
      (fun ctes (c : cte) ->
        (* Recursive CTEs: bind the anchor branch first, then resolve the whole. *)
        let self_ref =
          match c.cte_query.body with
          | Setop (_, anchor, _) ->
              let r = resolve_body ctx outer ctes anchor in
              [ (c.cte_name, rename_cols r c.cte_columns) ]
          | _ -> []
        in
        let r = resolve_query ctx outer (self_ref @ ctes) c.cte_query in
        (c.cte_name, rename_cols r c.cte_columns) :: ctes)
      ctes q.ctes
  in
  resolve_body ctx outer ctes q.body

(* ------------------------------------------------------------ entry point *)

type result = { columns : column list; filters : deps; warnings : string list; reads : string list }

let analyze ~(catalog : catalog) ?(target_columns = []) (q : query) : result =
  let ctx = { catalog; warnings = []; filters = Deps.empty; referenced = [] } in
  let r = resolve_query ctx None [] q in
  let r = rename_cols r target_columns in
  { columns = r.cols; filters = ctx.filters; warnings = List.rev ctx.warnings; reads = List.rev ctx.referenced }

let deps_list (d : deps) : (src * kind) list = Deps.bindings d
