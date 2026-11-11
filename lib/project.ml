(* A project: many SQL files (or one script) analysed together.

   Each bare SELECT file is a model named after the file (the dbt
   convention); CREATE ... AS and INSERT INTO statements define models named
   after their target; CREATE TABLE (without AS) declares a source schema.
   Models are resolved in dependency order, so a downstream model sees the
   exact column list of everything it reads, including SELECT * expansions. *)

open Ast
module Lin = Lineage

type file = { path : string; stem : string; source : string }

type model = {
  name : string;
  file : string;
  columns : Lin.column list;
  filters : Lin.deps;
  reads : string list;
  warnings : string list;
}

type error = { efile : string; line : int; col : int; message : string }

type t = {
  models : model list;  (** dependency order *)
  sources : (string * string list) list;  (** relations that are not models, with the columns read from them *)
  declared : (string * string list) list;  (** schemas from CREATE TABLE / --schema *)
  errors : error list;
}

let lc = String.lowercase_ascii

(* Every table name a query reads, minus names it defines as CTEs. *)
let tables_read (q : query) : name list =
  let out = ref [] and ctes = ref [] in
  let rec query q =
    List.iter (fun c -> ctes := lc c.cte_name :: !ctes; query c.cte_query) q.ctes;
    body q.body;
    List.iter expr q.order_by
  and body = function
    | Select s ->
        List.iter from s.from;
        List.iter (function Item (e, _) -> expr e | All _ -> ()) s.items;
        List.iter (Option.iter expr) [ s.where; s.having; s.qualify ];
        List.iter expr s.group_by
    | Values rows -> List.iter (List.iter expr) rows
    | Setop (_, a, b) -> body a; body b
    | Nested q -> query q
  and from = function
    | Table { tname; _ } -> out := tname :: !out
    | Derived { query = q; _ } -> query q
    | Table_func { args; _ } -> List.iter expr args
    | Join { left; right; cond } -> (
        from left;
        from right;
        match cond with On e -> expr e | _ -> ())
  and expr = function
    | Subquery q -> query q
    | Func { args; agg_filter; over; _ } ->
        List.iter expr args;
        Option.iter expr agg_filter;
        Option.iter (fun (w : window) -> List.iter expr (w.partition @ w.order)) over
    | Op (_, es) -> List.iter expr es
    | Case { operand; branches; else_ } ->
        Option.iter expr operand;
        List.iter (fun (a, b) -> expr a; expr b) branches;
        Option.iter expr else_
    | Column _ | Star _ | Literal | Ordinal _ -> ()
  in
  query q;
  List.filter (fun n -> match n with [ t ] -> not (List.mem (lc t) !ctes) | _ -> true) (List.rev !out)

type pending = { pname : string; pfile : string; defs : (query * string list) list }

let analyze (files : file list) : t =
  let errors = ref [] in
  let declared : (string, string list) Hashtbl.t = Hashtbl.create 16 in
  let declared_order = ref [] in
  let pendings : (string, pending) Hashtbl.t = Hashtbl.create 16 in
  let order = ref [] in
  let add_def name file q cols =
    match Hashtbl.find_opt pendings name with
    | Some p -> Hashtbl.replace pendings name { p with defs = p.defs @ [ (q, cols) ] }
    | None ->
        order := name :: !order;
        Hashtbl.replace pendings name { pname = name; pfile = file; defs = [ (q, cols) ] }
  in
  List.iter
    (fun f ->
      let src = if Jinja.has_jinja f.source then Jinja.preprocess f.source else f.source in
      match Parser.parse_script src with
      | exception Lexer.Error (message, line, col) -> errors := { efile = f.path; line; col; message } :: !errors
      | parsed ->
          let bare = ref 0 in
          List.iter
            (function
              | Error (message, line, col) -> errors := { efile = f.path; line; col; message } :: !errors
              | Ok (Query q) ->
                  incr bare;
                  let name = if !bare = 1 then lc f.stem else Printf.sprintf "%s_%d" (lc f.stem) !bare in
                  add_def name f.path q []
              | Ok (Create_as { target; columns; query }) -> add_def (Lin.canonical target) f.path query columns
              | Ok (Insert { target; columns; query }) ->
                  let name = Lin.canonical target in
                  let cols = if columns <> [] then columns else Option.value (Hashtbl.find_opt declared name) ~default:[] in
                  add_def name f.path query cols
              | Ok (Create_table { target; columns }) ->
                  let name = Lin.canonical target in
                  if not (Hashtbl.mem declared name) then declared_order := name :: !declared_order;
                  Hashtbl.replace declared name columns
              | Ok Other -> ())
            parsed.statements)
    files;
  let model_names = List.rev !order in
  (* Map a referenced table name to a model name: exact, then by last part. *)
  let last s = Ast.last (String.split_on_char '.' s) in
  let match_name (candidates : string list) (key : string) : string option =
    if List.mem key candidates then Some key
    else
      match List.filter (fun m -> last m = last key) candidates with
      | [ m ] -> Some m
      | _ -> None
  in
  let to_model key = match_name model_names key in
  (* Topological order; a cycle is reported and broken. *)
  let state : (string, [ `Visiting | `Done ]) Hashtbl.t = Hashtbl.create 16 in
  let sorted = ref [] in
  let rec visit path name =
    match Hashtbl.find_opt state name with
    | Some `Done -> ()
    | Some `Visiting ->
        let p = Hashtbl.find pendings name in
        errors :=
          { efile = p.pfile; line = 0; col = 0;
            message = Printf.sprintf "dependency cycle: %s" (String.concat " -> " (List.rev (name :: path))) }
          :: !errors
    | None ->
        Hashtbl.replace state name `Visiting;
        let p = Hashtbl.find pendings name in
        List.iter
          (fun (q, _) ->
            List.iter
              (fun t -> match to_model (Lin.canonical t) with Some m when m <> name -> visit (name :: path) m | _ -> ())
              (tables_read q))
          p.defs;
        Hashtbl.replace state name `Done;
        sorted := name :: !sorted
  in
  List.iter (visit []) model_names;
  let sorted = List.rev !sorted in
  (* Resolve in order. *)
  let resolved : (string, Lin.column list) Hashtbl.t = Hashtbl.create 16 in
  let declared_names = Hashtbl.fold (fun k _ acc -> k :: acc) declared [] in
  let catalog =
    { Lin.find =
        (fun key ->
          match to_model key with
          | Some m -> (
              match Hashtbl.find_opt resolved m with
              | Some cols ->
                  let names = List.filter_map (fun (c : Lin.column) -> if c.name = "*" then None else Some c.name) cols in
                  Some (m, names, List.exists (fun (c : Lin.column) -> c.name = "*") cols)
              | None -> Some (m, [], true) (* in a cycle: treat as open *))
          | None -> (
              match match_name declared_names key with
              | Some d when Hashtbl.find declared d <> [] -> Some (d, Hashtbl.find declared d, false)
              | _ -> None)) }
  in
  let models =
    List.map
      (fun name ->
        let p = Hashtbl.find pendings name in
        let results = List.map (fun (q, cols) -> Lin.analyze ~catalog ~target_columns:cols q) p.defs in
        (* several INSERTs into one target: merge column by column *)
        let columns =
          match results with
          | [] -> []
          | first :: rest ->
              List.fold_left
                (fun (acc : Lin.column list) (r : Lin.result) ->
                  List.map
                    (fun (c : Lin.column) ->
                      match List.find_opt (fun (c2 : Lin.column) -> Lin.same c2.name c.name) r.columns with
                      | Some c2 -> { c with deps = Lin.union c.deps c2.deps }
                      | None -> c)
                    acc)
                first.columns rest
        in
        Hashtbl.replace resolved name columns;
        { name; file = p.pfile; columns;
          filters = Lin.unions (List.map (fun (r : Lin.result) -> r.filters) results);
          reads = List.sort_uniq compare (List.concat_map (fun (r : Lin.result) -> r.reads) results);
          warnings = List.sort_uniq compare (List.concat_map (fun (r : Lin.result) -> r.warnings) results) })
      sorted
  in
  (* Everything read that is not a model is a source. *)
  let source_cols : (string, string list) Hashtbl.t = Hashtbl.create 16 in
  let source_order = ref [] in
  let note (s : Lin.src) =
    if not (List.mem s.rel model_names) then (
      let cur = match Hashtbl.find_opt source_cols s.rel with Some c -> c | None -> source_order := s.rel :: !source_order; [] in
      if not (List.exists (Lin.same s.col) cur) then Hashtbl.replace source_cols s.rel (cur @ [ s.col ]))
  in
  List.iter
    (fun m ->
      List.iter (fun (c : Lin.column) -> Lin.Deps.iter (fun s _ -> note s) c.deps) m.columns;
      Lin.Deps.iter (fun s _ -> note s) m.filters;
      List.iter (fun r -> if not (List.mem r model_names || Hashtbl.mem source_cols r) then (source_order := r :: !source_order; Hashtbl.replace source_cols r [])) m.reads)
    models;
  (* Declared columns appear even when unused, so `unused` can report them. *)
  List.iter
    (fun d ->
      if Hashtbl.mem source_cols d then
        let cur = Hashtbl.find source_cols d in
        let extra = List.filter (fun c -> not (List.exists (Lin.same c) cur)) (Hashtbl.find declared d) in
        Hashtbl.replace source_cols d (cur @ extra))
    (List.rev !declared_order);
  let sources = List.rev_map (fun s -> (s, Hashtbl.find source_cols s)) !source_order |> List.sort compare in
  { models;
    sources;
    declared = List.rev_map (fun d -> (d, Hashtbl.find declared d)) !declared_order;
    errors = List.rev !errors }

(* ------------------------------------------------------------ the graph *)

type node = { table : string; column : string }
type edge = { src : node; dst : node; kind : Lin.kind }

let edges (p : t) : edge list =
  List.concat_map
    (fun m ->
      List.concat_map
        (fun (c : Lin.column) ->
          List.map
            (fun ((s : Lin.src), kind) -> { src = { table = s.rel; column = s.col }; dst = { table = m.name; column = c.name }; kind })
            (Lin.deps_list c.deps))
        m.columns)
    p.models

let find_model p name = List.find_opt (fun m -> m.name = name) p.models

(* Does relation [t] have [c] as a named column? If not, a "*" passthrough
   carries it. *)
let has_named_column p t c =
  match find_model p t with
  | Some m -> List.exists (fun (col : Lin.column) -> Lin.same col.name c) m.columns
  | None -> true

let same_node a b = a.table = b.table && Lin.same a.column b.column

(* Edges into a node, following "*" passthroughs with the column renamed. *)
let edges_into p (all : edge list) (n : node) : edge list =
  let direct = List.filter (fun e -> same_node e.dst n) all in
  if direct <> [] || has_named_column p n.table n.column then direct
  else
    List.filter_map
      (fun e ->
        if e.dst.table = n.table && e.dst.column = "*" then
          Some { e with dst = n; src = { e.src with column = (if e.src.column = "*" then n.column else e.src.column) } }
        else None)
      all

let edges_out_of p (all : edge list) (n : node) : edge list =
  let direct = List.filter (fun e -> same_node e.src n) all in
  let via_star =
    List.filter_map
      (fun e ->
        if e.src.table = n.table && e.src.column = "*" && n.column <> "*" && not (has_named_column p e.dst.table n.column)
        then Some { e with src = n; dst = { e.dst with column = n.column } }
        else None)
      all
  in
  direct @ via_star

type tree = Tree of node * Lin.kind option * tree list

(* Recursive walk; a node already on the current path is not expanded again. *)
let walk (next : node -> edge list) (pick : edge -> node) (root : node) : tree =
  let rec go path n kind =
    if List.exists (same_node n) path then Tree (n, kind, [])
    else
      let kids = List.map (fun e -> go (n :: path) (pick e) (Some e.kind)) (next n) in
      Tree (n, kind, kids)
  in
  go [] root None

let trace p n = let all = edges p in walk (edges_into p all) (fun e -> e.src) n
let impact p n = let all = edges p in walk (edges_out_of p all) (fun e -> e.dst) n

(* Models whose row filters (WHERE/JOIN/GROUP BY...) read [n]. *)
let filtered_by p (n : node) : string list =
  List.filter_map
    (fun m -> if Lin.Deps.exists (fun s _ -> s.rel = n.table && (Lin.same s.col n.column || s.col = "*")) m.filters then Some m.name else None)
    p.models

let consumers p table = List.filter (fun m -> List.mem table m.reads) p.models

(* Columns that something reads (models) or declares (sources) but that no
   downstream model uses, as a value or as a filter. Final models, with no
   consumers, are outputs and are never reported. *)
let unused (p : t) : node list =
  let all = edges p in
  let used n =
    edges_out_of p all n <> []
    || List.exists (fun m -> Lin.Deps.exists (fun s _ -> s.rel = n.table && Lin.same s.col n.column) m.filters) p.models
  in
  let of_models =
    List.concat_map
      (fun m ->
        if consumers p m.name = [] then []
        else
          List.filter_map
            (fun (c : Lin.column) ->
              let n = { table = m.name; column = c.name } in
              if c.name = "*" || used n then None else Some n)
            m.columns)
      p.models
  in
  let of_declared =
    List.concat_map
      (fun (d, cols) ->
        if consumers p d = [] then []
        else List.filter_map (fun c -> let n = { table = d; column = c } in if used n then None else Some n) cols)
      p.declared
  in
  of_models @ of_declared

let parse_node (s : string) : node option =
  match String.rindex_opt s '.' with
  | Some i when i > 0 && i < String.length s - 1 ->
      Some { table = lc (String.sub s 0 i); column = String.sub s (i + 1) (String.length s - i - 1) }
  | _ -> None

(* Resolve a user-typed node: "orders.amount" may name model "orders" or
   source "raw.orders" by its last part. *)
let find_node (p : t) (n : node) : node option =
  let tables = List.map (fun m -> m.name) p.models @ List.map fst p.sources in
  let table =
    if List.mem n.table tables then Some n.table
    else
      match List.filter (fun t -> Ast.last (String.split_on_char '.' t) = n.table) tables with
      | [ t ] -> Some t
      | _ -> None
  in
  Option.map (fun table -> { n with table }) table

let warnings p = List.concat_map (fun m -> List.map (fun w -> (m.name, w)) m.warnings) p.models
