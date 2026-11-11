(* Project -> text, JSON, Mermaid and Graphviz. *)

module P = Project
module Lin = Lineage

type style = { color : bool }

let paint st code s = if st.color then Printf.sprintf "\027[%sm%s\027[0m" code s else s
let bold st = paint st "1"
let dim st = paint st "2"
let kind_color = function Lin.Direct -> "32" | Lin.Transform -> "36" | Lin.Aggregate -> "35"

let src_name (s : Lin.src) = s.rel ^ "." ^ s.col

(* Display width in code points, for alignment. *)
let width s =
  let n = ref 0 in
  String.iter (fun c -> if Char.code c land 0xC0 <> 0x80 then incr n) s;
  !n

let pad s w = s ^ String.make (max 0 (w - width s)) ' '

(* ------------------------------------------------------------ text *)

let deps_text st (d : Lin.deps) =
  match Lin.deps_list d with
  | [] -> dim st "(constant)"
  | xs ->
      String.concat ", "
        (List.map
           (fun ((s : Lin.src), k) ->
             let label = src_name s in
             match k with
             | Lin.Direct -> paint st (kind_color k) label
             | _ -> paint st (kind_color k) label ^ dim st (" " ^ Lin.kind_name k))
           xs)

let lineage_text st (p : P.t) : string =
  let b = Buffer.create 4096 in
  List.iter
    (fun (m : P.model) ->
      Buffer.add_string b (Printf.sprintf "%s  %s\n" (bold st m.name) (dim st m.file));
      let w = List.fold_left (fun acc (c : Lin.column) -> max acc (width c.name)) 0 m.columns in
      List.iter
        (fun (c : Lin.column) ->
          Buffer.add_string b (Printf.sprintf "  %s  %s %s\n" (pad c.name w) (dim st "<-") (deps_text st c.deps)))
        m.columns;
      if not (Lin.Deps.is_empty m.filters) then
        Buffer.add_string b
          (Printf.sprintf "  %s %s\n" (dim st "rows filtered or grouped by:")
             (String.concat ", " (List.map (fun ((s : Lin.src), _) -> src_name s) (Lin.deps_list m.filters))));
      List.iter (fun w -> Buffer.add_string b (Printf.sprintf "  %s %s\n" (paint st "33" "warning:") w)) m.warnings;
      Buffer.add_char b '\n')
    p.models;
  if p.sources <> [] then (
    Buffer.add_string b (bold st "sources" ^ "\n");
    List.iter
      (fun (s, cols) -> Buffer.add_string b (Printf.sprintf "  %s  %s\n" s (dim st (String.concat ", " cols))))
      p.sources);
  List.iter
    (fun (e : P.error) ->
      Buffer.add_string b
        (Printf.sprintf "%s %s:%d:%d %s\n" (paint st "31" "error:") e.efile e.line e.col e.message))
    p.errors;
  Buffer.contents b

let tree_text st (title : string) (t : P.tree) : string =
  let b = Buffer.create 1024 in
  Buffer.add_string b (title ^ "\n");
  let rec go prefix is_last (P.Tree (n, kind, kids)) depth =
    let label = n.table ^ "." ^ n.column in
    if depth = 0 then Buffer.add_string b (bold st label ^ "\n")
    else (
      let k = match kind with Some k -> dim st (" (" ^ Lin.kind_name k ^ ")") | None -> "" in
      Buffer.add_string b (prefix ^ (if is_last then "└─ " else "├─ ") ^ paint st (match kind with Some k -> kind_color k | None -> "0") label ^ k ^ "\n"));
    let prefix' = if depth = 0 then "" else prefix ^ if is_last then "   " else "│  " in
    let n = List.length kids in
    List.iteri (fun i kid -> go prefix' (i = n - 1) kid (depth + 1)) kids
  in
  go "" true t 0;
  Buffer.contents b

(* ------------------------------------------------------------ JSON *)

let deps_json (d : Lin.deps) =
  Json.List
    (List.map
       (fun ((s : Lin.src), k) ->
         Json.Obj [ ("table", Json.String s.rel); ("column", Json.String s.col); ("kind", Json.String (Lin.kind_name k)) ])
       (Lin.deps_list d))

let node_json (n : P.node) = Json.Obj [ ("table", Json.String n.table); ("column", Json.String n.column) ]

let project_json (p : P.t) : Json.t =
  Json.Obj
    [ ( "models",
        Json.List
          (List.map
             (fun (m : P.model) ->
               Json.Obj
                 [ ("name", Json.String m.name);
                   ("file", Json.String m.file);
                   ( "columns",
                     Json.List
                       (List.map (fun (c : Lin.column) -> Json.Obj [ ("name", Json.String c.name); ("sources", deps_json c.deps) ]) m.columns) );
                   ( "row_filters",
                     Json.List (List.map (fun ((s : Lin.src), _) -> node_json { table = s.rel; column = s.col }) (Lin.deps_list m.filters)) );
                   ("reads", Json.List (List.map (fun r -> Json.String r) m.reads));
                   ("warnings", Json.List (List.map (fun w -> Json.String w) m.warnings)) ])
             p.models) );
      ( "sources",
        Json.List
          (List.map
             (fun (s, cols) -> Json.Obj [ ("name", Json.String s); ("columns", Json.List (List.map (fun c -> Json.String c) cols)) ])
             p.sources) );
      ( "edges",
        Json.List
          (List.map
             (fun (e : P.edge) ->
               Json.Obj [ ("from", node_json e.src); ("to", node_json e.dst); ("kind", Json.String (Lin.kind_name e.kind)) ])
             (P.edges p)) );
      ("unused", Json.List (List.map node_json (P.unused p)));
      ( "errors",
        Json.List
          (List.map
             (fun (e : P.error) ->
               Json.Obj
                 [ ("file", Json.String e.efile); ("line", Json.Int e.line); ("column", Json.Int e.col); ("message", Json.String e.message) ])
             p.errors) ) ]

let rec tree_json (P.Tree (n, kind, kids)) : Json.t =
  Json.Obj
    [ ("table", Json.String n.table);
      ("column", Json.String n.column);
      ("kind", match kind with Some k -> Json.String (Lin.kind_name k) | None -> Json.Null);
      ("children", Json.List (List.map tree_json kids)) ]

(* ------------------------------------------------------------ diagrams *)

let ident_safe s = String.map (fun c -> if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') then c else '_') s

let tables_of (p : P.t) =
  List.map (fun (s, cols) -> (s, cols, `Source)) p.sources
  @ List.map (fun (m : P.model) -> (m.name, List.map (fun (c : Lin.column) -> c.name) m.columns, `Model)) p.models

let mermaid (p : P.t) : string =
  let b = Buffer.create 4096 in
  Buffer.add_string b "flowchart LR\n";
  List.iter
    (fun (t, cols, kind) ->
      Buffer.add_string b (Printf.sprintf "  subgraph %s[\"%s\"]\n" (ident_safe ("t_" ^ t)) t);
      List.iter (fun c -> Buffer.add_string b (Printf.sprintf "    %s[\"%s\"]\n" (ident_safe (t ^ "__" ^ c)) c)) cols;
      Buffer.add_string b "  end\n";
      if kind = `Source then Buffer.add_string b (Printf.sprintf "  class %s source\n" (ident_safe ("t_" ^ t))))
    (tables_of p);
  List.iter
    (fun (e : P.edge) ->
      let arrow = match e.kind with Lin.Direct -> "-->" | Lin.Transform -> "-.->" | Lin.Aggregate -> "==>" in
      Buffer.add_string b
        (Printf.sprintf "  %s %s %s\n" (ident_safe (e.src.table ^ "__" ^ e.src.column)) arrow
           (ident_safe (e.dst.table ^ "__" ^ e.dst.column))))
    (P.edges p);
  Buffer.add_string b "  classDef source fill:#fff7e6,stroke:#d98a00\n";
  Buffer.contents b

let dot (p : P.t) : string =
  let b = Buffer.create 4096 in
  Buffer.add_string b "digraph lineage {\n  rankdir=LR;\n  node [shape=plaintext, fontname=\"Helvetica\"];\n";
  let esc s = String.concat "&amp;" (String.split_on_char '&' s) |> String.split_on_char '<' |> String.concat "&lt;" in
  List.iter
    (fun (t, cols, kind) ->
      let header = if kind = `Source then "#fdebd0" else "#dbeafe" in
      Buffer.add_string b
        (Printf.sprintf "  %s [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td bgcolor=\"%s\"><b>%s</b></td></tr>"
           (ident_safe ("t_" ^ t)) header (esc t));
      List.iter (fun c -> Buffer.add_string b (Printf.sprintf "<tr><td port=\"%s\">%s</td></tr>" (ident_safe c) (esc c))) cols;
      Buffer.add_string b "</table>>];\n")
    (tables_of p);
  List.iter
    (fun (e : P.edge) ->
      let style = match e.kind with Lin.Direct -> "" | Lin.Transform -> " [style=dashed]" | Lin.Aggregate -> " [style=bold]" in
      Buffer.add_string b
        (Printf.sprintf "  %s:%s -> %s:%s%s;\n" (ident_safe ("t_" ^ e.src.table)) (ident_safe e.src.column)
           (ident_safe ("t_" ^ e.dst.table)) (ident_safe e.dst.column) style))
    (P.edges p);
  Buffer.add_string b "}\n";
  Buffer.contents b
