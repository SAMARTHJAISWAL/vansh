open Vansh
module P = Project

let usage =
  {|vansh: column-level lineage for SQL and dbt projects.

Usage:
  vansh [lineage] [PATH...]          every output column and the columns it is computed from
  vansh trace TABLE.COLUMN [PATH...]  upstream: where does this column come from?
  vansh impact TABLE.COLUMN [PATH...] downstream: what breaks if this column changes?
  vansh unused [PATH...]             columns that are produced but never used downstream
  vansh graph [PATH...]              the whole graph as Mermaid (default), DOT or JSON
  vansh check [PATH...]              exit 1 on parse errors or lineage warnings (for CI)

PATH is a .sql file or a directory searched recursively (dbt's target/,
dbt_packages/ and dot-directories are skipped). With no PATH, or -, read SQL
from standard input.

Options:
  -f, --format FMT    text (default), json, mermaid, dot
  -s, --schema FILE   CREATE TABLE statements describing source tables
      --color WHEN    auto (default), always, never
  -h, --help          show this help
  -V, --version       show the version

Examples:
  vansh models/
  vansh impact stg_orders.amount models/
  vansh trace orders.lifetime_value models/ --schema warehouse.sql
  vansh graph -f mermaid models/ > lineage.mmd
  echo "select a + b as c from t" | vansh
|}

let version = "0.1.0"

let die code msg =
  prerr_endline ("vansh: " ^ msg);
  exit code

let skip_dirs = [ "target"; "dbt_packages"; "dbt_modules"; "node_modules"; "logs" ]

let rec collect path : string list =
  if Sys.is_directory path then
    Sys.readdir path |> Array.to_list |> List.sort compare
    |> List.concat_map (fun f ->
           if f.[0] = '.' || List.mem f skip_dirs then [] else collect (Filename.concat path f))
  else if Filename.check_suffix path ".sql" then [ path ]
  else []

let read_file path = In_channel.with_open_bin path In_channel.input_all

let file_of path : P.file =
  { path; stem = Filename.remove_extension (Filename.basename path); source = read_file path }

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  let format = ref "text" and schemas = ref [] and color = ref "auto" and positional = ref [] in
  let rec parse = function
    | [] -> ()
    | ("-h" | "--help") :: _ -> print_string usage; exit 0
    | ("-V" | "--version") :: _ -> print_endline ("vansh " ^ version); exit 0
    | ("-f" | "--format") :: v :: rest -> format := v; parse rest
    | ("-s" | "--schema") :: v :: rest -> schemas := v :: !schemas; parse rest
    | "--color" :: v :: rest -> color := v; parse rest
    | ("-f" | "--format" | "-s" | "--schema" | "--color") :: [] -> die 2 "option needs a value"
    | x :: _ when String.length x > 1 && x.[0] = '-' && x <> "-" -> die 2 ("unknown option " ^ x ^ " (try --help)")
    | x :: rest -> positional := x :: !positional; parse rest
  in
  parse args;
  let positional = List.rev !positional in
  let command, rest =
    match positional with
    | ("lineage" | "unused" | "graph" | "check") as c :: rest -> (c, rest)
    | ("trace" | "impact") as c :: target :: rest -> (c ^ ":" ^ target, rest)
    | ("trace" | "impact") :: [] -> die 2 "trace and impact need a TABLE.COLUMN"
    | rest -> ("lineage", rest)
  in
  if not (List.mem !format [ "text"; "json"; "mermaid"; "dot" ]) then die 2 "--format must be text, json, mermaid or dot";
  let files =
    match rest with
    | [] | [ "-" ] -> [ { P.path = "<stdin>"; stem = "query"; source = In_channel.input_all stdin } ]
    | paths ->
        List.concat_map
          (fun p ->
            if not (Sys.file_exists p) then die 1 ("no such file or directory: " ^ p);
            match collect p with [] -> die 1 ("no .sql files in " ^ p) | fs -> List.map file_of fs)
          paths
  in
  let schema_files = List.map (fun p -> if Sys.file_exists p then file_of p else die 1 ("no such schema file: " ^ p)) (List.rev !schemas) in
  let project = P.analyze (schema_files @ files) in
  let st =
    { Render.color =
        (match !color with
        | "always" -> true
        | "never" -> false
        | _ -> Sys.getenv_opt "NO_COLOR" = None && Out_channel.isatty stdout) }
  in
  let print_json j = print_endline (Json.to_string j) in
  let node_arg s =
    match P.parse_node s with
    | None -> die 2 ("expected TABLE.COLUMN, got " ^ s)
    | Some n -> (
        match P.find_node project n with
        | Some n -> n
        | None -> die 1 (Printf.sprintf "no table named %s in this project" n.table))
  in
  let has_errors = project.errors <> [] in
  (match String.split_on_char ':' command with
  | [ "lineage" ] -> (
      match !format with
      | "json" -> print_json (Render.project_json project)
      | "mermaid" -> print_string (Render.mermaid project)
      | "dot" -> print_string (Render.dot project)
      | _ -> print_string (Render.lineage_text st project))
  | [ "graph" ] -> (
      match !format with
      | "json" -> print_json (Render.project_json project)
      | "dot" -> print_string (Render.dot project)
      | _ -> print_string (Render.mermaid project))
  | [ ("trace" | "impact") as which; target ] ->
      let n = node_arg target in
      let tree = if which = "trace" then P.trace project n else P.impact project n in
      if !format = "json" then
        print_json
          (Json.Obj
             [ ("tree", Render.tree_json tree);
               ("filtered_in", Json.List (List.map (fun m -> Json.String m) (if which = "impact" then P.filtered_by project n else []))) ])
      else (
        let title =
          if which = "trace" then "upstream lineage (what this column is computed from)"
          else "downstream impact (what is computed from this column)"
        in
        print_string (Render.tree_text st title tree);
        if which = "impact" then
          match P.filtered_by project n with
          | [] -> ()
          | ms -> Printf.printf "\nalso filters or groups rows in: %s\n" (String.concat ", " ms))
  | [ "unused" ] ->
      let u = P.unused project in
      if !format = "json" then print_json (Json.List (List.map Render.node_json u))
      else if u = [] then print_endline "no unused columns: everything produced upstream is used downstream"
      else (
        Printf.printf "%d column%s produced but never used downstream:\n" (List.length u) (if List.length u = 1 then "" else "s");
        List.iter (fun (n : P.node) -> Printf.printf "  %s.%s\n" n.table n.column) u)
  | [ "check" ] ->
      let ws = P.warnings project in
      List.iter (fun (e : P.error) -> Printf.printf "error: %s:%d:%d %s\n" e.efile e.line e.col e.message) project.errors;
      List.iter (fun (m, w) -> Printf.printf "warning: %s: %s\n" m w) ws;
      Printf.printf "%d model%s, %d error%s, %d warning%s\n" (List.length project.models)
        (if List.length project.models = 1 then "" else "s")
        (List.length project.errors) (if List.length project.errors = 1 then "" else "s")
        (List.length ws) (if List.length ws = 1 then "" else "s");
      if has_errors || ws <> [] then exit 1
  | _ -> die 2 "unknown command");
  if has_errors then exit 1
