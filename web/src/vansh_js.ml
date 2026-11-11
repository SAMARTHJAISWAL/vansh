(* Browser entry point: exposes [vansh.analyze(names, sources)], returning the
   same JSON as `vansh --format json`. Compiled with js_of_ocaml; the page
   runs the real analyzer, not a port. *)

open Js_of_ocaml
open Vansh

let analyze (names : Js.js_string Js.t Js.js_array Js.t) (sources : Js.js_string Js.t Js.js_array Js.t) =
  let names = Array.map Js.to_string (Js.to_array names) in
  let sources = Array.map Js.to_string (Js.to_array sources) in
  let files =
    Array.to_list
      (Array.mapi
         (fun i name ->
           { Project.path = name; stem = Filename.remove_extension (Filename.basename name); source = sources.(i) })
         names)
  in
  let json =
    try Json.to_string ~indent:false (Render.project_json (Project.analyze files))
    with e -> Json.to_string (Json.Obj [ ("fatal", Json.String (Printexc.to_string e)) ])
  in
  Js.string json

let mermaid (names : Js.js_string Js.t Js.js_array Js.t) (sources : Js.js_string Js.t Js.js_array Js.t) =
  let names = Array.map Js.to_string (Js.to_array names) in
  let sources = Array.map Js.to_string (Js.to_array sources) in
  let files =
    Array.to_list
      (Array.mapi (fun i name -> { Project.path = name; stem = Filename.remove_extension (Filename.basename name); source = sources.(i) }) names)
  in
  Js.string (Render.mermaid (Project.analyze files))

let () =
  Js.export "vansh"
    (Js.Unsafe.obj
       [| ("analyze", Js.Unsafe.inject (Js.wrap_callback analyze));
          ("mermaid", Js.Unsafe.inject (Js.wrap_callback mermaid));
          ("version", Js.Unsafe.inject (Js.string "0.1.0")) |])
