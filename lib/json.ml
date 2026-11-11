(* A minimal JSON value and printer, so the library has no dependencies. *)

type t = Null | Bool of bool | Int of int | String of string | List of t list | Obj of (string * t) list

let escape (b : Buffer.t) (s : string) =
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | c when Char.code c < 0x20 -> Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"'

let to_string ?(indent = true) (v : t) : string =
  let b = Buffer.create 4096 in
  let nl depth = if indent then (Buffer.add_char b '\n'; Buffer.add_string b (String.make (2 * depth) ' ')) in
  let rec go depth = function
    | Null -> Buffer.add_string b "null"
    | Bool x -> Buffer.add_string b (string_of_bool x)
    | Int i -> Buffer.add_string b (string_of_int i)
    | String s -> escape b s
    | List [] -> Buffer.add_string b "[]"
    | Obj [] -> Buffer.add_string b "{}"
    | List xs ->
        Buffer.add_char b '[';
        List.iteri (fun i x -> if i > 0 then Buffer.add_char b ','; nl (depth + 1); go (depth + 1) x) xs;
        nl depth;
        Buffer.add_char b ']'
    | Obj kvs ->
        Buffer.add_char b '{';
        List.iteri
          (fun i (k, v) ->
            if i > 0 then Buffer.add_char b ',';
            nl (depth + 1);
            escape b k;
            Buffer.add_string b (if indent then ": " else ":");
            go (depth + 1) v)
          kvs;
        nl depth;
        Buffer.add_char b '}'
  in
  go 0 v;
  Buffer.contents b
