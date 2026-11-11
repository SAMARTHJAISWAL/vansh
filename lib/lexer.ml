(* SQL lexer.

   Produces a flat array of tokens with positions. It is deliberately
   dialect-tolerant: double-quoted, backticked and bracketed identifiers,
   single-quoted strings with '' escapes, dollar-quoted strings, line and
   block comments, and the operators used by Postgres, Snowflake, BigQuery,
   DuckDB and Spark SQL. *)

type kind =
  | Ident of string  (** unquoted, lower-cased *)
  | Quoted of string  (** quoted identifier, case kept *)
  | String of string
  | Number of string
  | Op of string  (** operators and punctuation other than those below *)
  | Lparen
  | Rparen
  | Lbracket
  | Rbracket
  | Comma
  | Dot
  | Semicolon
  | Eof

type token = { kind : kind; line : int; col : int }

exception Error of string * int * int

let is_ident_start c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || Char.code c >= 128
let is_ident_char c = is_ident_start c || (c >= '0' && c <= '9') || c = '$'
let is_digit c = c >= '0' && c <= '9'

(* Longest first, so ">=" wins over ">". *)
let operators =
  [ "::"; "||"; "<="; ">="; "<>"; "!="; "=>"; "->>"; "->"; "=="; "!~*"; "~*"; "!~"; "+"; "-"; "*"; "/"; "%"; "="; "<"; ">";
    "~"; "^"; "&"; "|"; ":"; "?"; "@"; "#"; "!" ]

let tokenize (src : string) : token array =
  let n = String.length src in
  let toks = ref [] in
  let line = ref 1 and line_start = ref 0 in
  let i = ref 0 in
  let push kind start = toks := { kind; line = !line; col = start - !line_start + 1 } :: !toks in
  let newline_at j =
    incr line;
    line_start := j + 1
  in
  let err msg = raise (Error (msg, !line, !i - !line_start + 1)) in
  let peek k = if !i + k < n then src.[!i + k] else '\000' in
  (* Read a delimited token, honouring a doubled closing delimiter as an escape. *)
  let delimited close =
    let start = !i in
    incr i;
    let buf = Buffer.create 16 in
    let rec go () =
      if !i >= n then err "unterminated string or identifier"
      else
        let c = src.[!i] in
        if c = close then
          if peek 1 = close then (
            Buffer.add_char buf c;
            i := !i + 2;
            go ())
          else incr i
        else (
          if c = '\n' then newline_at !i;
          if c = '\\' && close = '\'' && !i + 1 < n then (
            Buffer.add_char buf src.[!i + 1];
            i := !i + 2)
          else (
            Buffer.add_char buf c;
            incr i);
          go ())
    in
    go ();
    (Buffer.contents buf, start)
  in
  while !i < n do
    let c = src.[!i] in
    let start = !i in
    if c = '\n' then (
      newline_at !i;
      incr i)
    else if c = ' ' || c = '\t' || c = '\r' then incr i
    else if c = '-' && peek 1 = '-' then (
      while !i < n && src.[!i] <> '\n' do
        incr i
      done)
    else if c = '/' && peek 1 = '*' then (
      i := !i + 2;
      while !i < n && not (src.[!i] = '*' && peek 1 = '/') do
        if src.[!i] = '\n' then newline_at !i;
        incr i
      done;
      if !i >= n then err "unterminated block comment";
      i := !i + 2)
    else if c = '\'' then
      let s, st = delimited '\'' in
      push (String s) st
    else if c = '"' then
      let s, st = delimited '"' in
      push (Quoted s) st
    else if c = '`' then
      let s, st = delimited '`' in
      push (Quoted s) st
    else if c = '$' && peek 1 = '$' then (
      (* $$ dollar-quoted $$ *)
      i := !i + 2;
      let b = !i in
      while !i < n && not (src.[!i] = '$' && peek 1 = '$') do
        if src.[!i] = '\n' then newline_at !i;
        incr i
      done;
      if !i >= n then err "unterminated $$ string";
      push (String (String.sub src b (!i - b))) start;
      i := !i + 2)
    else if is_digit c || (c = '.' && is_digit (peek 1)) then (
      while !i < n && (is_digit src.[!i] || src.[!i] = '.' || src.[!i] = '_') do
        incr i
      done;
      if !i < n && (src.[!i] = 'e' || src.[!i] = 'E') then (
        incr i;
        if !i < n && (src.[!i] = '+' || src.[!i] = '-') then incr i;
        while !i < n && is_digit src.[!i] do
          incr i
        done);
      push (Number (String.sub src start (!i - start))) start)
    else if is_ident_start c then (
      while !i < n && is_ident_char src.[!i] do
        incr i
      done;
      push (Ident (String.lowercase_ascii (String.sub src start (!i - start)))) start)
    else
      match c with
      | '(' -> push Lparen start; incr i
      | ')' -> push Rparen start; incr i
      | '[' ->
          (* [bracketed identifier] (SQL Server) vs array subscript: an
             identifier is a run of name characters closed by ']'. *)
          let j = ref (!i + 1) in
          while !j < n && src.[!j] <> ']' && src.[!j] <> '\n' && src.[!j] <> '[' do
            incr j
          done;
          let inner = if !j < n && src.[!j] = ']' then String.sub src (!i + 1) (!j - !i - 1) else "" in
          let looks_like_name =
            inner <> "" && is_ident_start inner.[0] && String.for_all (fun c -> is_ident_char c || c = ' ') inner
          in
          if looks_like_name then (
            push (Quoted inner) start;
            i := !j + 1)
          else (
            push Lbracket start;
            incr i)
      | ']' -> push Rbracket start; incr i
      | ',' -> push Comma start; incr i
      | '.' -> push Dot start; incr i
      | ';' -> push Semicolon start; incr i
      | _ -> (
          let rest = n - !i in
          match
            List.find_opt
              (fun op -> String.length op <= rest && String.sub src !i (String.length op) = op)
              operators
          with
          | Some op ->
              push (Op op) start;
              i := !i + String.length op
          | None -> err (Printf.sprintf "unexpected character %C" c))
  done;
  push Eof !i;
  Array.of_list (List.rev !toks)

let describe = function
  | Ident s -> s
  | Quoted s -> "\"" ^ s ^ "\""
  | String s -> "'" ^ s ^ "'"
  | Number s -> s
  | Op s -> s
  | Lparen -> "("
  | Rparen -> ")"
  | Lbracket -> "["
  | Rbracket -> "]"
  | Comma -> ","
  | Dot -> "."
  | Semicolon -> ";"
  | Eof -> "end of input"
