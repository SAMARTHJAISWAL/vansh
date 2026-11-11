(* Syntax tree for the subset of SQL that matters for lineage: everything
   that can move a value from one column to another. Clauses that cannot
   (LIMIT, ORDER BY at the top level, hints) are parsed and dropped. *)

type name = string list
(** Dotted name, e.g. ["analytics"; "orders"]. Parts keep their spelling. *)

type expr =
  | Column of name  (** [col], [t.col], [schema.t.col] *)
  | Star of name  (** [*] or [t.*] used as a value, e.g. [count( * )] *)
  | Literal
  | Ordinal of int  (** GROUP BY 2: the second select item *)
  | Func of { fname : string; args : expr list; agg_filter : expr option; over : window option }
  | Op of string * expr list  (** operators, CASE, BETWEEN, IN lists, CAST... *)
  | Case of { operand : expr option; branches : (expr * expr) list; else_ : expr option }
  | Subquery of query  (** scalar, IN or EXISTS subquery *)

and window = { partition : expr list; order : expr list }

and select_item =
  | Item of expr * string option  (** expression and optional alias *)
  | All of { qual : name; except : string list }
      (** [*] or [t.*], with any [EXCLUDE]/[EXCEPT] column list *)

and from_item =
  | Table of { tname : name; alias : string option; col_aliases : string list }
  | Derived of { query : query; alias : string option; col_aliases : string list }
  | Table_func of { fname : string; args : expr list; alias : string option; col_aliases : string list }
      (** UNNEST(...), FLATTEN(...), LATERAL generators *)
  | Join of { left : from_item; right : from_item; cond : join_cond }

and join_cond = On of expr | Using of string list | Natural | Cross

and select = {
  items : select_item list;
  from : from_item list;
  where : expr option;
  group_by : expr list;
  having : expr option;
  qualify : expr option;
}

and body =
  | Select of select
  | Values of expr list list
  | Setop of string * body * body
  | Nested of query  (** parenthesised query inside a set operation *)

and query = { ctes : cte list; body : body; order_by : expr list }

and cte = { cte_name : string; cte_columns : string list; cte_query : query }

type statement =
  | Query of query  (** bare SELECT: the output is the model itself *)
  | Create_as of { target : name; columns : string list; query : query }
  | Insert of { target : name; columns : string list; query : query }
  | Create_table of { target : name; columns : string list }  (** DDL that only declares a schema *)
  | Other  (** anything vansh does not trace (GRANT, SET, USE ...) *)

let last (n : name) = match List.rev n with x :: _ -> x | [] -> ""
let dotted (n : name) = String.concat "." n
