(* Unit tests. No framework: each case prints on failure and the run exits
   non-zero if any failed. *)

open Vansh
module Lin = Lineage
module P = Project

let failures = ref 0
let passed = ref 0

let check name cond =
  if cond then incr passed
  else (
    incr failures;
    Printf.printf "FAIL: %s\n%!" name)

let project_of ?(files = []) sql = P.analyze (files @ [ { P.path = "q.sql"; stem = "q"; source = sql } ])

let model p name = match P.find_model p name with Some m -> m | None -> failwith ("no model " ^ name)

(* "col" -> sorted list of "table.column:kind" *)
let lineage_of p mname col =
  let m = model p mname in
  match List.find_opt (fun (c : Lin.column) -> Lin.same c.name col) m.columns with
  | None -> [ "<no column " ^ col ^ ">" ]
  | Some c -> List.map (fun ((s : Lin.src), k) -> s.rel ^ "." ^ s.col ^ ":" ^ Lin.kind_name k) (Lin.deps_list c.deps)

let expect_lineage name ?files ?(model = "q") sql col expected =
  let p = project_of ?files sql in
  let got = lineage_of p model col in
  let ok = List.sort compare got = List.sort compare expected in
  if not ok then Printf.printf "  %s: got [%s]\n" name (String.concat "; " got);
  check name ok

let column_names p mname = List.map (fun (c : Lin.column) -> c.name) (model p mname).columns
let warnings_of p = (model p "q").warnings

(* ------------------------------------------------------------ lexer *)

let () =
  let toks = Lexer.tokenize "select \"Order Id\", 'it''s' -- comment\n/* block */ from `proj.ds.t` where x::int >= 2" in
  let kinds = Array.to_list (Array.map (fun (t : Lexer.token) -> t.kind) toks) in
  check "lexer: quoted identifier keeps case" (List.mem (Lexer.Quoted "Order Id") kinds);
  check "lexer: doubled quote escape" (List.mem (Lexer.String "it's") kinds);
  check "lexer: backticked dotted name splits into parts"
    (List.mem (Lexer.Quoted "proj") kinds && List.mem (Lexer.Quoted "ds") kinds && List.mem (Lexer.Quoted "t") kinds);
  check "lexer: comments dropped" (not (List.exists (function Lexer.Ident "comment" | Lexer.Ident "block" -> true | _ -> false) kinds));
  check "lexer: :: and >= operators" (List.mem (Lexer.Op "::") kinds && List.mem (Lexer.Op ">=") kinds);
  check "lexer: line numbers" ((List.nth (Array.to_list toks) 5).line = 2)

(* ------------------------------------------------------------ parser *)

let parses sql = List.for_all Result.is_ok (Parser.parse_script sql).statements

let () =
  let ok =
    [ "select 1";
      "select a, b c, d as \"E\" from t where x in (1, 2) and y between 1 and 3 or z like 'a%' escape '!'";
      "select count(*) filter (where a > 0), sum(distinct b) from t group by all";
      "select * exclude (a, b) from t qualify row_number() over (partition by c order by d desc nulls last) = 1";
      "select t.* except (secret) replace (upper(name) as name) from t";
      "select x:customer.id::string as cid, payload['k'][0] from events";
      "select f.value from t, lateral flatten(input => t.items) f";
      "select item from orders, unnest(items) as item with offset";
      "select a from t1 natural join t2 cross join t3 full outer join t4 using (k)";
      "select listagg(name, ', ') within group (order by name) from t";
      "select extract(year from d), substring(s from 2 for 3), trim(both ' ' from s), position('a' in s) from t";
      "select cast(a as decimal(10, 2)), try_cast(b as varchar), a::timestamp with time zone from t";
      "select date '2024-01-01', interval '1 day', current_date, case when a then 1 end from t";
      "with recursive r(n) as (select 1 union all select n + 1 from r where n < 10) select n from r";
      "(select a from t1) union all (select a from t2) order by a limit 10 offset 5";
      "select a from t where exists (select 1 from u where u.id = t.id) and b = any (select c from v)";
      "select a from t tablesample bernoulli (10)";
      "select 1 from t fetch first 10 rows only";
      "create or replace transient table db.s.t cluster by (a) as select a from u";
      "insert into t (a, b) select x, y from u";
      "insert overwrite table t partition (dt) select * from u";
      "create table t (id int primary key, name varchar(10) not null, constraint pk primary key (id))";
      "values (1, 'a'), (2, 'b')";
      "select array[1, 2], struct(a, b), [1, 2, 3] from t";
      "select a from t lateral view explode(xs) e as x";
      "grant select on t to role r; set x = 1; select 1" ]
  in
  List.iter (fun sql -> check ("parses: " ^ sql) (parses sql)) ok;
  (match (Parser.parse_script "select from t").statements with
  | [ Error (msg, _, _) ] -> check "empty select list has a clear message" (String.length msg >= 22 && String.sub msg 0 22 = "expected a select list")
  | _ -> check "empty select list is an error" false);
  let r = Parser.parse_script "select from where; select 1" in
  check "bad statement is reported, next one still parses"
    (match r.statements with [ Error _; Ok _ ] -> true | _ -> false)

(* ------------------------------------------------------------ lineage *)

let () =
  expect_lineage "rename" "select id as order_id from orders" "order_id" [ "orders.id:direct" ];
  expect_lineage "expression" "select price * qty as total from items" "total"
    [ "items.price:transform"; "items.qty:transform" ];
  expect_lineage "cast stays direct" "select amount::numeric as amount from t" "amount" [ "t.amount:direct" ];
  expect_lineage "aggregate" "select sum(amount) as revenue from t" "revenue" [ "t.amount:aggregate" ];
  expect_lineage "count star has no column sources" "select count(*) as n from t" "n" [];
  expect_lineage "qualified through aliases"
    "select o.id, c.name from orders o join customers c on c.id = o.customer_id" "name" [ "customers.name:direct" ];
  expect_lineage "CTE chain"
    "with a as (select x as y from t), b as (select y + 1 as z from a) select z from b" "z" [ "t.x:transform" ];
  expect_lineage "CTE column list"
    "with a (renamed) as (select x from t) select renamed from a" "renamed" [ "t.x:direct" ];
  expect_lineage "derived table"
    "select s.total from (select sum(v) as total from t) s" "total" [ "t.v:aggregate" ];
  expect_lineage "union merges positionally"
    "select a as v from t1 union all select b from t2" "v" [ "t1.a:direct"; "t2.b:direct" ];
  expect_lineage "case reads conditions and values"
    "select case when status = 'x' then amount else 0 end as amt from t" "amt"
    [ "t.status:transform"; "t.amount:transform" ];
  expect_lineage "window partition and order count"
    "select row_number() over (partition by c order by d) as rn from t" "rn" [ "t.c:transform"; "t.d:transform" ];
  expect_lineage "scalar subquery, correlated"
    "select o.id, (select max(p.amount) from payments p where p.order_id = o.id) as max_paid from orders o" "max_paid"
    [ "payments.amount:aggregate" ];
  expect_lineage "lateral column alias"
    "select price * 2 as doubled, doubled + 1 as plus_one from t" "plus_one" [ "t.price:transform" ];
  expect_lineage "unnest value alias"
    "select item from orders, unnest(items) as item" "item" [ "orders.items:transform" ];
  expect_lineage "flatten table function"
    "select f.value as v from events e, lateral flatten(input => e.payload) f" "v" [ "events.payload:transform" ];
  expect_lineage "struct field access" "select address.city as city from customers" "city" [ "customers.address:transform" ];
  expect_lineage "BigQuery backticked table, short alias" "select t.a from `proj.ds.t`" "a" [ "proj.ds.t.a:direct" ];
  expect_lineage "three-part name" "select analytics.orders.id from analytics.orders" "id" [ "analytics.orders.id:direct" ];
  expect_lineage "recursive CTE"
    "with recursive r (n) as (select start_n from seed union all select n + 1 from r) select n from r" "n"
    [ "seed.start_n:transform" ];
  expect_lineage "insert target columns rename" ~model:"report"
    "insert into report (customer, spend) select name, sum(amount) from t group by name" "spend" [ "t.amount:aggregate" ];
  ()

(* SELECT * over an open table passes every column through. *)
let () =
  let p = project_of "with s as (select * from raw.orders) select id as order_id, s.status from s" in
  check "star passthrough: named read" (lineage_of p "q" "order_id" = [ "raw.orders.id:direct" ]);
  check "star passthrough: qualified read" (lineage_of p "q" "status" = [ "raw.orders.status:direct" ])

(* A schema closes tables: * expands, unqualified columns resolve, typos warn. *)
let () =
  let schema = { P.path = "schema.sql"; stem = "schema"; source = "create table orders (id int, amount int, note text);\ncreate table users (uid int, name text);" } in
  let p = project_of ~files:[ schema ] "select o.* exclude (note), name from orders o join users on uid = id" in
  check "schema: star expands to known columns" (column_names p "q" = [ "id"; "amount"; "name" ]);
  check "schema: unqualified column resolves" (lineage_of p "q" "name" = [ "users.name:direct" ]);
  let p = project_of ~files:[ schema ] "select o.amout from orders o" in
  check "schema: unknown column warns" (List.exists (fun w -> w = "o has no column amout") (warnings_of p))

let () =
  let p = project_of "select id from a join b on a.k = b.k" in
  check "ambiguous column warns" (List.exists (fun w -> String.length w > 0 && String.sub w 0 9 = "column id") (warnings_of p));
  check "ambiguous column keeps both candidates" (List.length (lineage_of p "q" "id") = 2);
  let p = project_of "select id from a join b using (id)" in
  check "USING key is not ambiguous" (warnings_of p = []);
  let p = project_of "select o.id from orders o where o.status = 'x' group by o.id having count(*) > 1" in
  let filters = List.map (fun ((s : Lin.src), _) -> s.rel ^ "." ^ s.col) (Lin.deps_list (model p "q").filters) in
  check "row filters collected" (List.sort compare filters = [ "orders.id"; "orders.status" ])

(* ------------------------------------------------------------ dbt *)

let () =
  let src = "{{ config(materialized='table') }}\n{# note #}\nselect id\nfrom {{ ref('stg_orders') }}\njoin {{ source('raw', 'users') }} on 1=1\n{% if is_incremental() %}where x > 1{% endif %}" in
  let out = Jinja.preprocess src in
  check "jinja: ref rewritten" (String.length out > 0 && Parser.parse_script out |> fun r -> List.for_all Result.is_ok r.statements);
  check "jinja: line numbers preserved"
    (List.length (String.split_on_char '\n' out) = List.length (String.split_on_char '\n' src));
  let contains s sub =
    let n = String.length s and m = String.length sub in
    let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
    go 0
  in
  check "jinja: source becomes schema.table" (contains out "raw.users");
  check "jinja: ref becomes the model name" (contains out "stg_orders")

(* dbt macros that decide lineage, and the ones that cannot be known *)
let () =
  let dbt sql col expected name = expect_lineage name sql col expected in
  dbt "select {{ dbt_utils.generate_surrogate_key(['order_id', 'line_no']) }} as sk from t" "sk"
    [ "t.order_id:transform"; "t.line_no:transform" ] "macro: surrogate key reads its columns";
  dbt "select {{ dbt.date_trunc('day', 'created_at') }} as d from t" "d" [ "t.created_at:transform" ] "macro: date_trunc";
  dbt "select {{ cents_to_dollars('amount') }} as amount from t" "amount" [ "t.amount:transform" ]
    "macro: unknown value macro reads its column arguments";
  dbt "select {{ my_fmt('amount', tz='UTC', package_name='x') }} as a from t" "a" [ "t.amount:transform" ]
    "macro: configuration arguments are not columns";
  dbt "select {{ var('fx_rate') }} * amount as inr from t" "inr" [ "t.amount:transform" ] "macro: var() is a constant";
  dbt "select {{ \"t.price\" if var('use_price', true) else 'null' }} as p from t" "p" [ "t.price:direct" ]
    "macro: inline if takes the first branch";
  dbt "select {% if is_incremental() %} new_col {% else %} old_col {% endif %} as c from t" "c" [ "t.old_col:direct" ]
    "jinja: is_incremental() counts as false";
  dbt "select {% if var('x') %} a {% elif y %} b {% else %} c {% endif %} as v from t" "v" [ "t.a:direct" ]
    "jinja: the first branch of if/elif/else";
  dbt "{% set q %} select broken from {% endset %} select a from t" "a" [ "t.a:direct" ] "jinja: set blocks are skipped";
  let p = project_of "select {{ dbt_utils.star(from=ref('x')) }} from {{ ref('x') }}" in
  check "macro: dbt_utils.star is *" (column_names p "q" = [ "*" ]);
  let p = project_of "select a, sum(b) as s from t {{ dbt_utils.group_by(n=1) }}" in
  check "macro: group_by(n) groups" (p.errors = [] && (model p "q").filters <> Lin.Deps.empty);
  let p = project_of "with spine as ({{ dbt.date_spine('day', '2020-01-01', '2021-01-01') }}) select date_day from spine" in
  check "macro: a macro producing a whole query is opaque, not an error" (p.errors = [] && warnings_of p = []);
  let p = project_of "select\n  {{ fill_staging_columns(cols) }}\n  {{ source_relation() }}\n  id\nfrom base" in
  check "macro: macro-generated select items need no commas" (p.errors = [] && List.mem "id" (column_names p "q"));
  let p = project_of "select a from t {{ livemode_predicate() }} and b > 1" in
  check "macro: predicate macro followed by AND" (p.errors = [])

let read_file path = In_channel.with_open_bin path In_channel.input_all

let rec sql_files dir =
  Sys.readdir dir |> Array.to_list |> List.sort compare
  |> List.concat_map (fun f ->
         let p = Filename.concat dir f in
         if Sys.is_directory p then sql_files p
         else if Filename.check_suffix f ".sql" then [ { P.path = p; stem = Filename.remove_extension f; source = read_file p } ]
         else [])

let () =
  let p = P.analyze (sql_files "fixtures/dukaan/models") in
  check "dukaan: no errors" (p.errors = []);
  check "dukaan: no warnings" (P.warnings p = []);
  let names = List.map (fun (m : P.model) -> m.name) p.models in
  let index n = let rec go i = function [] -> -1 | x :: r -> if x = n then i else go (i + 1) r in go 0 names in
  check "dukaan: upstream models resolve first"
    (index "stg_orders" < index "fct_orders" && index "fct_orders" < index "dim_customers" && index "stg_payments" < index "fct_orders");
  check "dukaan: lineage crosses models"
    (lineage_of p "dim_customers" "lifetime_value" = [ "fct_orders.amount_paid:aggregate" ]);
  check "dukaan: star from source CTE resolves" (lineage_of p "stg_orders" "customer_id" = [ "raw.orders.user_id:direct" ]);
  let rec flatten (P.Tree (n, _, kids)) = (n.table ^ "." ^ n.column) :: List.concat_map flatten kids in
  let impacted = flatten (P.impact p { table = "raw.payments"; column = "amount" }) in
  List.iter
    (fun n -> check ("dukaan: impact reaches " ^ n) (List.mem n impacted))
    [ "stg_payments.amount"; "fct_orders.amount_paid"; "fct_orders.balance_due"; "dim_customers.tier"; "channel_daily.revenue" ];
  check "dukaan: impact does not over-reach" (not (List.mem "dim_customers.full_name" impacted));
  let traced = flatten (P.trace p { table = "dim_customers"; column = "tier" }) in
  check "dukaan: trace reaches the raw column" (List.mem "raw.payments.amount" traced);
  check "dukaan: status filters dim_customers" (List.mem "dim_customers" (P.filtered_by p { table = "fct_orders"; column = "status" }));
  let unused = List.map (fun (n : P.node) -> n.table ^ "." ^ n.column) (P.unused p) in
  check "dukaan: unused staging column found" (List.mem "stg_customers.email" unused);
  check "dukaan: used column not reported" (not (List.mem "stg_customers.full_name" unused));
  check "dukaan: final models never reported" (not (List.exists (fun u -> String.length u > 13 && String.sub u 0 13 = "dim_customers") unused))

let () =
  let p = project_of "select a from t; select from where" in
  check "errors carry a line number" (match p.errors with [ e ] -> e.line = 1 | _ -> false);
  let cyc = P.analyze [ { P.path = "a.sql"; stem = "a"; source = "select x from b" }; { P.path = "b.sql"; stem = "b"; source = "select x from a" } ] in
  check "dependency cycle is reported" (List.exists (fun (e : P.error) -> String.length e.message >= 16 && String.sub e.message 0 16 = "dependency cycle") cyc.errors)

let () =
  let p = P.analyze (sql_files "fixtures/dukaan/models") in
  let j = Json.to_string (Render.project_json p) in
  check "json: has models and edges" (String.length j > 1000 && j.[0] = '{');
  let m = Render.mermaid p in
  check "mermaid: starts with flowchart" (String.sub m 0 12 = "flowchart LR");
  check "dot: is a digraph" (String.sub (Render.dot p) 0 15 = "digraph lineage")

let () =
  Printf.printf "%d passed, %d failed\n" !passed !failures;
  if !failures > 0 then exit 1
