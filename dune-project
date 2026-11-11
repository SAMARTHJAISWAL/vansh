(lang dune 3.20)

(name vansh)

(generate_opam_files true)

(source
 (github SAMARTHJAISWAL/vansh))

(authors "Samarth Jaiswal")

(license MIT)

(package
 (name vansh)
 (synopsis "Column-level lineage for SQL and dbt projects")
 (description
  "vansh parses SQL (and dbt models with ref/source) and traces every output column back to the source columns it is computed from, through CTEs, subqueries, joins, set operations and SELECT *. It answers impact questions such as: if I change this column, what breaks downstream?")
 (depends
  (ocaml
   (>= 5.2))
  dune)
 (depopts js_of_ocaml js_of_ocaml-compiler))
