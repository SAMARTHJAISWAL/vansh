Lineage of a single query from stdin:

  $ echo "select o.id, o.amount * 1.18 as with_tax, sum(p.paid) as paid from orders o join payments p on p.order_id = o.id group by 1, 2" | vansh --color never
  query  <stdin>
    id        <- orders.id
    with_tax  <- orders.amount transform
    paid      <- payments.paid aggregate
    rows filtered or grouped by: orders.amount, orders.id, payments.order_id
  
  sources
    orders  id, amount
    payments  paid, order_id

Impact analysis across the dbt fixture project:

  $ vansh impact raw.payments.amount fixtures/dukaan/models --color never
  downstream impact (what is computed from this column)
  raw.payments.amount
  └─ stg_payments.amount (transform)
     ├─ fct_orders.amount_paid (aggregate)
     │  ├─ channel_daily.revenue (aggregate)
     │  ├─ dim_customers.lifetime_value (aggregate)
     │  └─ dim_customers.tier (aggregate)
     ├─ fct_orders.online_amount (aggregate)
     └─ fct_orders.balance_due (aggregate)

Upstream trace:

  $ vansh trace dim_customers.tier fixtures/dukaan/models --color never
  upstream lineage (what this column is computed from)
  dim_customers.tier
  └─ fct_orders.amount_paid (aggregate)
     └─ stg_payments.amount (aggregate)
        └─ raw.payments.amount (transform)

Columns nobody downstream uses:

  $ vansh unused fixtures/dukaan/models
  15 columns produced but never used downstream:
    stg_payments.payment_id
    stg_payments.payment_method
    stg_order_items.product_name
    stg_order_items.category
    stg_order_items.quantity
    stg_orders.coupon_code
    fct_orders.order_id
    fct_orders.online_amount
    fct_orders.items_total
    fct_orders.balance_due
    fct_orders.distinct_products
    fct_orders.customer_order_number
    stg_customers.email
    stg_customers.signed_up_on
    stg_customers.is_opted_in

CI check passes on the clean project:

  $ vansh check fixtures/dukaan/models
  7 models, 0 errors, 0 warnings

and fails on a broken query, with the line number:

  $ printf 'select a from t;\n\nselect from where' | vansh check
  error: <stdin>:3:8 expected a select list (found from)
  1 model, 1 error, 0 warnings
  [1]

Mermaid output:

  $ echo "select a as b from t" | vansh graph
  flowchart LR
    subgraph t_t["t"]
      t__a["a"]
    end
    class t_t source
    subgraph t_query["query"]
      query__b["b"]
    end
    t__a --> query__b
    classDef source fill:#fff7e6,stroke:#d98a00

Bad arguments:

  $ vansh impact fixtures/dukaan/models
  vansh: expected TABLE.COLUMN, got fixtures/dukaan/models
  [2]
