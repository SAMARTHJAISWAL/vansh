{{ config(materialized='table') }}

with payments as (
    select
        order_id,
        sum(amount)                                               as amount_paid,
        sum(case when payment_type = 'online' then amount end)    as online_amount,
        count(*)                                                  as payment_count
    from {{ ref('stg_payments') }}
    group by order_id
),

items as (
    select order_id, sum(line_total) as items_total, count(distinct product_id) as distinct_products
    from {{ ref('stg_order_items') }}
    group by 1
)

select
    o.order_id,
    o.customer_id,
    o.ordered_at,
    o.status,
    o.channel,
    coalesce(p.amount_paid, 0)                    as amount_paid,
    p.online_amount,
    i.items_total,
    i.items_total - coalesce(p.amount_paid, 0)    as balance_due,
    i.distinct_products,
    row_number() over (partition by o.customer_id order by o.ordered_at) as customer_order_number
from {{ ref('stg_orders') }} o
left join payments p on p.order_id = o.order_id
left join items i on i.order_id = o.order_id
