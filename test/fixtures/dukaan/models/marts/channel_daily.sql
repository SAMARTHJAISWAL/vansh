select
    date_trunc('day', ordered_at)   as order_date,
    channel,
    count(*)                        as orders,
    sum(amount_paid)                as revenue
from {{ ref('fct_orders') }}
group by 1, 2
qualify revenue > 0
