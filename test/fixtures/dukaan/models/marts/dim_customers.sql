with order_stats as (
    select
        customer_id,
        min(ordered_at)       as first_order_at,
        max(ordered_at)       as last_order_at,
        count(*)              as order_count,
        sum(amount_paid)      as lifetime_value
    from {{ ref('fct_orders') }}
    where status = 'completed'
    group by customer_id
)

select
    c.customer_id,
    c.full_name,
    c.city,
    s.first_order_at,
    s.last_order_at,
    coalesce(s.order_count, 0)        as order_count,
    coalesce(s.lifetime_value, 0)     as lifetime_value,
    case
        when s.lifetime_value >= 50000 then 'gold'
        when s.lifetime_value >= 10000 then 'silver'
        else 'bronze'
    end                               as tier
from {{ ref('stg_customers') }} c
left join order_stats s on s.customer_id = c.customer_id
