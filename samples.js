window.VANSH_SAMPLES = {
 "dbt": [
  {
   "name": "models/staging/stg_customers.sql",
   "source": "-- One row per customer, cleaned.\n{{ config(materialized='view') }}\n\nselect\n    id                                    as customer_id,\n    trim(first_name) || ' ' || trim(last_name) as full_name,\n    lower(email)                          as email,\n    city,\n    signup_date::date                     as signed_up_on,\n    marketing_opt_in                      as is_opted_in\nfrom {{ source('raw', 'customers') }}\nwhere not is_test_account\n"
  },
  {
   "name": "models/staging/stg_order_items.sql",
   "source": "select\n    oi.order_id,\n    oi.product_id,\n    p.name                  as product_name,\n    p.category,\n    oi.quantity,\n    oi.quantity * p.price   as line_total\nfrom {{ source('raw', 'order_items') }} as oi\njoin {{ source('raw', 'products') }} as p\n    on p.id = oi.product_id\n"
  },
  {
   "name": "models/staging/stg_orders.sql",
   "source": "with source as (\n    select * from {{ source('raw', 'orders') }}\n),\n\nrenamed as (\n    select\n        id            as order_id,\n        user_id       as customer_id,\n        placed_at::timestamp as ordered_at,\n        status,\n        channel,\n        coupon_code\n    from source\n)\n\nselect * from renamed\nwhere status <> 'test'\n"
  },
  {
   "name": "models/staging/stg_payments.sql",
   "source": "select\n    id                          as payment_id,\n    order_id,\n    method                      as payment_method,\n    -- amounts arrive in paise\n    amount / 100.0              as amount,\n    case when method in ('upi', 'card') then 'online' else 'offline' end as payment_type\nfrom {{ source('raw', 'payments') }}\n"
  },
  {
   "name": "models/marts/channel_daily.sql",
   "source": "select\n    date_trunc('day', ordered_at)   as order_date,\n    channel,\n    count(*)                        as orders,\n    sum(amount_paid)                as revenue\nfrom {{ ref('fct_orders') }}\ngroup by 1, 2\nqualify revenue > 0\n"
  },
  {
   "name": "models/marts/dim_customers.sql",
   "source": "with order_stats as (\n    select\n        customer_id,\n        min(ordered_at)       as first_order_at,\n        max(ordered_at)       as last_order_at,\n        count(*)              as order_count,\n        sum(amount_paid)      as lifetime_value\n    from {{ ref('fct_orders') }}\n    where status = 'completed'\n    group by customer_id\n)\n\nselect\n    c.customer_id,\n    c.full_name,\n    c.city,\n    s.first_order_at,\n    s.last_order_at,\n    coalesce(s.order_count, 0)        as order_count,\n    coalesce(s.lifetime_value, 0)     as lifetime_value,\n    case\n        when s.lifetime_value >= 50000 then 'gold'\n        when s.lifetime_value >= 10000 then 'silver'\n        else 'bronze'\n    end                               as tier\nfrom {{ ref('stg_customers') }} c\nleft join order_stats s on s.customer_id = c.customer_id\n"
  },
  {
   "name": "models/marts/fct_orders.sql",
   "source": "{{ config(materialized='table') }}\n\nwith payments as (\n    select\n        order_id,\n        sum(amount)                                               as amount_paid,\n        sum(case when payment_type = 'online' then amount end)    as online_amount,\n        count(*)                                                  as payment_count\n    from {{ ref('stg_payments') }}\n    group by order_id\n),\n\nitems as (\n    select order_id, sum(line_total) as items_total, count(distinct product_id) as distinct_products\n    from {{ ref('stg_order_items') }}\n    group by 1\n)\n\nselect\n    o.order_id,\n    o.customer_id,\n    o.ordered_at,\n    o.status,\n    o.channel,\n    coalesce(p.amount_paid, 0)                    as amount_paid,\n    p.online_amount,\n    i.items_total,\n    i.items_total - coalesce(p.amount_paid, 0)    as balance_due,\n    i.distinct_products,\n    row_number() over (partition by o.customer_id order by o.ordered_at) as customer_order_number\nfrom {{ ref('stg_orders') }} o\nleft join payments p on p.order_id = o.order_id\nleft join items i on i.order_id = o.order_id\n"
  }
 ],
 "query": [
  {
   "name": "order_attribution.sql",
   "source": "-- Marketing attribution: last paid touch before each order, Snowflake style.\ncreate or replace table analytics.order_attribution as\nwith touches as (\n    select user_id, occurred_at, channel, campaign, cost_inr\n    from raw.ad_clicks\n    union all\n    select user_id, sent_at, 'email', template_name, 0\n    from raw.email_sends\n),\n\norders as (\n    select\n        o.order_id,\n        o.user_id,\n        o.created_at,\n        o.subtotal + o.shipping - coalesce(o.discount, 0) as net_revenue\n    from raw.orders o\n    where o.status not in ('cancelled', 'test')\n),\n\nlast_touch as (\n    select\n        orders.*,\n        t.channel,\n        t.campaign,\n        t.cost_inr,\n        datediff('hour', t.occurred_at, orders.created_at) as hours_to_convert\n    from orders\n    join touches t\n      on t.user_id = orders.user_id\n     and t.occurred_at <= orders.created_at\n    qualify row_number() over (partition by orders.order_id order by t.occurred_at desc) = 1\n)\n\nselect\n    order_id,\n    user_id,\n    channel,\n    campaign,\n    net_revenue,\n    hours_to_convert,\n    net_revenue - cost_inr                                   as contribution,\n    sum(net_revenue) over (partition by campaign)            as campaign_revenue,\n    case when hours_to_convert <= 24 then 'same day' else 'later' end as speed\nfrom last_touch\n"
  }
 ],
 "blank": [
  {
   "name": "query.sql",
   "source": "-- Write any SELECT, or add more files with + File.\nselect\n    id,\n    amount * 2 as doubled\nfrom orders\n"
  }
 ]
};
