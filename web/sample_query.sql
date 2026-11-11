-- Marketing attribution: last paid touch before each order, Snowflake style.
create or replace table analytics.order_attribution as
with touches as (
    select user_id, occurred_at, channel, campaign, cost_inr
    from raw.ad_clicks
    union all
    select user_id, sent_at, 'email', template_name, 0
    from raw.email_sends
),

orders as (
    select
        o.order_id,
        o.user_id,
        o.created_at,
        o.subtotal + o.shipping - coalesce(o.discount, 0) as net_revenue
    from raw.orders o
    where o.status not in ('cancelled', 'test')
),

last_touch as (
    select
        orders.*,
        t.channel,
        t.campaign,
        t.cost_inr,
        datediff('hour', t.occurred_at, orders.created_at) as hours_to_convert
    from orders
    join touches t
      on t.user_id = orders.user_id
     and t.occurred_at <= orders.created_at
    qualify row_number() over (partition by orders.order_id order by t.occurred_at desc) = 1
)

select
    order_id,
    user_id,
    channel,
    campaign,
    net_revenue,
    hours_to_convert,
    net_revenue - cost_inr                                   as contribution,
    sum(net_revenue) over (partition by campaign)            as campaign_revenue,
    case when hours_to_convert <= 24 then 'same day' else 'later' end as speed
from last_touch
