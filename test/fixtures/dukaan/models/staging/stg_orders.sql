with source as (
    select * from {{ source('raw', 'orders') }}
),

renamed as (
    select
        id            as order_id,
        user_id       as customer_id,
        placed_at::timestamp as ordered_at,
        status,
        channel,
        coupon_code
    from source
)

select * from renamed
where status <> 'test'
