select
    id                          as payment_id,
    order_id,
    method                      as payment_method,
    -- amounts arrive in paise
    amount / 100.0              as amount,
    case when method in ('upi', 'card') then 'online' else 'offline' end as payment_type
from {{ source('raw', 'payments') }}
