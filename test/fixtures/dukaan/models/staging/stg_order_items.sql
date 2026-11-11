select
    oi.order_id,
    oi.product_id,
    p.name                  as product_name,
    p.category,
    oi.quantity,
    oi.quantity * p.price   as line_total
from {{ source('raw', 'order_items') }} as oi
join {{ source('raw', 'products') }} as p
    on p.id = oi.product_id
