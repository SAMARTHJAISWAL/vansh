-- One row per customer, cleaned.
{{ config(materialized='view') }}

select
    id                                    as customer_id,
    trim(first_name) || ' ' || trim(last_name) as full_name,
    lower(email)                          as email,
    city,
    signup_date::date                     as signed_up_on,
    marketing_opt_in                      as is_opted_in
from {{ source('raw', 'customers') }}
where not is_test_account
