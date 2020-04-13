import cfg

check_bq_data_exists = """
SELECT load_id
FROM `bigquery-public-data.austin_waste.waste_and_diversion`
WHERE report_date BETWEEN DATE('{{ macros.ds_add(ds, -365) }}') AND DATE('{{ ds }}')
"""

write_weight_data_to_bq = """
SELECT report_date AS date,
       load_type AS type,
       SUM(load_weight) AS weight
FROM `bigquery-public-data.austin_waste.waste_and_diversion` 
WHERE report_date BETWEEN DATE('{{ macros.ds_add(ds, -365) }}') AND DATE('{{ ds }}')
GROUP BY report_date, load_type
HAVING type = '""" + cfg.TYPE + """'
"""

prepare_and_merge_data = """
WITH
simple_route_counts AS (
SELECT report_date,
       route_type,
       count(route_type) AS count
FROM `my-first-project-238015.waste.route` 
GROUP BY report_date, route_type
),
max_route_counts AS (
SELECT report_date,
       FIRST_VALUE(route_type) OVER (PARTITION BY report_date ORDER BY count DESC) AS top_route,
       ROW_NUMBER() OVER (PARTITION BY report_date ORDER BY count desc) AS row_number
FROM simple_route_counts
),
top_routes AS (
SELECT report_date AS date,
       top_route,
FROM max_route_counts
WHERE row_number = 1
)
SELECT a.date,
       a.type,
       a.weight,
       b.top_route
FROM `my-first-project-238015.waste.weight` a
LEFT JOIN top_routes b
ON a.date = b.date
ORDER BY a.date DESC
"""