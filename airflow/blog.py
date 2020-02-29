# import general modules
import os
import json
from datetime import datetime, timedelta

# import airflow modules
from airflow import DAG
from airflow.operators.bash_operator import BashOperator
from airflow.operators.python_operator import PythonOperator
from airflow.contrib.operators.bigquery_operator import BigQueryOperator
from airflow.contrib.operators.bigquery_check_operator import BigQueryCheckOperator
from airflow.contrib.operators.gcs_to_bq import GoogleCloudStorageToBigQueryOperator
from airflow.contrib.operators.bigquery_to_gcs import BigQueryToCloudStorageOperator
from airflow.contrib.sensors.gcs_sensor import GoogleCloudStorageObjectSensor 

# set up configuration
start_date = datetime(2020, 2, 20)
schedule_interval = timedelta(days=1)

# set config parameters
PROJECT = 'my-first-project-238015'
DATASET = 'waste'

TABLE_WEIGHT = 'weight'
TABLE_ROUTE = 'route'
TABLE_MERGE = 'merge'

BQ_TABLE_WEIGHT = '.'.join([DATASET, TABLE_WEIGHT])
BQ_TABLE_ROUTE = '.'.join([PROJECT, DATASET, TABLE_ROUTE])
BQ_TABLE_MERGE = '.'.join([DATASET, TABLE_MERGE])

BUCKET = 'austin_waste'
FOLDER_DAT = 'data'
FOLDER_RES = 'results'

SOURCE_OBJECT = FOLDER_DAT + '/' +'route.csv'
DESTINATION_OBJECT = FOLDER_RES + '/' + 'merge.csv'

BUCKET_URI = 'gs://' + BUCKET + '/' 
DESTINATION_URI = BUCKET_URI + DESTINATION_OBJECT

TYPE = 'DEAD ANIMAL'

# set default dag arguments
default_args = {'owner': 'Manuel Tilgner',
                'depends_on_past': False,
                'start_date': start_date,
                'email': ['manuel.tilgner@statworx.com'],
                'email_on_failure': False,
                'email_on_retry': False,
                'retries': 1,
                'retry_delay': timedelta(minutes=5)}

# compose queries for our workflow
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
HAVING type = '""" + TYPE + """'
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

# write dag
with DAG(dag_id='blog', default_args=default_args, schedule_interval=schedule_interval, catchup=False) as dag:

    t1 = BigQueryCheckOperator(task_id='check_bq_data_exists',
                               sql=check_bq_data_exists,
                               use_legacy_sql=False)

    t2 = GoogleCloudStorageObjectSensor(task_id='check_gcs_file_exists',
                                        bucket=BUCKET,
                                        object=SOURCE_OBJECT)

    t3 = BigQueryOperator(task_id='write_weight_data_to_bq',
                          sql=write_weight_data_to_bq,
                          destination_dataset_table=BQ_TABLE_WEIGHT,
                          create_disposition='CREATE_IF_NEEDED',
                          write_disposition='WRITE_TRUNCATE',
                          use_legacy_sql=False)

    t4 = GoogleCloudStorageToBigQueryOperator(task_id='write_route_data_to_bq',
                                              bucket=BUCKET,
                                              source_objects=[SOURCE_OBJECT],
                                              field_delimiter=';',
                                              destination_project_dataset_table=BQ_TABLE_ROUTE,
                                              create_disposition='CREATE_IF_NEEDED',
                                              write_disposition='WRITE_TRUNCATE',
                                              skip_leading_rows=1)

    t5 = BigQueryOperator(task_id='prepare_and_merge_data',
                          sql=prepare_and_merge_data,
                          use_legacy_sql=False,
                          destination_dataset_table=BQ_TABLE_MERGE,
                          create_disposition='CREATE_IF_NEEDED',
                          write_disposition='WRITE_TRUNCATE')

    t6 = BigQueryToCloudStorageOperator(task_id='export_results_to_gcs',
                                        source_project_dataset_table=BQ_TABLE_MERGE,
                                        destination_cloud_storage_uris=DESTINATION_OBJECT,
                                        export_format='CSV')

    t1 >> t2 >> [t3, t4] >> t5 >> t6
