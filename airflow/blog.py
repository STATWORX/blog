# set up blog
# airflow is a framework for managing workflows. it allows you to programatically
# define, schedule and monitor workflows with tasks and dependencies for whatever
# job you need. it is based on directed acyclic graphs. error handling comes included.

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

# set up bigquery connection
# airflow connections -d --conn_id bigquery_default
# airflow connections -a --conn_id bigquery_default --conn_uri
# 'google-cloud-platform://:@:?extra__google_cloud_platform__project=my-first-project-238015&
# extra__google_cloud_platform__key_path=/Users/manueltilgner/personal_token.json'
# airflow connections -d --conn_id google_cloud_default
# airflow connections -a --conn_id google_cloud_default --conn_uri 
# 'google-cloud-platform://:@:?extra__google_cloud_platform__project=my-first-project-238015&
# extra__google_cloud_platform__key_path=/Users/manueltilgner/personal_token.json'

# it is important to note that the scheduler runs your job one schedule_interval
# AFTER the start date, at the END of the period. so if we set the
# start_date for 2020-01-01, the dag will run on 2020-01-02, the
# macro {{ ds }} will still equal 2020-01-01 though. you can also use
# {{ yesterday_ds }}. by setting it to yesterday's date, the dag will run 
# immidiately.

# in the words of airflow 
# If you run a DAG on a schedule_interval of one day, the run stamped 2020-01-01
# will be triggered soon after 2020-01-01T23:59. In other words, the job instance
# is started once the period it covers has ended. The execution_date available in
# the context will also be 2020-01-01.

# The first DAG Run is created based on the minimum start_date for the tasks
# in your DAG. Subsequent DAG Runs are created by the scheduler process, based
# on your DAG’s schedule_interval, sequentially. If your start_date is 2020-01-01
# and schedule_interval is @daily, the first run will be created on 
# 2020-01-02 i.e., after your start date has passed.

# Now, about execution_date and when it is triggered, this is a common gotcha
# for people onboarding on Airflow. Airflow sets execution_date based on the
# left bound of the schedule period it is covering, not based on when it fires
# (which would be the right bound of the period). When running an schedule='@hourly'
# task for instance, a task will fire every hour. The task that fires at 2pm will
# have an execution_date of 1pm because it assumes that you are processing the
# 1pm to 2pm time window at 2pm. Similarly, if you run a daily job, the run an
# with execution_date of 2016-01-01 would trigger soon after midnight on 2016-01-02.

# start_date = The first dag start time. keep it STATIC
# execution_date = max(start_date, last_run_date)
# schedule_interval parameter accepts cron or timedelta values
# next_dag_start_date = execution_date + schedule_interval
# On Home Page, Last Run is execution_date. Hoover over on ( i ) to see
# the actual last run time

# Let’s pretend that we unpause the DAG. What will the scheduler do in regards
# to the Foo DAG? It will attempt to find previous runs. However, since this
# is a new DAG, there won’t be any previous runs.

# Key Takeaway: The execution_date of a DAGRun is not when the DAG starts.
# The general rule of thumb is: the execution_date is one cron iteration prior 
# to when the DAG Run is supposed to be scheduled to run. For example, if a job 
# is supposed to run every hour, the execution_date of the DAGRun created at
# approximately 2 PM will be 1 PM.

# When the scheduler taps this DAG (with 'start_date': datetime(2018, 1, 1)),
# it will generate a DAG run for each day from the start_date to 4/30/18,
# and then for each day going forward.

# set up configuration
# i'm setting the start date to yesterday, airflow recommends setting a static
# date here as dynamic dates can behvave unpredictably,
# so it's just for demonstration purposes
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
# choose the start date with care because airflow can and will do backfill from
# this date to the present
default_args = {'owner': 'Manuel Tilgner',
                'depends_on_past': False,
                'start_date': start_date,
                'email': ['manuel.tilgner@statworx.com'],
                'email_on_failure': False,
                'email_on_retry': False,
                'retries': 1,
                'retry_delay': timedelta(minutes=5)}

# specify schema
# bq show --format=prettyjson bigquery-public-data:austin_waste.waste_and_diversion
# | jq '.schema.fields'

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
