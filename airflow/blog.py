# import general modules
import os
import json
from datetime import datetime, timedelta

# import config parameters and queries
import cfg 
import queries

# import airflow modules
from airflow import DAG
from airflow.contrib.operators.bigquery_operator import BigQueryOperator
from airflow.contrib.operators.bigquery_check_operator import BigQueryCheckOperator
from airflow.contrib.operators.gcs_to_bq import GoogleCloudStorageToBigQueryOperator
from airflow.contrib.operators.bigquery_to_gcs import BigQueryToCloudStorageOperator
from airflow.contrib.sensors.gcs_sensor import GoogleCloudStorageObjectSensor 

# set up configuration
start_date = datetime(2020, 2, 20)
schedule_interval = timedelta(days=1)

# set default dag arguments
default_args = {'owner': 'Manuel Tilgner',
                'depends_on_past': False,
                'start_date': start_date,
                'email': ['manuel.tilgner@statworx.com'],
                'email_on_failure': False,
                'email_on_retry': False,
                'retries': 1,
                'retry_delay': timedelta(minutes=5)}

# write dag
with DAG(dag_id='blog', default_args=default_args, schedule_interval=schedule_interval, catchup=False) as dag:

    t1 = BigQueryCheckOperator(task_id='check_bq_data_exists',
                               sql=queries.check_bq_data_exists,
                               use_legacy_sql=False)

    t2 = GoogleCloudStorageObjectSensor(task_id='check_gcs_file_exists',
                                        bucket=cfg.BUCKET,
                                        object=cfg.SOURCE_OBJECT)

    t3 = BigQueryOperator(task_id='write_weight_data_to_bq',
                          sql=queries.write_weight_data_to_bq,
                          destination_dataset_table=cfg.BQ_TABLE_WEIGHT,
                          create_disposition='CREATE_IF_NEEDED',
                          write_disposition='WRITE_TRUNCATE',
                          use_legacy_sql=False)

    t4 = GoogleCloudStorageToBigQueryOperator(task_id='write_route_data_to_bq',
                                              bucket=cfg.BUCKET,
                                              source_objects=[cfg.SOURCE_OBJECT],
                                              field_delimiter=';',
                                              destination_project_dataset_table=cfg.BQ_TABLE_ROUTE,
                                              create_disposition='CREATE_IF_NEEDED',
                                              write_disposition='WRITE_TRUNCATE',
                                              skip_leading_rows=1)

    t5 = BigQueryOperator(task_id='prepare_and_merge_data',
                          sql=queries.prepare_and_merge_data,
                          use_legacy_sql=False,
                          destination_dataset_table=cfg.BQ_TABLE_MERGE,
                          create_disposition='CREATE_IF_NEEDED',
                          write_disposition='WRITE_TRUNCATE')

    t6 = BigQueryToCloudStorageOperator(task_id='export_results_to_gcs',
                                        source_project_dataset_table=cfg.BQ_TABLE_MERGE,
                                        destination_cloud_storage_uris=cfg.DESTINATION_URI,
                                        export_format='CSV')

    t1 >> t2 >> [t3, t4] >> t5 >> t6
