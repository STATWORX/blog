# Naive Considerations on Delta Lake querying

Lakehouse is a prominent data approach to manage huge volumes. The main idea is the use of the columnar format parquet, put some metadata on top of that to basically ensure transactional writes and store it to cheap storage (object storages like S3, ADLS, etc). When I was working with that approach, I questioned the efficiency of the approach and my naive understanding was that all data would always to be read to process the data, to query it for reporting purposes. (Disclamer: this is not true). This is a uncovering.
First of all, I want to introduce main requirements from batch processing and ad-hoc querying, then move level by level to see what features are given. 

## Different requirements ad-hoc queries vs. batch processing
With ad-hoc queries an interface for a user/application is meant that can quasi-randomly execute queries on some data sets.
With batch processing a well-defined (i.e. well-known) and deployed process is meant that runs on a regular schedule, loads some data, processes it and stores it back.
Let us consider different dimensions.

### Latency / Efficiency
Batch processing does not rely on latency, however execution efficiency is still a valid metric to consider as it saves money.
Ad-hoc queries definitely require good latency performance for user's trust and the business value. Latency is a part of the execution efficiency of the query. The efficiency of the whole process (may include storage, preaggregation) is a rather subordinal measure.

### Reliability / Precision
Batch processing shall be resilient to downtimes of connected upstream system, shall be able to restore failed tasks, shall calculate the outcome in a precise manner as downstream processes rely on the output, shall produce the same outcome on the same data (idempotency).
Ad-hoc queries' requirements are different: Value estimates instead of precise values (thinking of Theta sketches) are acceptable in various situations. Also recalcualting a value in case of an outage is supposed to be acceptable if that does not occur too often. However, the overall query service downtime should be reduced.

### Data Layout
The data layout shall enable good query performances on different kind of queries. Batch processing, having the nature of a defined process, uses specific queries that the layout can be optimized towards.

### Scalability
Both types need scalability but in different aspects. The batch process and the backend need to adapt with rising amount of data. This, in general, is observable and can be remediated by adding more components to the scalable backend. With the regards to ad-hoc queries, the scalability requirements also encloses the (unknown) rise in the number of queries.


## Performance Measures implemented in Parquet
### Due to the nature of a columnar format
**TODO** Image of Columnar.

First of all, Parquet is a columnar format, i.e. data is not stored row-wise as in CSV or Avro format but columns are stored en bloc. To be more precise, data sets are split up into row groups (= slices of data in the row dimension) and within the groups, column data is kept together. Thus, whenever readers are supposed to read only a subset of columns, readers can look up the offset (Have a look at [parquet-format](https://github.com/apache/parquet-format#file-format) ) of the needed column blocks, and read only the portions needed to be loaded over network and into RAM. Especially for wide tables and/or slice and dice queries, this saves a lot of unnessarily loaded data.
That's nice!
### Statistics
**TODO** Image of Bloom Filter

Secondly, lets have a look how the number of data reads can be reduced. Parquet allows to **store statistics** of columns for minimal and maximal values within the metadata. Data pages not matching a filter clause can by that omitted.
For those columns with categorical values and a high cardinality, checks against min-max-boundaries often do not make too much sense. Noting down every occuring element in a (sorted) dictionary, helps as a prefilter of data pages but comes with the downside of space usage. A good compromise is a **Bloom Filter** that is part of the format specification since version 2.7.0. Briefly, a Bloom Filter is an efficient approximation to a set that produces true negatives but no false positives.
**TODO** bit more on bloom filters.
 Nice stuff, too! (See [Bloom Filter Wikipedia](https://en.wikipedia.org/wiki/Bloom_filter))

 ### Indices
 

### Supported Features of Writer Implementation
Having first a look at Pandas and let's restrict to the underlying engine of pyarrow. Currently the versions 1.0, 2.4 and 2.6 are supported, see [Pyarrow](https://arrow.apache.org/docs/python/generated/pyarrow.parquet.write_table.html#pyarrow.parquet.write_table). In addition, writing statistics can be restricted to a handful of columns instead of all (parameter: `write_statistics`)

Spark on the other hand relies on `parquet-mr` and starting with version 1.11.0 the format version 2.7.0 is integrated. With Spark version 3.2.0 and higher it integrates `parquet-mr` version 1.12.1 and higher.


## Performance Measures implemented in Delta Tables
### Statistics
According to the [Delta Table specifications](https://github.com/delta-io/delta/blob/master/PROTOCOL.md#per-file-statistics), statistics for min and max value can be added to the Delta log. Like in the parquet implementation, this enables data skipping but now on a higher level: skipping whole files instead to columns from row groups. 

### Apart from Statistics
Delta as format does not provide any further mechanism to improve the query performance. Databricks, however, added some tool to the box that further could improve the performance:
**Z order** is a mechanism to achieve a sorting algorithm on multiple fields. Thus, data can be sorted before and then broken up into files. This makes data skipping much more efficient, when filtered on a subset of the field in the Z order set. 
Databricks also implemented a **Bloom Filter Index** that works in the same fashion as for Parquet files. The implementation has not been open sourced yet.

### Comment on Iceberg
Iceberg is a competitor for Delta Tables. I could not find out from the specification document any possibility to store statistics along the metadatas.

## Comments on modern Object Storages
Nowadays, data resides mostly on object stores like S3 or Azure's Blobstorage. The main feature that would ensure a smart approach to data reads is that data can be read partially with ranges. 
Both Azure REST API implementation as well as S3 support such byte range reads. 

## Naive Questions at the end
...