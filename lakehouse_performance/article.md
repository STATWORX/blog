# Naive Considerations on Delta Lake querying

- Difference user queries / batch processing
- Prolog: Lakehouse
- Performance measures on parquet
- Performance measures on Delta
- 

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
First, Parquet is a columnar format, i.e. data is not stored row-wise as in CSV or Avro format but columns are stored en bloc. To be more precise, data sets are split up into row groups (= slices of data in the row dimension) and within the groups, column data sticks together. Thus, whenever readers are supposed to read only a subset of columns, readers can look up the offset (Have a look at [parquet-format](https://github.com/apache/parquet-format#file-format) ) of the needs column blocks, and read only the portions needed to be loaded (to RAM / over network ?). 
#TODO group by. That's nice!
Secondly, lets have a look how data reads can be reduced. Parquet allows to store statistics of columns for minimal and maximal values within the metadata. Data pages not matching a filter clause can by that omitted.
For columns with categorical values, checks against min-max-boundaries often do not make too much sense. Noting down every occuring element in a (sorted) dictionary, helps as a prefilter of data pages but comes with the downside of space usage. A good compromise is a Bloom Filter that is part of the format specification (# TODO since when? ). Briefly, a Bloom Filter is an efficient approximation to a set that produces true negatives but no false positives. Nice stuff, too! (See # TODO Link) 


- Bloom Filters
- Data Skipping with Column Statistics
- Reading single columns instead of all
- At which version is it implemented
- Which writer supports it?

## Push-Down predicates in modern Object Storages
Nowadays, data resides mostly on object stores like S3 or Azure's Blobstorage. 
- Filter on values?
- Select columns precisely
- AWS / minio yes
- Azure ??
- 

## Performance Measures implemented in Delta Tables
According to the [Delta Table specifications](https://github.com/delta-io/delta/blob/master/PROTOCOL.md#per-file-statistics), statistics for min and max value can be added to the Delta log. Again, this enables data skipping, here on a more granular way compared to the Parquet implementation. 

- Data Skipping with Statistics
- Data Colocation with Z-Order

## Naive Questions at the end
...