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
Ad-hoc queries' requirements are different: Value estimates instead of precise values (thinking of Theta sketches) are acceptable in various situations. Also recalcualting a value in case of an outage is supposed to be acceptable if not too often. However, the overall query service downtime should be reduced.

### Data Layout
The data layout shall enable good query performances on different kind of queries. Batch processing, having the nature of a defined process, uses specific queries that data can be optimized towards.

### Scalability
Both types need scalability but in different aspects. The batch process and the backend need to expand with rising amount of data. This is observable and can be remediated if the backend systems can scale. Ad-hoc queries, in addition, have also scalability requirements in terms of number of queries.


## Performance Measures implemented in Parquet
- Bloom Filters
- Data Skipping with Column Statistics
- Reading single columns instead of all
- At which version is it implemented
- Which writer supports it?

## Push-Down predicates in modern Object Storages
- Filter on values?
- Select columns precisely
- AWS / minio yes
- Azure ??
- 

## Performance Measures implemented in Delta Tables
- Data Skipping with Statistics
- Data Colocation with Z-Order

## Naive Questions at the end
...