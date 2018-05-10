# Benchmark of calculations with sparse matrices

In this simulation I benchmark the usage of sparse matrices. I compare the size and the time of the following operations:

- transpose <br/>
`t(X)`
- crossproduct <br/>
`X %*% t(X)`
- addition <br/>
`X + X`
- multiplication <br/>
`X * X`
- switching <br/>
`X[, c(2,3)] <- X[, c(3,2)]`

For more information and details visit our [blog](https://www.statworx.com/de/blog/sparse-matrizen-wann-sollte-man-sie-nutzen/)!



### Results size

<img src="plots/sparse-normal-speichergroesse.png" align="center"/>

### Results time

<img src="plots/sparse-normal-matrixoperation.png" align="center"/>


