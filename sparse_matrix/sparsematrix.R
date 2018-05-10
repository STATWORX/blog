
# settings ----------------------------------------------------------------
#rm(list = ls())

library(data.table)
library(ggplot2)
library(Matrix)
library(microbenchmark)

path <- "sparse_matrix/plots/"

# functions ---------------------------------------------------------------

statusbar <- function (run = numeric(),
                       max.run = numeric(),
                       percent.max = 20L)
{
  percent      <- run / max.run
  percent.step <- round(percent * percent.max, 0)
  statusbar <- paste0("[",
                      paste0(rep("=", percent.step), collapse = ""),
                      paste0(rep(" ", percent.max - percent.step), collapse = ""),
                      "] ",
                      sprintf("%7.2f", percent * 100, 2),
                      "%")
  cat("\r", statusbar)
  flush.console()
}

# matrix operations ----
X_transpose <- function(X) {
  t(X)
  return(NULL)}
X_square <- function(X) {
  X %*% t(X)
  return(NULL)}
X_add <- function(X) {
  X + X
  return(NULL)}
X_multiply <- function(X) {
  X * X
  return(NULL)}
X_replace <- function(X) {
  X[, c(2,3)] <- X[, c(3,2)]
  return(NULL)}

S_transpose <- function(S) {
  t(S)
  return(NULL)}
S_square <- function(S) {
  S %*% t(S)
  return(NULL)}
S_add <- function(S) {
  S + S
  return(NULL)}
S_multiply <- function(S) {
  S * S
  return(NULL)}
S_replace <- function(S) {
  S[, c(2,3)] <- S[, c(3,2)]
  return(NULL)}



# data --------------------------------------------------------------------


DT <- as.data.table(expand.grid(n = seq(100, 2500, 400),
                                dichte = c(0.01, 0.2, 0.4, 0.6, 0.8, 1)))
DT <- DT[order(n)]

# create matrix -----------------------------------------------------------

all.numbers <- round(runif(DT[, max(n)]^2),4)

# this part takes a lot of time!
# It took me around 10 hours.

for (i.row in 1:dim(DT)[1]) {
  # i.row <- 1
  set.seed(i.row)
  n      <- DT[i.row, n]
  dichte <- DT[i.row, dichte]
  
  # normal Matrix
  numbers <- all.numbers[1:(n^2)]
  set_zero <- sample(n*n, ceiling(n*n*dichte), replace = FALSE)
  numbers[set_zero] <- 0
  
  if (exists("X") |exists("S") ) {
    rm(X, S)
  }
  tmp <- NULL
  X <- matrix(numbers, nrow = n)
  # sparse matrix
  S <- Matrix(numbers, nrow = n, sparse = TRUE)
  
  # matrix operations benchmark
  tmp <- microbenchmark(X_transpose(X),
                        X_square(X),
                        X_add(X),
                        X_multiply(X),
                        X_replace(X),
                        S_transpose(S),
                        S_square(S),
                        S_add(S),
                        S_multiply(S),
                        S_replace(S),
                        times = 100L, unit = "ns")
  
  
  tmp <- data.table(summary(tmp))
  
  DT[i.row, c(as.character(tmp$expr)) := as.list(tmp$median)]
  
  # Size ----
  DT[i.row, c("SIZE_normal", "SIZE_sparse") := 
       list(object.size(X), object.size(S)) ]
  statusbar(run = i.row, max.run = dim(DT)[1])
}

# save results
saveRDS(DT, "results.rds")

# plots -------------------------------------------------------------------

# Size ----
plot.data <-  melt.data.table(DT,
                              id = 1:2,
                              measure=patterns("SIZE_"), 
                              variable.name = "Art",
                              value.name = "Size")
plot.data[, Size := as.numeric(Size) /1024/1024]
plot.data[, Dichte := as.factor(dichte)]
plot.data[, Art := as.factor(Art)]

plot.size <- 
  ggplot(plot.data, aes(x = n, y = Size, linetype = Art, color = Dichte)) +
  geom_line() +
  xlab("Spaltenanzahl") + 
  ylab("Speichergröße (Mb)")

ggsave(plot.size, filename = paste0(path, "sparse-normal-speichergroesse.png"))

# Time ----
plot.data <-  melt.data.table(DT,
                              id = 1:2,
                              #measure.vars =  3:12,
                              measure = patterns("X_", "S_"), 
                              variable.name = "Function",
                              value.name = "Time")
plot.data[, Function := factor(Function, 
                               levels = 1:5,
                               labels = c("Transponieren",
                                          "Multiplikation (Matrix)", "Addition",
                                          "Multiplikation (Zellenweise)",
                                          "Ersetzen"))]

setnames(plot.data, c("Time1", "Time2"), c("normal", "sparse"))
plot.data <-  melt.data.table(plot.data,
                              id = 1:3,
                              variable.name = "Art",
                              value.name = "Time")

plot.data[, Dichte := as.factor(dichte)]
plot.data[, Art := as.factor(Art)]
plot.data[, Time := as.numeric(Time)]
plot.data[, Zeit := Time/1e9]

plot.time <-
  ggplot(plot.data, aes(x = n, y = Zeit, linetype = Art, color = Dichte)) +
  geom_line() + 
  xlab("Spaltenanzahl") + 
  ylab("Zeit (s)") + 
  facet_wrap(~Function, ncol = 1, scales = "free")

ggsave(plot.time, filename = paste0(path, "sparse-normal-matrixoperation.png"))
