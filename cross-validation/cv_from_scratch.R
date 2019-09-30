## Building Cross-Validation From Scratch in R

rm(list = ls())

## Install and load required packages
# devtools::install_github("andrebleier/Xy")
library(tidyverse)
library(Xy)

## Simulate data
sim <- Xy(n = 1000,
          numvars = c(2,2),
          catvars = 0,
          cor = c(-0.5, 0.9),
          noisevars = 0)

sim_data <- sim$data

## Define error metric
RMSE <- function(f, o){
  sqrt(mean((f - o)^2))
}

## Define k
k <- 5

## Partition the data
set.seed(12345)
sim_data <- mutate(sim_data,
                   my.folds = sample(1:k,
                                     size = nrow(sim_data),
                                     replace = TRUE))

## Train and validate the model
cv.fun <- function(this.fold, data){
  
  train <- filter(data, my.folds != this.fold)
  validate <- filter(data, my.folds == this.fold)
  
  model <- lm(y ~ NLIN_1 + NLIN_2 + LIN_1 + LIN_2,
              data = train)
  
  pred <- predict(model, newdata = validate) %>% as.vector()
  
  this.rmse <- RMSE(f = pred, o = validate$y)
  
  return(this.rmse)
}

## Iterate through each fold
cv.error <- sapply(seq_len(k),
                   FUN = cv.fun,
                   data = sim_data) %>%
  mean()

## Return the cross-validation error
cv.error
