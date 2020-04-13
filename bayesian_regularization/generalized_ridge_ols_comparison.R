
# Comparison of the Generalized Ridge and OLS estimator -------------------
source("generalized_ridge_point_estimator.R")
library(dplyr)

# Example using mtcars ----------------------------------------------------
X <- as.matrix(mtcars[,2:ncol(mtcars)])
y <- mtcars[,1]


# Set penalty to zero to establish comparison with OLS --------------------
lambda <- 0
mu <- rnorm(ncol(X) + 1) #any random vector for mu will do


# Calculate Generalized Ridge Coefficients --------------------------------
gen_ridge_coef <- gen_ridge(
  X = X,
  y = y,
  lambda = lambda,
  mu = mu
)


# Calculate OLS coefficients ----------------------------------------------
ols_coef <- coef(
  lm(
    formula = mpg ~ ., 
    data = mtcars
  )
)


# Compare the two ---------------------------------------------------------
ols_coef <- data.frame(
  coef_ols = ols_coef,
  var_name = names(ols_coef)
)

rownames(ols_coef) <- NULL

comparison <- dplyr::left_join(
  x = gen_ridge_coef,
  y = ols_coef,
  by = "var_name"
)

comparison <- comparison %>% 
  dplyr::mutate(
    abs_diff = round(abs(coef - coef_ols), 1)
  )
