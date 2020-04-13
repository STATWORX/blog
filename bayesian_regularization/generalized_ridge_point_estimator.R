
# Generalized Ridge Estimator ---------------------------------------------

gen_ridge <- function(X, y, lambda, mu){

  colnames(X)[1] <- "(Intercept)"
  
  # define dimensions
  n <- nrow(X)
  k <- ncol(X)
  X_t <- t(X)
  mu <- matrix(mu, nrow = k, ncol = 1)
  
  # diagonal matrix with lambdas
  lambda_diag <- diag(
    x = lambda,
    nrow = k,
    ncol = k
  )
  
  # inverted part
  first_part <- solve(
    X_t %*% X + lambda_diag
  )
  
  # remaining part
  second_part <- X_t %*% y + lambda * mu
  colnames(second_part) <- "coefficient"
  
  # Final multiplication
  final_res <- first_part %*% second_part
  
  # return as data.frame
  final_res <- data.frame(
    var_name = rownames(final_res),
    coef = final_res[,'coefficient']
  )
  
  rownames(final_res) <- NULL
  
  return(final_res)
}
