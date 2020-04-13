
# Utils -------------------------------------------------------------------

# Scale function ----------------------------------------------------------
z_scale <- function(x){
  (x - mean(x, na.rm = TRUE) / sd(x, na.rm = TRUE))
}


# Generic wrapper function for generalized ridge --------------------------

ridge_wrapper <- function(X, y, mu, lambda){
  res <- gen_ridge(
    X = X,
    y = y,
    lambda = lambda,
    mu = mu
  )
  res$lambda <- lambda
  return(res)
}


# Lambda ridge iterator ---------------------------------------------------

lambda_iterator <- function(X, y, mu, max_lambda = 100){

  lambdas <- seq(0, max_lambda, 0.5)
  
  purrr::map_dfr(
    .x = lambdas,
    .f = ridge_wrapper,
    X = X,
    y = y,
    mu = mu
  )
}

# Penalty plot ------------------------------------------------------------

penalty_plot <- function(res){
  res %>% 
    ggplot(aes(x = log(lambda), y = coef, color = var_name)) +
    geom_line() + theme_minimal() +
    labs(
      x = "log(Lambda)", 
      y = "Coefficient",
      color = ""
    )
}


# Bayes wrapper -----------------------------------------------------------

bayes_wrapper <- function(df, prior_params, lambdas_inv){
  
  res_bayes_ref <- list()
  
  for(i in 1:length(lambdas_inv)){
    
    message(paste("Start with model", i))
    
    res_bayes_ref[[i]] <- brms_model(
      df = df,
      priors = prior_fun(
        prior_params = prior_params,
        lambda = lambdas_inv[i]
      )
    )
    
    res_bayes_ref[[i]]$lambda <- lambdas_inv[i]
    
    message(paste("Done with model", i))
  }
  
  res_bayes_ref <- bind_rows(res_bayes_ref)
  
  return(res_bayes_ref)
}

