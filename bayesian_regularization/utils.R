
# Utils -------------------------------------------------------------------

# Scale function ----------------------------------------------------------
z_scale <- function(x){
  (x - mean(x, na.rm = TRUE) / sd(x, na.rm = TRUE))
}


# Generic wrapper function for generalized ridge --------------------------

ridge_wrapper <- function(X, y, mu, lambda, X_new, y_new){
  
  res <- gen_ridge(
    X = X,
    y = y,
    lambda = lambda,
    mu = mu
  )
  res$lambda <- lambda
  
  oos_errors <- error_wrapper(
    X_new = X_new, 
    y_new = y_new, 
    res = res,
    lambda = lambda
  )
  
  res$mape <- oos_errors$mape
  res$rmse <- oos_errors$rmse
  res$mae <- oos_errors$mae
  return(res)
}


# Get ridge predictions ---------------------------------------------------

ridge_pred <- function(X, y, res, lambda){
  b <- res[res$lambda == lambda,'coef']
  preds <- as.numeric(X %*% b)
  return(preds)
}


# predict_ridge -----------------------------------------------------------



# Function to calculate oos errors ----------------------------------------

errors <- function(preds, y){
  data.frame(
    rmse = sqrt(mean((preds - y)^2)),
    mae = mean(abs(preds - y)),
    mape = 100 * mean(abs((preds - y)/y))
  ) %>% mutate_all(function(x) round(x, 2))
}


# error wrapper -----------------------------------------------------------

error_wrapper <- function(X_new, y_new, res, lambda){
  errors(
    preds = ridge_pred(
      X = X_new, 
      y = y_new, 
      res = res, 
      lambda = lambda
    ), 
    y = y_new
  )
}

# Lambda ridge iterator ---------------------------------------------------

lambda_iterator <- function(X, y, mu, max_lambda = 100, X_new, y_new){
  lambdas <- seq(0, max_lambda, 0.5)
  purrr::map_dfr(
    .x = lambdas,
    .f = ridge_wrapper,
    X = X,
    y = y,
    mu = mu,
    X_new,
    y_new
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

# mape plot ---------------------------------------------------------------

error_plot_df <- function(res_list, error_col = "mape"){
  
  for(i in 1:length(res_list)){
    res_list[[i]] <- res_list[[i]][,c("lambda", error_col)] %>% 
      dplyr::distinct()
    
    res_list[[i]]$prior <- paste0("prior ", i)
  }
  
  dplyr::bind_rows(res_list)
}

# Bayes wrapper -----------------------------------------------------------

bayes_wrapper <- function(df, prior_params, lambdas_inv){
  
  res_bayes_ref <- list()
  
  for(i in 1:length(lambdas_inv)){
    
    message(paste("Start with model", i))
    
    res_bayes_ref[[i]] <- brms_get_coef(
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

