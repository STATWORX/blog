
# BRMS model --------------------------------------------------------------

brms_model <- function(df, priors){
  
  # run model
  my_model <- brms::brm(
    formula = price~.,
    data = df, family = "normal",
    prior = priors,
    warmup = 1000, iter = 2000, chains = 4,
    control = list(adapt_delta = 0.95)
  )
  
  return(my_model)
}


# Summary function --------------------------------------------------------

brms_summary <- function(model){
  
  # get summary
  post_sum <- brms::posterior_summary(model)
  
  # wrangle estimates into dataframe
  est <- post_sum[,"Estimate"]
  est <- est[which(grepl("b_", names(est)))]
  
  res <- data.frame(
    coef = est,
    var_name = gsub("b_", "", names(est))
  )
  
  rownames(res) <- NULL
  
  return(res)
}

# Get coefficients -----------------------------------------------------------

brms_get_coef <- function(df, priors, lambdas_inv){
  
  model <- brms_model(
    df = df,
    priors = prior_fun(
      prior_params = priors,
      lambda = lambdas_inv
    )
  )
  
  brms_summary(model)
}

# prior function ----------------------------------------------------------

prior_fun <- function(prior_params, lambda){
  priors <- c(
    brms::set_prior(paste0("normal(", prior_params["carat"], "," ,lambda, ")"), coef = "carat"),
    brms::set_prior(paste0("normal(", prior_params["depth"], "," ,lambda, ")"), coef = "depth"),
    brms::set_prior(paste0("normal(", prior_params["table"], "," ,lambda, ")"), coef = "table"),
    brms::set_prior(paste0("normal(", prior_params["x"], "," ,lambda, ")"), coef = "x"),
    brms::set_prior(paste0("normal(", prior_params["y"], "," ,lambda, ")"), coef = "y"),
    brms::set_prior(paste0("normal(", prior_params["z"], "," ,lambda, ")"), coef = "z")
  )
  
  return(priors)
}

