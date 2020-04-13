// Data block
data {
  
  // Dimensions
  int n; // n observations
  int k; // k variables (with intercept)
  int n_1; // number of test samples
  
  // Observed data
  vector[n] y; // vector for prices
  matrix[n, k] X; // matrix of predictors (with intercept)
  matrix[n_1, k] X_new; // matrix of new feature data
  
  // Prior information
  matrix[k, k] B_0; //matrix of prior variances
  vector[k] b_0; // vector of prior means
}

// Parameters for which we want posterior distributions
parameters {
  vector[k] beta; // vector of regression coefficients
  real<lower=0> sigma; // variance
}

// We model only the mean of the posterior, not the variance
transformed parameters {
  vector[n] mu;
  mu = X * beta;
}

// The model we want to estimate
model {
  beta ~ multi_normal(b_0, sigma*B_0); 
  y ~ normal(mu, sigma);
}

// predictions
generated quantities {
  vector[n_1] preds;
  preds = X_new * beta;
}
