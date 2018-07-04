#######################################################
#------ ---# Food for Regression #---------------#
#-----------------------------------------------------#
# Kunde:    Blog 
# Projekt:  ---
# Datum:    03.07.2018
#
#----------------- CHANGE HISTORY --------------------#
# ---
#######################################################


# Libraries
library(tidyverse)


# 1. Setup data------------------------------
# -------------------------------------------

# 1.1 Setup data.frame-----------------------
start <- as.Date("2013-05-01") # Not the real date
end <- as.Date("2018-06-22")
dt <- data.frame(date = seq.Date(start, end, by = "day")) 

# Weekdays
dt$weekday <- weekdays.Date(dt$date)  

# Take out sundays
dt <- dt[dt$weekday != "Sonntag", ]

# 1.2 Define prices ------------------------------
obs <- nrow(dt)

# Price range
pricerange <- seq(3.19, 10.99, 0.10)

# Random pricing
set.seed(1234)
dt$prices <- sample(pricerange, obs, replace = TRUE)

# 1.3 Define demand ------------------------------
# Setup up multiplicative function
multiply_demand <- function(a, b, price) {
  demand <-a*(price^-b)
  demand 
}

# Calculate demand dependent on price
set.seed(1246)
dt$demand_mult <- round(multiply_demand(a = 2500, b = 1.5, price = dt$prices), 0) +
  runif(obs, min = -20, max = 20)

# Take out negative sales
dt$demand_mult <- ifelse(dt$demand_mult < 0, 1, dt$demand_mult)

# 1.4 Define costs ------------------------------
# Setup cost function
calculate_cost <- function(fix, var, demand) {
  total.cost <- fix + var*demand 
  total.cost
  
}

# Define costs
var <- 2.5
fix <- 300
dt$costs <- calculate_cost(fix = fix, var = var, demand = dt$demand_mult)

# 1.5 Other Kpis ------------------------------
dt$turnover <- dt$demand_mult*dt$prices
dt$return <- dt$turnover - dt$costs

# 2. Models----------------------------------
# -------------------------------------------

# 2.1 log-log-----------------------------
loglog <- lm(log(demand_mult) ~ log(prices), data = dt) 
summary(loglog)
dt$demand_loglog <- exp(predict(loglog))

# 2.2 level-level-----------------------------
levlev <- lm(demand_mult ~ prices, data = dt) 
summary(levlev)
dt$demand_levlev <- predict(levlev)

mean(coef(levlev)[2]*(dt$prices/dt$demand_mult))

# 2.3 log-level-----------------------------
loglev <- lm(log(demand_mult) ~ prices, data = dt) 
summary(loglev) 
dt$demand_loglev <- exp(predict(loglev))

mean(coef(loglev)[2]*dt$prices)

# 2.4 level-log-----------------------------
levlog <- lm(demand_mult ~ log(prices), data = dt) 
summary(levlog)
dt$demand_levlog <- predict(levlog)

mean(coef(levlog)[2]/(dt$demand_mult))

# 2.5 Graphical presentation of models------
#png(filename="elasticity_regression_comp.png")
ggplot(dt) + geom_point(aes(prices, demand_mult), alpha = 0.1) + 
  ylab("Price") + xlab("Sold Salads") +
  geom_line(aes(prices, demand_loglog, linetype = "loglog")) +
  geom_line(aes(prices, demand_levlev, linetype = "levlev")) +
  geom_line(aes(prices, demand_loglev, linetype = "loglev")) +
  geom_line(aes(prices, demand_levlog, linetype = "levlog")) +
  scale_linetype_discrete(name = "Model") +
  theme_minimal()
#dev.off()

# 3 Optimization--------------------------------------
# ------------------------------------------------------

# 3.1 Extract coefficents  ------------------------------
ela <- as.numeric(coef(loglog)[2])
ela_lower <- ela+summary(loglog)$coefficients[2,2]*1.96
ela_upper <- ela-summary(loglog)$coefficients[2,2]*1.96

base <- as.numeric(coef(loglog)[1])
base

# 3.2. Calculate optimizes price range  -----------------
optimized_price <-  (ela*var)/(1+(ela))
optimized_price_lower <- (ela_lower*var)/(1+(ela_lower))
optimized_price_upper <- (ela_upper*var)/(1+(ela_upper))

# 3.3. Caculate demand  ------------------------------
assumed_demand <- multiply_demand(a = exp(base), b = abs(ela), price = optimized_price) 
assumed_demand_lower <- multiply_demand(a = exp(base), b = abs(ela_lower), price = optimized_price_lower) 
assumed_demand_upper <- multiply_demand(a = exp(base), b = abs(ela_upper), price = optimized_price_upper)

# 3.4 Caculate turnover  ------------------------------
assumed_turnover <- assumed_demand*optimized_price
assumed_turnover_lower <- assumed_demand_lower*optimized_price_lower
assumed_turnover_upper <- assumed_demand_upper*optimized_price_upper

# 3.5 Caculate cost  ------------------------------
assumed_cost <- calculate_cost(fix = fix, var = var, demand = assumed_demand)
assumed_cost_lower <- calculate_cost(fix = fix, var = var, demand = assumed_demand_lower)
assumed_cost_upper <- calculate_cost(fix = fix, var = var, demand = assumed_demand_upper)

# 3.6. Caculate profit  ------------------------------
assumed_profit <- assumed_turnover - assumed_cost
assumed_profit_lower <- assumed_turnover_lower - assumed_cost_lower
assumed_profit_upper <- assumed_turnover_upper - assumed_cost_upper


# 4 Optimization: Adjusted SE---------------------------
# ------------------------------------------------------
se <- 0.15

# 4.1 Extract coefficents  ------------------------------
ela <- as.numeric(coef(loglog)[2])
ela_lower <- ela+se*1.96
ela_upper <- ela-se*1.96

base <- as.numeric(coef(loglog)[1])
base

# 4.2. Calculate optimizes price range  -----------------
optimized_price <-  (ela*var)/(1+(ela))
optimized_price_lower <- (ela_lower*var)/(1+(ela_lower))
optimized_price_upper <- (ela_upper*var)/(1+(ela_upper))

# 4.3. Caculate demand  ------------------------------
assumed_demand <- multiply_demand(a = exp(base), b = abs(ela), price = optimized_price) 
assumed_demand_lower <- multiply_demand(a = exp(base), b = abs(ela_lower), price = optimized_price_lower) 
assumed_demand_upper <- multiply_demand(a = exp(base), b = abs(ela_upper), price = optimized_price_upper)

# 4.4 Caculate turnover  ------------------------------
assumed_turnover <- assumed_demand*optimized_price
assumed_turnover_lower <- assumed_demand_lower*optimized_price_lower
assumed_turnover_upper <- assumed_demand_upper*optimized_price_upper

# 4.5 Caculate cost  ------------------------------
assumed_cost <- calculate_cost(fix = fix, var = var, demand = assumed_demand)
assumed_cost_lower <- calculate_cost(fix = fix, var = var, demand = assumed_demand_lower)
assumed_cost_upper <- calculate_cost(fix = fix, var = var, demand = assumed_demand_upper)

# 4.6. Caculate profit  ------------------------------
assumed_profit <- assumed_turnover - assumed_cost
assumed_profit_lower <- assumed_turnover_lower - assumed_cost_lower
assumed_profit_upper <- assumed_turnover_upper - assumed_cost_upper

