#######################################################
#------ ---# Food for Regression 2#---------------#
#-----------------------------------------------------#
# Kunde:    Blog 
# Projekt:  ---
# Datum:    18.09.2018
#
#----------------- CHANGE HISTORY --------------------#
# ---
#######################################################


# Libraries
library(tidyverse)
library(zoo)
library(gridExtra)
library(grid)

# 1. Setup data------------------------------
# -------------------------------------------

# 1.1 Setup data.frame-----------------------
start <- as.Date("2013-04-28") # Not the real date
end <- as.Date("2018-06-23")
dt <- data.frame(date = seq.Date(start, end, by = "day")) 

# Weekdays
dt$weekday <- weekdays.Date(dt$date)  

# Take out sundays
dt <- dt[dt$weekday != "Sonntag", ]

# 1.2 Define prices ------------------------------
obs <- 75

# Price range
pricerange <- seq(6.79, 10.99, 0.10)

# Random pricing
set.seed(1234)
prices <- sample(pricerange, obs, replace = TRUE)

# Add time
len <- rep(6, obs)

# Add promo data 
p_dt <- data.frame(prices,  len)
p_dt$promo <- ifelse(p_dt$prices <= 7.49, 1, 0)

# Generate Pricing Periods
days_used <- sum(p_dt$len)

set.seed(1234)
while(days_used < nrow(dt)) {
  i <- sample(1:nrow(p_dt), 1)
  if((1614/6) > 10) {
    m <- sample(seq(1, 10, 1), 1)
  } else {
    m <- sample(seq(1, (1614/6), 1), 1)
  }
  if(p_dt$promo[i] == 1) {
      p_dt$len[i] = p_dt$len[i]
  } else {
      p_dt$len[i] = p_dt$len[i] + m*6
  }
  days_used <- sum(p_dt$len)
}

# Add prices to dt
price <- rep(p_dt$prices, p_dt$len)[1:1614]
dt$price <- price

# Add promo based on pricing
dt$promo <- ifelse(dt$price <= 7.49, 1, 0)

# Add some price independet promotions
set.seed(1234)
other_promos <- sample(1:1614, 20)
dt$promo[other_promos] <- 1


# 1.3 Define demand based on product prices ------------------------------
# Setup up multiplicative function
multiply_demand <- function(a, b, price) {
  demand <-a*(price^-b)
  demand 
}

# Calculate demand dependent on price
set.seed(1246)
dt$demand_mult <- round(multiply_demand(a = 2500, b = 1.5, price = dt$price) , 0)

# Take out negative sales
dt$demand_mult <- ifelse(dt$demand_mult < 0, 1, dt$demand_mult)

# 1.4 Define demand based on promotion ------------------------------
#Add demannd with promo
dt$demand_mult_promo <- ifelse(dt$promo == 1, round(dt$demand_mult * 1.5, 0), dt$demand_mult)

# 1.5 Define demand based on competitor prices -----------------------
obs_alt <- 60

# Price range
pricerange_alt <- seq(4.79, 7.99, 0.10)

# Random pricing
set.seed(14)
prices_alt <- sample(pricerange_alt, obs_alt, replace = TRUE)

# Add time
len_alt <- rep(6, obs_alt)

# Generate Pricing Periods
p_alt_dt <- data.frame(prices_alt,  len_alt)
days_alt_used <- sum(p_alt_dt$len)

set.seed(1234)
while(days_alt_used < nrow(dt)) {
  i <- sample(1:nrow(p_alt_dt), 1)
  if((1614/6) > 10) {
    m <- sample(seq(1, 10, 1), 1)
  } else {
    m <- sample(seq(1, (1614/6), 1), 1)
  }
  p_alt_dt$len[i] = p_alt_dt$len[i] + m*6
  days_alt_used <- sum(p_alt_dt$len)
}

# Add prices to dt
price_alt <- rep(p_alt_dt$prices, p_alt_dt$len)[1:1614]
dt$price_alt <- price_alt

# cClculate price changes
dt$cross_ela_factor <- (dt$price_alt-lag(dt$price_alt))/lag(dt$price_alt)
dt$cross_ela_factor <- ifelse(dt$cross_ela_factor == 0, NA, dt$cross_ela_factor)
dt$cross_ela_factor[1] <- 0
dt$cross_ela_factor <- na.locf(dt$cross_ela_factor, fromLast = FALSE) 

# Adjust demand
dt$demand_mult_promo_cross <- round(((dt$cross_ela_factor)+1)*dt$demand_mult_promo, 0)

# 1.6 Add a little bit of randomness -----------------------
dt$demand_mult_promo_cross = round(dt$demand_mult_promo_cross + runif(nrow(dt), min = -10, max = 10),0)

# 1.8 Define costs ------------------------------
# Setup cost function
calculate_cost <- function(fix, var, demand) {
  total.cost <- fix + var*demand 
  total.cost
  
}

# Define costs
var <- 2.5
fix <- 300
dt$cost <- calculate_cost(fix = fix, var = var, demand = dt$demand_mult_promo_cross)

# 1.9 Other Kpis ------------------------------
dt$turnover <- dt$demand_mult_promo_cross*dt$price
dt$return <- dt$turnover - dt$cost

# 2. Models----------------------------------
# -------------------------------------------

# 2.1 log-log simple-----------------------------
loglog <- lm(log(demand_mult_promo_cross) ~ log(price), data = dt) 
summary(loglog)
dt$demand_loglog <- exp(predict(loglog))

# 2.2 log-log with controls-----------------------------
loglog <- lm(log(demand_mult_promo_cross) ~ log(price) + promo + log(price_alt), data = dt) 
summary(loglog)
dt$demand_loglog <- exp(predict(loglog))

# 3. Grpahs----------------------------------
# -------------------------------------------

# 3.1 Graph price and demand ----------------

# Scatter of demand
g.top <- ggplot(dt) + 
  geom_point(aes(date, demand_mult_promo_cross), alpha = 0.5) + 
  theme_minimal() +
  theme(plot.margin = unit(c(1,1,-20,1),units="points"),
        axis.title.y = element_text(vjust =0.25),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("Sales") + xlab("") +
  geom_hline(yintercept = 0, alpha = 0.5)


# Line plot prices
dt_prices <- data.frame(date = dt$date, price = dt$price, label = "Our Saladbar")
dt_prices2 <- data.frame(date = dt$date, price = dt$price_alt, label = "Main Competitor")
dt_prices <- rbind(dt_prices, dt_prices2)

g.bottom <- ggplot(dt_prices[dt_prices$label == "Our Saladbar", ]) +
  geom_line(aes(date, price, colour = label))  +
  theme_minimal() +
  theme(plot.margin = unit(c(1,1,1,6),units="points"),
        legend.position = c(0.2, 0.15),
        legend.title=element_blank()) +
  ylab("Price") + xlab("Years")
grid.arrange(g.top, g.bottom, heights = c(2/6, 4/6))


# 3.2 Graph with all variables----------------
promo_dates <- dt$date[dt$promo == 1]

# Scatter of demand  
g.top <- ggplot(dt) + 
  geom_vline(xintercept = promo_dates, alpha = 0.10, colour = "blue") +
  geom_point(aes(date, demand_mult_promo_cross), alpha = 0.5) + 
  geom_smooth(aes(date, demand_mult_promo_cross)) +
  theme_minimal() +
  theme(plot.margin = unit(c(1,1,-20,1),units="points"),
        axis.title.y = element_text(vjust =0.25),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("Sales") + xlab("") +
  geom_hline(yintercept = 0, alpha = 0.5)

# Line plot prices
dt_prices <- data.frame(date = dt$date, price = dt$price, label = "Our Saladbar")
dt_prices2 <- data.frame(date = dt$date, price = dt$price_alt, label = "Main Competitor")
dt_prices <- rbind(dt_prices, dt_prices2)

g.bottom <- ggplot(dt_prices) +
  geom_vline(xintercept = promo_dates, alpha = 0.10, colour = "blue") +
  geom_line(aes(date, price, colour = label))  +
  theme_minimal() +
  theme(plot.margin = unit(c(1,1,1,6),units="points"),
        legend.position = c(0.2, 0.15),
        legend.title=element_blank()) +
  ylab("Price") + xlab("Years")
grid.arrange(g.top, g.bottom, heights = c(2/6, 4/6))




