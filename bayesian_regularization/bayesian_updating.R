rm(list = ls())

# packages
library(dplyr)
library(ggplot2)

# prior
alpha <- 1
beta <- 1
n <- 250

# 
k <- seq(0, 1, length.out = 500)
prior <- dbeta(k, alpha, beta)

posteriors <- data.frame(
  x = k,
  y = prior,
  alpha = alpha,
  beta = beta,
  n = n,
  type = "prior"
)

for(i in 1:200){

  share <- runif(1, min = 0, max = 0.6)
  n <- round(runif(1, min=100, max=500))
  r <- round(share*n)
  
  alpha <- alpha + r
  beta <- beta + n - r

  post <- dbeta(k, alpha, beta)
  
  post <- data.frame(
    x = k,
    y = post,
    alpha = alpha,
    beta = beta,
    n = n,
    type = paste("posterior", i)
  )

  posteriors <- rbind(posteriors, post)
}

p <- posteriors %>% 
  dplyr::select(x, y, type) %>%
  ggplot(aes(x = x, y = y, color = type)) +
  geom_line() +
  theme_minimal() + labs(x = "", y = "") +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.text.x = element_blank()) + 
  scale_color_viridis_d(alpha = 0.2) +
  xlim(c(0.15, 0.45))

tiff("titelbild.png", width = 800, height = 800, res = 100)
p
dev.off()
