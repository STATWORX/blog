##
##                           ARRANGE
##

library(data.table)
library(microbenchmark)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)

# Functions
source("code/functions.R")

scenarios <- jsonlite::read_json("output/arrange.JSON")

n_cols <- names(scenarios)
n_rows <- gsub(".*_(.*).csv$", "\\1", list.files("data")) %>% unique()

res_list <- list()
i_glob   <- 1

# Colums of data
for (n_col in n_cols) {

  # Rows of data
  for (n_row in n_rows) {
    
    cat("=== Data with ", n_col, " columns and ", n_row, " rows ===\n")

    cat("File load ... \n")
    filep <- sprintf("data/sim_data_%s_%s.csv", n_col, n_row)
    dt    <- fread(filep)

    # Scenarios
    for (i in seq(scenarios[[n_col]])) {
      
      cat("Scenario: ", i, "\n")

      arrange_szenario <- names(scenarios[[n_col]])[i]
      arrange_cols     <- scenarios[[n_col]][[i]] %>% unlist()

      # Get system time in milliseconds
      mb <- microbenchmark(setorderv(dt, cols = arrange_cols), times = 100L)
      median_time <- median(mb$time / 1e6) # milliseconds
      mean_time   <- mean(mb$time / 1e6)
      min_time    <- min(mb$time / 1e6)
      max_time    <- max(mb$time / 1e6)

      # Save result
      res_list[[i_glob]] <- list(
        n_col = n_col,
        n_row = n_row,
        scenario = arrange_szenario,
        min_time = min_time,
        mean_time_ms = mean_time,
        median_time_ms = median_time,
        max_time = max_time
      )
      i_glob <- i_glob + 1
    }
  }
}

result <- res_list %>% dplyr::bind_rows()
saveRDS(result, "output/arrange_results.RDS")
fwrite(result, file = "output/arrange_results.csv")

result %>% arrange(n_col, n_row, scenario) %>% View()

# Plot results
library(ggplot2)
result %>%
  ggplot(aes(x = as.factor(as.numeric(n_row)), y = as.factor(as.numeric(n_col)), fill = median_time_ms)) +
    geom_tile() +
    geom_text(aes(label = round(median_time_ms, 1)), col = "white") +
    labs(x = "# Rows", y = "# Columns") +
    facet_wrap( ~ scenario) +
    scale_fill_continuous("Median\narrange time\n(milliseconds)") +
    ggtitle("Data.table median arrange time", subtitle = "For 5 scenarios (100 runs each")

# Save
ggsave("output/arrange_results_datatable.pdf")
