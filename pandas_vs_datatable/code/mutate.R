##
##                           MUTATE
##

library(data.table)
library(microbenchmark)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)

# Functions
source("code/functions.R")

scenarios <- jsonlite::read_json("output/mutate.JSON")

n_cols <- names(scenarios)
n_rows <- gsub(".*_(.*).csv$", "\\1", list.files("data")) %>% unique()

#gb_label <- c("zip", "pois", "unif")
mutate_label <- c("INT", "DBL", "STR", "LGL")

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
    
    m_cols <- scenarios[[n_col]][["mutate"]] %>% unlist(.)
    
    # W/O group_by
    for (i in seq(m_cols)) {
      
      cat("Szenario:", i, "\n")
      
      m_col <- m_cols[i]
      
      # Get system time in milliseconds
      mb <- switch(i,
        "1" = microbenchmark(dt[, result := get(m_col) + 1], times = 100L),
        "2" = microbenchmark(dt[, result := get(m_col) * 2], times = 100L),
        "3" = microbenchmark(dt[, result := paste0(get(m_col), "a")], times = 100L),
        "4" = microbenchmark(dt[, result := !get(m_col)], times = 100L),
        NULL
      )
      
      median_time <- median(mb$time / 1e6) # milliseconds
      mean_time   <- mean(mb$time / 1e6)
      min_time    <- min(mb$time / 1e6)
      max_time    <- max(mb$time / 1e6)
      
      # Save result
      res_list[[i_glob]] <- list(
        n_col = n_col,
        n_row = n_row,
        scenario = sprintf("mutate_%s", mutate_label[i]),
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
saveRDS(result, "output/filter_results.RDS")
fwrite(result, file = "output/filter_results.csv")

# Plot results
library(ggplot2)
result %>%
  ggplot(aes(x = as.factor(as.numeric(n_row)), y = as.factor(as.numeric(n_col)), fill = median_time_ms)) +
    geom_tile() +
    geom_text(aes(label = round(median_time_ms, 1)), col = "white") +
    labs(x = "# Rows", y = "# Columns") +
    facet_wrap( ~ scenario) +
    scale_fill_continuous("Median\nmutate time\n(milliseconds)") +
    ggtitle("Data.table median mutate time", subtitle = "For 4 scenarios (100 runs each")

# Save
ggsave("output/filter_results_datatable.pdf")
