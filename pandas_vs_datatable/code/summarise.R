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

scenarios <- jsonlite::read_json("output/summarise.JSON")

n_cols <- names(scenarios)
n_rows <- gsub(".*_(.*).csv$", "\\1", list.files("data")) %>% unique()

s_label <- c("INT", "DBL", "STR", "LGL")
g_label <- c("zip", "pois", "unif")

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

    s_cols <- scenarios[[n_col]][["summarise"]] %>% unlist(.)

    # W/O group_by
    for (i in seq(s_cols)) {

      cat("Szenario without group_by:", i, " ")

      s_col <- s_cols[i]

      # Get system time in milliseconds
      mb <- switch(i,
        "1" = microbenchmark(dt[, .N, by = s_col], times = 100L),
        "2" = microbenchmark(dt[, .(mean = mean(get(s_col)), 
                                    median = median(get(s_col)), 
                                    sd = sd(get(s_col)))]
                             , times = 100L),
        "3" = microbenchmark(dt[, .N, by = s_col], times = 100L),
        "4" = microbenchmark(dt[, .N, by = s_col], times = 100L),
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
        scenario = sprintf("summarise_%s", s_label[i]),
        min_time = min_time,
        mean_time_ms = mean_time,
        median_time_ms = median_time,
        max_time = max_time
      )
      i_glob <- i_glob + 1
      
      cat("DONE\n")
    }
    
    # With group_by
    g_cols <- scenarios[[n_col]][["group_by"]] %>% unlist(.)
    for (j in seq(g_cols)) {
      
      g_col <- g_cols[j]
      
      for (i in seq(s_cols)) {
        
        cat("Scenario with group by", g_col, i, " ")
        
        s_col <- s_cols[i]
        
        # Get system time in milliseconds
        mb <- switch(i,
          "1" = microbenchmark(dt[, .N, by = c(s_col, g_col)], times = 100L),
          "2" = microbenchmark(dt[, .(mean = mean(get(s_col)), 
                                      median = median(get(s_col)), 
                                      sd = sd(get(s_col)))
                                  , by = g_col]
                               , times = 100L),
          "3" = microbenchmark(dt[, .N, by = c(s_col, g_col)], times = 100L),
          "4" = microbenchmark(dt[, .N, by = c(s_col, g_col)], times = 100L),
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
          scenario = sprintf("summarise_%s_by_%s", s_label[i], g_label[j]),
          min_time = min_time,
          mean_time_ms = mean_time,
          median_time_ms = median_time,
          max_time = max_time
        )
        i_glob <- i_glob + 1
        
        cat("DONE\n")
      }
      
    }
    
  }
}

result <- res_list %>% dplyr::bind_rows()
saveRDS(result, "output/summarise_results.RDS")
fwrite(result, file = "output/summarise_results.csv")

# Plot results
library(ggplot2)
result %>%
  ggplot(aes(x = as.factor(as.numeric(n_row)), y = as.factor(as.numeric(n_col)), fill = median_time_ms)) +
    geom_tile() +
    geom_text(aes(label = round(median_time_ms, 1)), col = "white") +
    labs(x = "# Rows", y = "# Columns") +
    facet_wrap( ~ scenario) +
    scale_fill_continuous("Median\nsummarise time\n(milliseconds)") +
    ggtitle("Data.table median summarise time", subtitle = "For 12 scenarios (100 runs each")

# Save
ggsave("output/summarise_results_datatable.pdf")
