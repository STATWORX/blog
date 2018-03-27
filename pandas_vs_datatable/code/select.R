##
##                              SELECT
##

library(data.table)
library(microbenchmark)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)

scenarios <- jsonlite::read_json("output/select.JSON") %>% transpose()

n_cols <- names(scenarios)
n_rows <- gsub(".*_(.*).csv$", "\\1", list.files("data")) %>% unique()

sel_fun <- function(x) {
  return( dt[, .SD, .SDcols = x] )
}

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
    for (scenario in names(scenarios[[n_col]])) {

      cat("Scenario: ", scenario, "\n")

      sel_ind <- scenarios[[n_col]][[scenario]] %>% unlist()
      sel_col <- names(dt)[sel_ind]

      # Get system time in milliseconds
      mb <- microbenchmark(sel_fun(sel_col))
      median_time <- median(mb$time / 1e6) # milliseconds
      mean_time   <- mean(mb$time / 1e6)
      min_time    <- min(mb$time / 1e6)
      max_time    <- max(mb$time / 1e6)

      # Save result
      res_list[[i_glob]] <- list(
        n_col = n_col,
        n_row = n_row,
        scenario = scenario,
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
saveRDS(result, "output/select_results.RDS")
fwrite(result, file = "output/select_results.csv")

# Plot results
library(ggplot2)
result %>%
  mutate(num_sel = gsub("s(\\d).*", "\\1", scenario),
         pos_sel = gsub("^s\\d(.*)$", "\\1", scenario)) %>%
  ggplot(aes(x = as.factor(as.numeric(n_row)), y = as.factor(as.numeric(n_col)), fill = median_time_ms)) +
    geom_tile() +
    labs(x = "# Rows", y = "# Columns") +
    facet_grid(pos_sel ~ num_sel) +
    scale_fill_continuous("Median\nselection time\n(milliseconds)") +
    ggtitle("Data.table median selection time", subtitle = "For number of variables selected and position")

# Save
ggsave("output/select_results.pdf")
