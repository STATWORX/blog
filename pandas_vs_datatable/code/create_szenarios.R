### CREATE SZENARIOS ###

library(dplyr)
library(data.table)
library(jsonlite)
library(purrr)

source("code/functions.R")

# SELECT: 7 SZENARIOS =========================================================

n_cols      <- c(12, 42, 120, 420, 1200)
szenarios   <- c("s1beg", "s1mid", "s1end", "s3beg", "s3mid", "s3end", "s3any")
select_list <- list()

set.seed(1L)

for ( szenario in szenarios ) {

  # Initiate sublist
  select_list[[szenario]] <- list()

  n_draws <- gsub("s(\\d).*", "\\1", szenario) %>% as.integer()
  pos     <- gsub("s\\d(.*)", "\\1", szenario)

  for ( n_col in n_cols ) {

    select_list[[szenario]][[as.character(n_col)]] <- draw_ind_for_select(n_col, n_draws, pos)

  }

}

# Save result
write(toJSON(select_list), file = "output/select.JSON")

# FILTER: 7 SZENARIOS =========================================================

n_cols      <- c(12, 42, 120, 420, 1200)
szenarios   <- c("INT", "DBL", "STR", "LGL")
filter_list <- list()

set.seed(1L)

for ( n_col in n_cols ) {

  input <- data.table::fread(paste0("data/sim_data_", n_col, "_500.csv"))
  cols  <- names(input)

  sel_cols <- map_chr(szenarios, ~ cols %>% grep(.x, ., value = TRUE) %>%
    sample(., 1))
  sel_cond <- input[sample(.N, 1), .SD, .SDcols = sel_cols] %>% as.list()

  filter_list[[as.character(n_col)]] <- sel_cond

}

# Save result
write(toJSON(filter_list), file = "output/filter.JSON")

# ARRANGE: 5 SZENARIOS =========================================================

# Arrange by what is given in the szenarios. ANY2 arranges by two selected
# columns

n_cols       <- c(12, 42, 120, 420, 1200)
szenarios    <- c("INT", "DBL", "STR", "LGL", "ANY2")
arrange_list <- list()

set.seed(1L)

for ( n_col in n_cols ) {

  # Data
  input <- data.table::fread(paste0("data/sim_data_", n_col, "_500.csv"))
  cols  <- names(input)

  # Add named Szenarios to sublist
  arrange_list[[as.character(n_col)]] <- map(szenarios, function(szenario) {
    out <- szenario %>% grep(., cols, value = TRUE)
    out <- if (!length(out)) sample(cols, 2) else sample(out, 1)
    return( out )
  }) %>% setNames(., szenarios)

}

# Save result
write(toJSON(arrange_list), file = "output/arrange.JSON")

# MUTATE: 12 SZENARIOS ========================================================

# INT: add 1 | DBL: multiply by 2 | STR: concatenate "a" | LGL: negate

n_cols       <- c(12, 42, 120, 420, 1200)
mutate_cols  <- c("INT", "DBL", "STR", "LGL")
groupby_cols <- c("zipois", "pois", "unif") %>% sprintf("INT_[0-9]+_%s", .)
mutate_list  <- list()

set.seed(1L)

for ( n_col in n_cols ) {

  input <- data.table::fread(paste0("data/sim_data_", n_col, "_500.csv"))
  cols  <- names(input)

  sel_mutate <- map_chr(mutate_cols, ~ cols %>% grep(.x, ., value = TRUE) %>%
    sample(., 1))
  sel_groupby <- map_chr(groupby_cols, ~ cols %>% grep(.x, ., value = TRUE) %>%
    sample(., 1))

  mutate_list[[as.character(n_col)]] <- list(
    mutate = sel_mutate,
    group_by = sel_groupby
  )

}

# Save results
write(toJSON(mutate_list), file = "output/mutate.JSON")

# SUMMARISE: 12 SZENARIOS ======================================================

# INT = STR = LGL: Count instances
# DBL: median, mean, and sd

n_cols       <- c(12, 42, 120, 420, 1200)
sum_cols  <- c("INT", "DBL", "STR", "LGL")
groupby_cols <- c("zipois", "pois", "unif") %>% sprintf("INT_[0-9]+_%s", .)
sum_list  <- list()

set.seed(1L)

for ( n_col in n_cols ) {

  input <- data.table::fread(paste0("data/sim_data_", n_col, "_500.csv"))
  cols  <- names(input)

  sel_sum <- map_chr(sum_cols, ~ cols %>% grep(.x, ., value = TRUE) %>%
    sample(., 1))
  sel_groupby <- map_chr(groupby_cols, ~ cols %>% grep(.x, ., value = TRUE) %>%
    sample(., 1))

  sum_list[[as.character(n_col)]] <- list(
    summarise = sel_sum,
    group_by = sel_groupby
  )

}

write(toJSON(sum_list), file = "output/summarise.JSON")
