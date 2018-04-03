### CREATE DATASETS ###

rm(list = ls())

library(VGAM)
library(dplyr)
library(data.table)

folder <- "data"
if (!file.exists(folder)) dir.create(folder)
setwd(folder)

# Initialize parameters
N_COLS_MAX <- 1200
N_ROWS_MAX <- 100000
N_COLS_SUB <- c(1200, 420, 120, 42, 12)
N_ROWS_SUB <- c(500, 5000, 50000, 100000)
grid <- expand.grid(cols = N_COLS_SUB, rows = N_ROWS_SUB)


# Set URL ;)
url <- "https://youtu.be/32FB-gYr49Y?t=34s"

# Start process
cat(paste0("Start with ", N_ROWS_MAX, " rows\n"))

# Initialize counts
j_int <- 1
j_dbl <- 1
j_lgl <- 1
j_str <- 1

set.seed(12345L)

# Initialize Data
df <- data.frame(x = 1:N_ROWS_MAX) %>% mutate(x = NULL)

# Generate max column max row data
for ( i in 1:N_COLS_MAX ) {

  cat("Column: ", i)

  ii <- ifelse(i %% 4 == 0, 4, i %% 4)

  if ( ii == 1 ) {

    cat(" | Integer\n")

    # INTEGER case
    jj   <- ifelse(j_int %% 3 == 0, 3, j_int %% 3)
    args <- switch(jj,
      # ZIPois
      "1" = list(n      = N_ROWS_MAX,
                 lambda = sample(1:6, 1),
                 pstr0  = round(runif(1, 0.3, 0.6), 2)),
      # POIS
      "2" = list(n      = N_ROWS_MAX,
                 lambda = sample(10:20, 1)),
      # DISCRETE UNIF
      "3" = list(size    = N_ROWS_MAX,
                 x       = 1:sample(10:20, 1),
                 replace = TRUE),
      NULL
    )
    fun      <- switch(jj, "1" = rzipois, "2" = rpois, "3" = base::sample)
    fun_name <- switch(jj, "1" = "zipois", "2" = "pois", "3" = "unif")
    col_name <- paste("INT", j_int, fun_name, paste(args[-1], collapse = "_"),
                      sep = "_")

    # Add to Data Frame
    df[[col_name]] <- do.call(what = fun, args)

    j_int <- j_int + 1

  } else if ( ii == 2 ) {

    cat(" | Double\n")

    # DOUBLE case
    jj   <- ifelse(j_dbl %% 3 == 0, 3, j_dbl %% 3)
    args <- switch(jj,
      # UNIF
      "1" = list(n   = N_ROWS_MAX,
                 min = 0,
                 max = floor(runif(1, 1e5, 1e6))),
      # NORMAL
      "2" = list(n    = N_ROWS_MAX,
                 mean = floor(runif(1, 1e5, 1e6)),
                 sd   = floor(runif(1, 1e5, 1e6) / 10)),
      # EXPONENTIAL
      "3" = list(n    = N_ROWS_MAX,
                 rate = runif(1, 1e-5, 1e-4)),
      NULL
    )
    fun      <- switch(jj, "1" = runif, "2" = rnorm, "3" = rexp, NULL)
    fun_name <- switch(jj, "1" = "unif", "2" = "norm", "3" = "exp")
    col_name <- paste("DBL", j_dbl, fun_name, paste(args[-1], collapse = "_"),
                      sep = "_")

    # Add to Data Frame
    df[[col_name]] <- do.call(what = fun, args)
    j_dbl <- j_dbl + 1

  } else if ( ii == 3 ) {

    cat(" | String\n")

    # STRING case
    str_len <- sample(c(2, 5, 10), 1)
    value   <- replicate(str_len, sample(c(letters, LETTERS), N_ROWS_MAX, TRUE)) %>%
      apply(., 1, paste, collapse = "")
    col_name <- paste("STR", j_str, str_len, sep = "_")

    # Add to Data Frame
    df[[col_name]] <- value

    j_str <- j_str + 1

  } else {

    cat(" | Bool\n")

    # BOOL case
    p_bin    <- round(runif(1, 0, 1), 2)
    value    <- rbinom(N_ROWS_MAX, size = 1, p_bin)
    col_name <- paste("LGL", j_lgl, p_bin, sep = "_")

    # Add to Data Frame
    df[[col_name]] <- value

    j_lgl <- j_lgl + 1

  }
}

cat(paste0("\nTatsächliche Größe bei 1200 Spalten: ",
          format(object.size(df), units = "Mb"), "\n\n"))

# Create data sets and save
for ( i in seq(nrow(grid)) ) {

  n_col = grid$cols[i]
  n_row = grid$rows[i]

  cat("Write data with", n_col, " columns and ", n_row, " rows\n")

  file_name <- paste("sim_data", n_col, n_row, sep = "_")
  fwrite(df[1:n_row, 1:n_col], paste0(file_name, ".csv"))

}

# Finished
browseURL(url)
