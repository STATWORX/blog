###
 # draw_ind_for_selsid
 #
 # Draws `n_draws` columns indices of a data frame with `n_cols` columns. `pos`
 # indicates the position from where draws should be generated
 ##

draw_ind_for_select <- function(n_cols, n_draws, pos = "any") {

  lb <- n_cols / 3 * switch(pos, "beg" = 0, "any" = 0, "mid" = 1, "end" = 2) + 1
  ub <- n_cols / 3 * switch(pos, "beg" = 1, "any" = 3, "mid" = 2, "end" = 3)

  sample(lb:ub, n_draws, FALSE)

}