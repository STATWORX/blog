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

###
 # Filter functions for filter.R
 #
 # Each function is the filter function for a scenario. Will be used in microbenchmark
 ##

filter_1o2 <- function() {
	return( dt[dt[[names(filter)]] >= filter] )
}

filter_3o4 <- function() {
	return( dt[dt[[names(filter)]] == filter] )
}

filter_1a2 <- function() {
	return(
		dt[
			dt[[names(filter)[1]]] >= filter[1] & dt[[names(filter)[2]]] >= filter[2]
		]
	)
}

filter_1a2a3 <- function() {
	return(
		dt[
			dt[[names(filter)[1]]] >= filter[1] & dt[[names(filter)[2]]] >= filter[2] &
			dt[[names(filter)[3]]] == filter[3]
		]
	)
}

filter_1a2a3a4 <- function() {
	return(
		dt[
			dt[[names(filter)[1]]] >= filter[1] & dt[[names(filter)[2]]] >= filter[2] &
			dt[[names(filter)[3]]] == filter[3] & dt[[names(filter)[4]]] == filter[4]
		]
	)
}