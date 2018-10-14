#' Creates a datetime object from a date and a time object
#'
#' @param date string containing date in format yyyy-mm-dd
#' @param time datetime object comprising time in HH:MM format
#'
#' @return datetime object
convert_to_time <- function(date, time) {
  date %>%
    sprintf("%s %s", ., strftime(time, format = "%H:%M:%S")) %>%
    ymd_hms() %>%
    return()
}

#' Changes data in a reactiveValues object
#'
#' @param x a reactiveValues object that shall be altered
#' @param ... key-value pairs that define what to change in x 
#'
#' @return reavtiveValues object with the set data
set_data <- function(x, ...) {
  args <- list(...)
  arg_names <- names(args)
  non_matching <- setdiff(arg_names, names(x))
  if (length(non_matching) > 0) {
    stop(sprintf("You cannot set the following attribute(s): %s", 
                 paste(non_matching, collapse = ", ")))
  }
  for (arg_name in arg_names) {
    if (arg_name %in% names(x)) {
      x[[arg_name]] <- args[[arg_name]]
    } 
  }
  return(x)
}

#' Set all entries in reactiveValues Object to NA
#'
#' @param x reactiveValues object
#'
#' @return reavtiveValues object with all entries set to NA
reset_data <- function(x) {
  for (n in names(x)) {
    x[[n]] <- NA
  }
  return(x)
}