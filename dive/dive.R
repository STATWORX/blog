#                            )    O
#                           (   o . O
#                            )   () .
#                           /  O   o
#                         _.|._ o .()
#              _         / _:_ \
#             <_><)     |.(_"_).|
#                __     _\. : ./_
#             |><_'>   / |..:..| \
#                     /_/ `---' \_\       ,
#             ,  (.   \_)        \_)  \)-<
#             _) \)~    \   T   /    ,(_)
#            _/ -(-<    _)__|__(_    \_)-<~
#             \)~ )-<  /....|....\  .~(_,_
#            >(_ (_/   """"" """""    _\
#         `-.__)__\_.----'`-.______.-'  `-.__
#                                                
#' dive
#' A function which simplifies debugging attempts of self-written R functions.
#' The main objective of this function is to get default and pre-specified arguments
#' of a function into a user-specified output. This output can either be the
#' global environment, a list or a console print.
#' @param x a string with a function or apply call
#' @param return a string either of type \code{"cons"}, \code{"env"} or
#'  \code{"list"}. Further information in the return section.
#'
#' @return The function has several return options:
#' \itemize{
#' \item \code{"cons"} prints the arguments into the console
#' \item \code{"env"} evaluates the arguments in the global 
#' \item \code{"list"} returns a list with the arguments.
#' }
#' When using *apply debugging only the return \code{"cons"} and \code{"list"}
#' are available, since there is not guess for the iterator.
#' @export
#' @details A word of caution: If you have string arguments in functions 
#'          like in the \code{\link{paste}}, you have to escape the quotation 
#'          marks. Suppose we want to debug the \code{\link{paste}} function
#'          with \code{\link{dive}}. Then specifying the argument \code{x} won't
#'          be work with \code{x = "paste("Hello World")"}, since there will be
#'          unexpected symbols in \code{"paste("Hello World")"}. Thus the correct
#'          specification is:
#'          \code{x = "paste(\"Hello World\")"}.
#' @examples
#' # Define a function
#' foo <- function(x = 3, y = 1, z = 1, type = "add") {
#'  if (type == "add") {
#'  OUT <- z+y+x
#'  } else if (type == "vec") {
#'  OUT <- c(z,y,x)
#'  } else {
#'  OUT <- list(z,y,x)
#'  }
#' return(OUT)
#' }
#' # Save the debug option into a string
#' my.debug <- "foo(x = 2, y = 3, z = 2)"
#' # Get the arguments with dive
#' dive(my.debug, return = "cons")
#' 
#' # Try dive with an apply function
#' my.apply.debug <- "lapply(c(1,3,5), FUN = foo, y = 6, z = 2)"
#' # Get the arguments with dive
#' dive(my.apply.debug, return = "cons")
#' 
#' # Try dive with a character argument
#' # Escape strings
#' my.string.debug <- "lapply(c(1,3,5), FUN = foo, y = 2, z = 1, type = \"vec\")"
#' dive(my.string.debug, return = "cons")
dive <- function(x, return = "cons") {
  
  # isolate function
  FUN <- regmatches(x, regexpr("^[^\\(]+", x))
  
  # get function arguments
  x.formal <- tryCatch({formals(FUN)}, error = function(e)
    return(paste0("Cannot find function.")))
  
  # exit with ill-defined function
  if (!is.list(x.formal)) stop(x.formal)
  
  # trim stuff
  x <- gsub(" |\\n", "", x)
  
  x.args <- strsplit(x, ",")[[1]]

  # get rid of ) at the end of the function call
  x.args[length(x.args)] <- gsub("\\)", 
                                 "", x.args[length(x.args)])
  # fix x.args due to ,
  open <- str_count(x.args, "\\(")
  open[1] <- open[1] - 1
  close <- str_count(x.args, "\\)")
  #close[length(close)] <- close[length(close)] - 1  

  if (!identical(open,close)) {
    end <- start <- c()
    x.append <- which(open-close==0)
  for(j in seq_along(open)) {
    if (open[j] != 0) {
    start[j] <- j
    end[j] <- min(which(open[j] == close[j:length(close)]))+j-1
    open[1:end[j]] <- 0
    }
  }
  end <- end[!is.na(end)]
  start <- start[!is.na(start)]
  ic <- intersect(end,start)
  if (length(ic)>0) {
      start <- start[-which(start == ic)]
      end <- end[-which(end == ic)]
  }
  # paste together
  xa <- c()
  for (glue in seq_along(start)) {
  xa[glue] <- paste(x.args[start[glue]:end[glue]], collapse = ",")
  }
  x.args <- c(x.args[x.append],
              xa)
 }
  # TODO implement purrr::map
  if (grepl("[[:alpha:]].*apply", FUN)) {
    
    # handle env return
    if (return == "env") {
      stop(paste("The argument",sQuote("return"), " has to be either ", 
                 sQuote("cons"), "or", sQuote("list"), 
                 "when using apply debugging."))
    }
    
    if (grepl("FUN", x)) {
      FUN <- gsub(".*,FUN=([[:alnum:]\\.\\_]*)(,|\\)).*", "\\1", x)
    } else {
      FUN <- gsub(".*,([[:alnum:]\\.\\_]*)(,|\\)).*", "\\1", x)
    }
    
    # get formals
    x.formal <- tryCatch({formals(FUN)}, error = function(e)
      return(paste0("Cannot find function.")))
    
    # get rid of apply stuff
    x.args <- x.args[-grep(paste0("apply|", FUN), x.args)]
    
  } else {
    
    # extract function from first argument
    x.args <- gsub(paste0(FUN,"\\("), "", x.args)
    
  }
  
  # CASE 1: No '=' specification
  if (!any(grepl("=", x.args))) {
    x.argl <- as.list(x.args)
    names(x.argl) <- names(x.formal)[2:(length(x.argl)+1)]
    # CASE 3: mixing '=' specification correct CASE 1 (unlikely)
    if (any(grepl("=", x.argl))) {
      where <- grep("=", x.argl)
      names(x.argl)[where] <- gsub("(.*)=.*", "\\1", x.argl[where])
      x.argl[where] <- gsub(".*=(.*)", "\\1", x.argl[where])
    }
    # CASE 2: '=' specification
  } else {
    x.argl <- as.list(gsub(".*=(.*)", "\\1", x.args))
    names(x.argl) <- gsub("(.*)=.*", "\\1", x.args)
  }
  
  # add missing information
  mis <- which(!names(x.formal) %in% names(x.argl))
  if(length(mis)>0) {
    x.argl <- append(x.argl, x.formal[mis])
  }
  
  # reorder
  x.argl <- x.argl[names(x.formal)]
  
  OUT <- switch(return,
                cons = {
                  OUT <- paste(names(x.argl), x.argl, sep = " = ")
                  return(cat(paste(OUT, collapse = "\n")))
                }, 
                env = {
                  # fix data type
                  num.ind <- suppressWarnings(which(!is.na(as.numeric(x.argl))))
                  x.argl[num.ind] <- as.numeric(x.argl[num.ind])
                  list2env(x.argl, envir = .GlobalEnv)
                  return("Stuff should be in your global environment")
                }, 
                list = {
                  # fix data type
                  num.ind <- suppressWarnings(which(!is.na(as.numeric(x.argl))))
                  x.argl[num.ind] <- as.numeric(x.argl[num.ind])
                  return(x.argl)
                }
  )
  return(OUT)
}