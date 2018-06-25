#' Title comparing scores
#'
#' @description This function compares two given matricies with scores.
#'   The first matrix contains the real values.
#'   Points are as follows:
#'   
#'   |         | trend | goal difference | result |
#'   | :-----: | :---: | :-------------: | :----: |
#'   | Victory |   2   |        3        |   4    |
#'   |  Draw   |   2   |        -        |   4    |
#'
#' @param x1 a matrix with the real scores
#' @param x2 a matrix with the predicted scores
#'
#' @return Retunrs the number of points
#' @export
#'
#' @examples
#' real <- matrix(as.integer(c(5, 0, 0, 1, 0, 1, 3, 3)),
#'                ncol = 2, byrow = TRUE)
#'                pred <- matrix(as.integer(c(0, 2, 0, 1, 1, 2, 1, 1)),
#'                               ncol = 2, byrow = TRUE)
#' compare_scores(real, pred)
#' 
compare_scores <- function(x1, x2) {
  # x1 <- realscores[1:5,]
  # x2 <- result_list[[1]][[1]][1:5,]
  # x2 <- prefinials[1:5,]
  # x2 <- matrix(c(5,1,0,1,1,2,1,1,3,1), ncol = 2, byrow = TRUE)
  
  #same result
  same_score <- apply(X = x1 == x2, FUN = all, MARGIN = 1)
  x1 <- matrix(x1[!same_score, ], ncol = 2, byrow = FALSE)
  x2 <- matrix(x2[!same_score, ], ncol = 2, byrow = FALSE)
  
  
  # ties
  same_diff_equal <- (x1[, 1] == x1[, 2]) &
    (x2[, 1] == x2[, 2])
  x1 <- matrix(x1[!same_diff_equal, ], ncol = 2, byrow = FALSE)
  x2 <- matrix(x2[!same_diff_equal, ], ncol = 2, byrow = FALSE)
  
  # same score difference
  same_diff <- x1[, 1] - x1[, 2] == x2[, 1] - x2[, 2]
  x1 <- matrix(x1[!same_diff,], ncol = 2, byrow = FALSE)
  x2 <- matrix(x2[!same_diff,], ncol = 2, byrow = FALSE)
  
  # same winner
  same_winner <- sign(x1[, 1] - x1[, 2]) == sign(x2[, 1] - x2[, 2])
  
  # points
  points <- sum(c(same_score*4, same_diff_equal*2, same_diff*3, same_winner*2),
                na.rm = TRUE)
  
  return(points)
}