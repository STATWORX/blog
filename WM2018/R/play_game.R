# Input: First three arguments are fixed. Default values are given
# Returns: a matrix with length(team1) rows 
# and 2 columns with goals
play_game <- function(team_data,
                      team1,
                      team2,
                      musthavewinner = FALSE,
                      play_fun = "simplest",
                      i.sim = numeric(),
                      settings = list()) {
  # Sanity checks
  if (length(team1) != length(team2))
    stop("Lengths of team should be the same")
  
  if (any(team1==team2))
    stop("A team cannot play against itself")
  
  ## Simplest version. 
  ## All teams are equal
  if (play_fun == "simple") {
    result <- cbind(rpois(length(team1), lambda = settings$normalgoals/2), 
                    rpois(length(team1), lambda = settings$normalgoals/2))
  } else if (play_fun == "skellam") {
    ## Skellam distribution
    ## Uncomment the code below to use the skellam model
    p1 <- .91/team_data$rating[team1]
    p2 <- .91/team_data$rating[team2]
    prob <- p1 / (p1 + p2)
    lambdaA <- FindParameter(prob, settings$eta)
    Agoals <- rpois(length(prob), lambdaA)
    Bgoals <- rpois(length(prob), settings$normalgoals - lambdaA)
    result <- cbind(Agoals, Bgoals)
    
  } else if (play_fun == "elo") {
    ## ELO version (no update here). Using sapply here instead of
    ## vectorization in case the elo ranking should be updated after each match.
    # Uncomment below to use the ELO model
    result <- t(sapply(seq_len(length(team1)),
                       function(i) {
                         AWinProb <- 1/(1 + 10^((team_data$elo[team2[i]] - team_data$elo[team1[i]])/400))
                         myres <- rbinom(1, size=1, prob=AWinProb)
                         fakegoals <- c(1,0)  
                         if (myres==0)
                           fakegoals <- c(0,1)
                         fakegoals
                       }
    )
    )
  } else if (play_fun == "history") {
    
    result <- scores_agg[Round == ifelse(musthavewinner, "Finale", "Prefinale"),
                 sample(result, length(team1), replace = TRUE, prob = percent)]
    result <- matrix(as.integer(unlist(strsplit(result, "-"))),
                     ncol = 2, byrow = TRUE)
    
  } else if (play_fun == "fussballmathe") {
    if (length(i.sim) == 0 ) {
      stop("i.sim needed for fussballmathe 'run'")
    } else if (fussballmathe[, max(run)] < i.sim) {
      stop("there are not so many different simulations")
    }
    
    result <- fussballmathe[run == i.sim, matrix(c(Goal1, Goal2), ncol = 2)]
    
  } else {
    stop("play_fun not defined")
  }
  
  # If we MUST have a winner then one simple trick is to add a random goal 
  # to one of the two teams that have the same score. Penalty goals seem rather 
  # random anyway
  if (musthavewinner) {
    result[result[,1]==result[,2],1] + 2*rbinom(sum(result[,1]==result[,2]), size=1, prob=.5) - 1
    
  }
  
  # set to integer
  result <- matrix(as.integer(result), ncol = 2, byrow = FALSE)
  
  return(result)
}
