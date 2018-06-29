#' Title loading game_data
#'
#' @description This functions loads the needed data for the simulation
#' @return At the end, all data is made available in the global environment.
#' @export
#'
game_data <- function() {
  
  out <- list(settings = list())
  out$settings <- list()
  
  ### real scores ----
  realscores <- matrix(as.integer(c(
    5, 0, 0, 1, 0, 1, 3, 3, 2, 1, 1, 1, 0, 1, 2, 0,
    0, 1, 0, 1, 1, 1, 1, 0, 3, 0, 1, 2,
    1, 2, 1, 2, 3, 1, 1, 0, 1, 0, 0, 1,
    1, 1, 1, 0, 0, 3, 2, 0, 2, 0, 1, 2,
    5, 2, 1, 2, 2, 1, 6, 1, 2, 2, 0, 3,
    3, 0, 2, 1, 1, 1, 2, 2, 0, 0, 0, 2, 1, 2, 1, 2,
    2, 0, 0, 3, 0, 2, 2 ,2 , 0, 1, 0, 1, 0, 1, 1, 2)),
    ncol = 2, byrow = TRUE)
  
  out$realscores <- realscores
  
  ### team data ----
  team_data <- data.table(
    number = 1:32,
    name = c("Egypt","Russia","Saudi Arabia","Uruguay",
             "Iran","Morocco","Portugal","Spain",
             "Australia","Denmark","France","Peru",
             "Argentina","Croatia","Iceland","Nigeria",
             "Brazil","Costa Rica","Switzerland","Serbia",
             "Germany","South Korea","Mexico","Sweden",
             "Belgium","England","Panama","Tunisia",
             "Colombia","Japan","Poland","Senegal"),
    group = rep(LETTERS[1:8], each = 4),
    rating = c(151, 41, 1001, 34,
               501, 501, 26, 7,
               301, 101, 7.5, 201,
               10, 34, 201, 201,
               5, 501, 101, 201,
               5.5, 751, 101, 151,
               12, 19, 1001, 751,
               41, 301, 51, 201),
    elo = c(1646, 1685, 1582, 1890, # From https://www.eloratings.net/, May 12th 2018
            1793, 1711, 1975, 2048,
            1714, 1843, 1984, 1906,
            1985, 1853, 1787, 1699,
            2131, 1745, 1879, 1770,
            2092, 1746, 1859, 1796,
            1931, 1941, 1669, 1649,
            1935, 1693, 1831, 1747)
  )
  
  out$team_data <- team_data
  
  ### match data ----
  group_match_data <- read.csv(
    text = paste0("team1,team2,date\n",
                  "Russia,Saudi Arabia,14/06/2018\n",
                  "Egypt,Uruguay,15/06/2018\n",
                  "Morocco,Iran,15/06/2018\n",
                  "Portugal,Spain,15/06/2018\n",
                  "France,Australia,16/06/2018\n",
                  "Argentina,Iceland,16/06/2018\n",
                  "Peru,Denmark,16/06/2018\n",
                  "Croatia,Nigeria,16/06/2018\n",
                  "Costa Rica,Serbia,17/06/2018\n",
                  "Germany,Mexico,17/06/2018\n",
                  "Brazil,Switzerland,17/06/2018\n",
                  "Sweden,South Korea,18/06/2018\n",
                  "Belgium,Panama,18/06/2018\n",
                  "Tunisia,England,18/06/2018\n",
                  "Colombia,Japan,19/06/2018\n",
                  "Poland,Senegal,19/06/2018\n",
                  "Russia,Egypt,19/06/2018\n",
                  "Portugal,Morocco,20/06/2018\n",
                  "Uruguay,Saudi Arabia,20/06/2018\n",
                  "Iran,Spain,20/06/2018\n",
                  "Denmark,Australia,21/06/2018\n",
                  "France,Peru,21/06/2018\n",
                  "Argentina,Croatia,21/06/2018\n",
                  "Brazil,Costa Rica,22/06/2018\n",
                  "Nigeria,Iceland,22/06/2018\n",
                  "Serbia,Switzerland,22/06/2018\n",
                  "Belgium,Tunisia,23/06/2018\n",
                  "South Korea,Mexico,23/06/2018\n",
                  "Germany,Sweden,23/06/2018\n",
                  "England,Panama,24/06/2018\n",
                  "Japan,Senegal,24/06/2018\n",
                  "Poland,Colombia,24/06/2018\n",
                  "Saudi Arabia,Egypt,25/06/2018\n",
                  "Uruguay,Russia,25/06/2018\n",
                  "Iran,Portugal,25/06/2018\n",
                  "Spain,Morocco,25/06/2018\n",
                  "Australia,Peru,26/06/2018\n",
                  "Denmark,France,26/06/2018\n",
                  "Nigeria,Argentina,26/06/2018\n",
                  "Iceland,Croatia,26/06/2018\n",
                  "Mexico,Sweden,27/06/2018\n",
                  "South Korea,Germany,27/06/2018\n",
                  "Serbia,Brazil,27/06/2018\n",
                  "Switzerland,Costa Rica,27/06/2018\n",
                  "England,Belgium,28/06/2018\n",
                  "Senegal,Colombia,28/06/2018\n",
                  "Panama,Tunisia,28/06/2018\n",
                  "Japan,Poland,28/06/2018"),
    header = TRUE)
  
  
  group_match_dt <- as.data.table(group_match_data)
  group_match_dt[, game := .I]
  group_match_dt[, team1_num := team_data$number[team_data$name == team1],
                 by = team1]
  group_match_dt[, team2_num := team_data$number[team_data$name == team2],
                 by = team2]
  
  out$group_match_dt <- group_match_dt
  
  ### read history files ----
  path <- "WM2018/data/history"
  
  all_files <- list.files(path, full.names = TRUE)
  txt_files <- grep(".txt", all_files, value = TRUE)
  
  getscores <- function(i.txt) {
    #i.txt <- txt_files[10]
    #print(i.txt)
    tmp_txt <- readLines(i.txt)
    start <- which(grepl("Gruppenspiele", tmp_txt))
    if (length(start) > 0) {
      tmp_txt <- tmp_txt[min(start):length(tmp_txt)]
    }
    
    
    index_finals <- min(which(grepl("(finale)|(Endrunde)|(Spiel um den dritten Platz)",
                                    tmp_txt, perl = TRUE)))
    # pre finials
    index_scores <- which(grepl("Abpfiff", tmp_txt[1:index_finals])) + 1
    scores <- data.table(result = tmp_txt[index_scores])
    scores[, File := gsub(".txt", "", basename(i.txt))]
    scores[, Round := "Prefinale"]
    
    # finals
    index_scores <- which(grepl("Abpfiff", tmp_txt[index_finals:length(tmp_txt)])) + index_finals - 1 
    new_index <- which(grepl("Elfmeterschießen", tmp_txt[index_finals:length(tmp_txt)])) + index_finals - 1 
    
    if (length(new_index) > 0) {
      tmp_index <- sapply(new_index, function(x) which(index_scores == x - 2) )
      index_scores[tmp_index] <- new_index - 1
    } else {
      tmp_index <- integer()
    }
    
    scores_final <- data.table(result = tmp_txt[index_scores + 1])
    scores_final[tmp_index, result := gsub("[[:blank:][:alpha:]\\(\\)]", "", result)]
    
    scores_final[, File := gsub(".txt", "", basename(i.txt))]
    scores_final[, Round := "Finale"]
    
    out <- rbind(scores, scores_final)
    return(out)
  }
  
  all_scores <- lapply(txt_files, getscores)
  
  scores <- rbindlist(all_scores)
  scores[, Jahr := gsub("[[:punct:]|[:alpha:]]", "", File)]
  scores[, TeamA := unlist(lapply(strsplit(scores$result, "-"), "[[", 1))]
  scores[, TeamB := unlist(lapply(strsplit(scores$result, "-"), "[[", 2))]
  scores <- scores[!(TeamA == TeamB & Round == "Finale"),]
  #scores[, Jahr := factor(x = Jahr, levels = sort(unique(Jahr)))]
  
  # aggregate
  scores_agg <- scores[, list (percent = .N/nrow(scores)),
                       by = c("Jahr", "Round", "result")]
  
  #normalgoals <- 2.75
  normalgoals <- scores[, mean(as.numeric(TeamA) + as.numeric(TeamB))]
  
  out$scores_agg <- scores_agg
  out$settings$normalgoals <- normalgoals
  
  ### data from simulation ----
  # http://fussballmathe.de/simulation/
  path <- "WM2018/data/fussballmathe/"
  fussballmathe <- fread(file = paste0(path, "results.csv"),
                    sep = ",",
                    header = TRUE)
  
  # swtich sides for some games to get the same order
  setnames(fussballmathe, c("Team1", "score", "Team2", "run", "game"))
  
  goal_var <- c("Goal1", "Goal2")
  fussballmathe[, c(goal_var) := tstrsplit(score, "-", fixed=TRUE)]
  fussballmathe[, c(goal_var) := lapply(.SD, as.integer), .SDcol = goal_var]
  fussballmathe[game < 0, c("Team1", "Team2", "Goal1", "Goal2") :=
             list(Team2, Team1, Goal2, Goal1)]
  fussballmathe[, game := abs(game)]
  fussballmathe[, run := run + 1]
  fussballmathe <- fussballmathe[order(run, game)]
  
  out$fussballmathe <- fussballmathe
  
  ### skellam data ----
  
  eta <- 2.75
  beta1 <- seq(0.05, eta-0.05, 0.05)
  
  
  skellam <- rep(0, length(beta1))
  counter <- 1
  for (i in beta1) {
    
    # Compute probability that team 1 wins
    skellam[counter] <- 
      sum(dskellam(1:12, i, eta - i)) /
      ( sum(dskellam(seq(-10,10,1), i, eta-i)) - dskellam(0, i, eta - i) )
    counter <- counter + 1
  }
  
  skellam <- data.frame(beta=beta1, prob=skellam)
  
  out$skellam <- skellam
  out$settings$eta <- eta
  
  ### data from betting game ----
  path <- "WM2018/data/"
  betting_game <- fread(file = paste0(path, "bets2018.csv"),
                        sep = ";",
                        header = TRUE,
                        select = c("G"),
                        col.names = c("x"))
  # set names
  betting_game[, type := "bets"]
  
  # aggregate scores
  betting_game <- betting_game[, list(freq = .N), by = c("type", "x")]
  
  out$betting_game <- betting_game
  
  
  #### DONE
  cat("loading into global:", names(out), "\n")

  list2env(out, envir = .GlobalEnv)
  
  return("DONE")
  
}

