
# settings ----------------------------------------------------------------
# rm(list =ls())

library(data.table)
library(ggplot2)

# load functions
sapply(list.files("WM2018/R", full.names = TRUE), source)


# load data ---------------------------------------------------------------

game_data()


# take sample -------------------------------------------------------------

# sample for my tip
set.seed(20180614)

# bet group stage
tips <- c(8,6,6,6,6,8,8)
tips <- sum(tips)
prefinials <- lapply(tips, function(tip) {
  scores_agg[Round == "Prefinale",
             sample(result, tip, replace = TRUE, prob = percent)]
  })

# set to matrix
prefinials <- matrix(as.integer(unlist(strsplit(prefinials[[1]], "-"))),
                     ncol = 2, byrow = TRUE)


# bet finals
tips <- c(8,4,2,2)
tips <- sum(tips)
finals <- lapply(tips, function(tip) {
  scores_agg[Round == "Finale",
             sample(result, tip, replace = TRUE, prob = percent)]
})



# comparing to real and others --------------------------------------------

# check realscores
print("check if any real scores were not within the data")
print(all(na.omit(unlist(realscores)) %in% scores_agg$result))

# simulate compare list
tips <- sum(c(8,6,6,6,6,8,8))
n_sim <- 100

result_list <- as.list(rep(NA, n_sim))
models <- c("history", "simple", "skellam", "elo", "fussballmathe")

set.seed(2688)

for (i.sim in seq_len(n_sim)) {
  # i.sim <- 1
  result_list[[i.sim]] <- lapply(models, function(i.model) {
    group_match_dt[, play_game(team_data = team_data,
                               team1 = team1_num,
                               team2 = team2_num,
                               play_fun = i.model,
                               i.sim = i.sim,
                               settings = settings)]
  })
  names(result_list[[i.sim]]) <- models
}

# i.sim <- result_list[[1]]
# i.model <- i.sim[[1]]
all_points <- unlist(lapply(result_list,
  function (i.sim) {
    sapply(i.sim, function (i.model) {
      compare_scores(realscores, i.model)
      })
   }))



# combine all predictions and my tips
points_dt <- data.table(x = all_points, type = names(all_points))
points_dt <- points_dt[, list(freq = .N), by = c("type", "x")]

# add results of the betting game participants
points_dt <- rbind(points_dt, betting_game, use.names = TRUE)

# change the labels for the later plot
type_levels <- c(models, "bets")
type_labels <- c("based on historical results",
                 "independent Poisson distributions",
                 "Skellam distribution",
                 "based on ELO ratings",
                 "simulation of fussballmathe",
                 "participants of the betting game")
points_dt[, type := factor(type,
                           levels = type_levels,
                           labels = type_labels)]

my_points <- points_dt[,list(mine = 0),  by = type]
my_points[, mine := ifelse(type %in% type_labels[c(1, 6)],
                           compare_scores(realscores, prefinials),
                           NA)]

max_points <- sum(sapply(realscores, function(x) sum(!is.na(x)))*4)



# plots -------------------------------------------------------------------

# difference between predictions

g1 <- ggplot(points_dt, aes(x = x, y = freq)) + 
  geom_bar(stat="identity", fill = "lightblue") +
  geom_vline(data = my_points,
             aes(xintercept = mine),
             color = "black") +
  geom_vline(data = points_dt[, list(mean_x = mean(x)), by = type],
             aes(xintercept = mean_x),
             color = "red") +
  #geom_vline(xintercept = max_points, color = "black") + 
  facet_wrap(~type, ncol = 1) +
  xlab("points") +
  ylab("frequency") +
  ggtitle("Frequencies of different estimation models") +
  theme_light() +
  theme(strip.background = element_rect(fill="steelblue")) +
  theme(strip.text = element_text(colour = "white", size = 16))
ggsave(g1, file = "WM2018/plots/points-for-all-models.png",
       width = 30, height = 30, units = "cm")


# historical distribution

g2 <- ggplot(scores_agg, aes(x = result, y = percent*100)) + 
  geom_bar(stat="identity", fill = "lightblue") +
  #geom_bar(stat="identity", position=position_dodge()) +
  theme_light() +
  theme(strip.background = element_rect(fill="steelblue")) +
  theme(strip.text = element_text(colour = "white", size = 16)) +
  theme(axis.text.x  = element_text(angle = 90, vjust = 0.5, size = 10)) + 
  ggtitle("distribution of World Cup scores") +
  ylab("%")
ggsave(g2, file = "WM2018/plots/score-distribution.png",
       width = 20, height = 10, units = "cm")



