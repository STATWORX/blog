##
##                    INITIALIZE FRIEND DATA
##

library(data.table)
library(dplyr)
library(lubridate)

friend_score <- fread("data/raw/friend_score.csv", data.table = FALSE)
saveRDS(friend_score, "data/friend_score.RDS")

visit_log <- fread("data/raw/visit_log.csv", 
                   data.table = FALSE,
                   na.strings = "") %>%
  mutate(exp_time = ymd_hms(exp_time),
         act_time = ymd_hms(act_time))
saveRDS(visit_log, "data/visit_log.RDS")
