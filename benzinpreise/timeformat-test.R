
library(data.table)
library(lubridate)
library(microbenchmark)


# variables set in station.R
# data from other scripts
DT <- fread(price_files[i.file], select = "date")
DT[, date := substr(date, 1, 19)]
DT[, c("DAY", "TIME") := tstrsplit(date, " ")]

n <- 10000
DT <- DT[sample(size = n, x = 1:.N, replace = FALSE), ]


# with base R
DT[, DATE_baseR := as.POSIXct(date)]

# with lubridate
DT[, DATE_ymd_hms := ymd_hms(date)]
DT[, DATE_parse_date_time := parse_date_time(date, "Ymd HMS")]

# with seperate cols and lubridate
DT[, DAY_ymd := ymd(DAY)]
DT[, TIME_hms := as.numeric(hms(TIME))]

# with data.table
DT[, DATE_IDate := as.IDate(date)]
DT[, DATE_ITime := as.ITime(date)]
DT[, c("DATE_IDate2", "DATE_ITime2") := IDateTime(date)]

# combination lubridate and data.table
DT[, TIME_hms_ITime := as.ITime(TIME_hms)]

# with seperate cols and data.table
DT[, DAY_IDate := as.IDate(DAY)]
DT[, TIME_ITime := as.ITime(TIME)]


# with unique date and data.table
hour_dt <- unique(DT[, .SD, .SDcol = "date"])
hour_dt[, DATE_ITIME_unique := as.ITime(date)]
hour_dt[, DATE_IDate_unique := as.IDate(date)]
DT <- merge(DT, hour_dt, by = "date", all.x = TRUE)


# with unique time and data.table
hour_dt <- unique(DT[, .SD, .SDcol = "TIME"])
hour_dt[, TIME_ITime_unique := as.ITime(TIME)]
DT <- merge(DT, hour_dt, by = "TIME", all.x = TRUE)


# rounding with lubridate
minute <- 30
DT[, ROUND_TIME_lb := lubridate::round_date(DATE_ymd_hms, paste0(minute, " mins"))]

# rounding with plyr
DT[, ROUND_TIME_pl := as.ITime(plyr::round_any(as.numeric(TIME_hms_ITime), minute * 60))]



# benchmark ---------------------------------------------------------------

# variables set in station.R
DT <- fread(price_files[i.file], select = "date")
DT[, date := substr(date, 1, 19)]
DT[, c("DAY", "TIME") := tstrsplit(date, " ")]


# presetting columns
DT[, DAY_ymd := lubridate::ymd(DAY)]
DT[, TIME_hms := as.numeric(lubridate::hms(TIME))]
DT[, DATE_ymd_hms := lubridate::ymd_hms(date)]
DT[, DATE_IDate := as.IDate(date)]
DT[, DATE_ITime := as.ITime(date)]

microbenchmark(
  # lubridate
  "lubridate::ymd()" = DT[, DAY_ymd := ymd(DAY)],
  "lubridate::hms()" = DT[, TIME_hms := as.numeric(hms(TIME))],
  # data.table
  "data.table::as.IDate()" = DT[, DATE_IDate := as.IDate(date)],
  "data.table::as.ITime()" = DT[, DATE_ITime := as.ITime(date)],
  # settings
  times = 100L, unit = "ms")


minute <- 30
microbenchmark(
  # rounding lubridate
  "lubridate::round_date()" = DT[, ROUND_TIME_lb := lubridate::round_date(DATE_ymd_hms, paste0(minute, " mins"))],
  # rounding plyr
  "plyr::round_any()" = DT[, ROUND_TIME_pl := as.ITime(plyr::round_any(as.numeric(DATE_ITime), minute * 60))],
  # settings
  times = 100L, unit = "ms")



