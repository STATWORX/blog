#rm(list = ls())
# for maps
#install.packages("scales")
#install.packages("tidyverse")
#install.packages("tmaptools")
#install.packages("ggimage")
#install.packages("ggmap")
#install.packages("zoo")
#install.packages("choroplethr")
#install.packages(dplyr) NO!
#install.packages("rgdal")
#install.packages("gpclib")
#install.packages(readr) NO!
#install.packages("R6")
#install.packages("stringdist")
# devtools::install_github("STATWORX/helfRlein", ref = "dev")

library(data.table)
#library(scales)
#library(tidyverse)
library(ggplot2)
library(tmaptools)
#library(ggimage)
library(ggmap)

#library(choroplethr)
#library(dplyr)

library(rgdal)
#library(maptools)
#library(gpclib)
#library(readr)
#library(R6)

#library(lubridate)
library(helfRlein)
library(jsonlite)
library(stringdist)

# TODO
# DONE - tagespreis: absolut, relativ zum mittelwert
# DONE - Preis Zeitreihe
# DONE - Preis regional
# DONE - Autobahnen
# DONE - Öffnungszeiten -> Nachts wird teurer, wenn andere zu sind?
# DONE - e10 vs Diesel vs Super
# DONE - Marken
# DONE - nach Wochentagen
# DONE - add :: for functions
# DONE - 95% INtervalle
# DONE - fix na forward, since closed has no price, shoud be NA

# functions ---------------------------------------------------------------

# set a specific value and range to NA
setNA <- function(x, value = c(-0.001, 0, 8.888, 9.999),
                  range = c(0.8, 2.5)) {
  #x <- c(0,-2,5,1,1,2,8)
  x_out <- ifelse(x %in% value, NA, x)
  x_out <- ifelse(between(x = x_out, lower = min(range), upper = max(range)), x, NA)
  return(x_out)
}

# to plot scales with a set dezimal pointnumber
scaleFUN <- function(x) {
  sprintf("%.2f", x)
}

# get last value
last_value <- function(x, na.rm = TRUE) {
  # x <- c(1,4,NA,3)
  # x <- c(1,4,NA, NA)
  if (na.rm) {
    return(last(x[!is.na(x)]))
  } else {
    return(last(x))
  }
}

# get weekday or holiday
get_wday <- function(x) {
  #x <- as.Date(this_day)
  
  holiday <- data.table(DATE = as.Date(c("2019-01-01", "2014-12-25")),
                        AREA = 0)
  
  if (holiday[, any(DATE %in% x)]) {
    return("Holiday")
  } else {
    return(weekdays(x))
  }
  
}

# get mean and quantiles 0.1 and 0.9
stat_fun <- function(x, na.rm = TRUE) {
  #x <- price_time_dt[,.SD, .SDcol = c("diesel", "e10")]
  x_quant1 <- x[, lapply(.SD, quantile, probs = c(0.1), na.rm = na.rm),
                .SDcols = names(x)]
  x_quant9 <- x[, lapply(.SD, quantile, probs = c(0.9), na.rm = na.rm),
                .SDcols = names(x)]
  x_mean <- x[, lapply(.SD, mean, na.rm = na.rm), .SDcols = names(x)]
  
  x_obs <- x[, lapply(.SD, function(y) sum(!is.na(y))), .SDcols = names(x)]
  
  setnames(x_quant1, paste0(names(x), "_Q10"))
  setnames(x_quant9, paste0(names(x), "_Q90"))
  setnames(x_mean, paste0(names(x), "_MEAN"))
  setnames(x_obs, paste0(names(x), "_OBS"))
  
  out <- data.table(x_quant1, x_quant9, x_mean, x_obs)
  return(out)
}

# aggregate price data by key
agg_price <- function(x, by = NULL) {
  #x <- copy(time_dt)
  #by <- c("TIME", "DATE")
  
  this_cols <- c("PRICE", "MIN", "MAX", "OBS")
  if (any(this_cols %nin% names(x))) {
    stop(paste0("missing columns: ",
                paste0(setdiff(this_cols, names(x)), collapse = ", ")))
  }
  
  dt <- x[, list("PRICE" = weighted.mean(x = PRICE, w = OBS,  na.rm = TRUE),
                 "MIN" = min(MIN, na.rm = TRUE),
                 "MAX" = max(MAX, na.rm = TRUE),
                 "OBS" = sum(OBS)),
                 by = by]
  
  return(dt)
}

agg_price2 <- function(files, by = NULL, scaled = FALSE, scaledby = NULL) {
  # files <- paste0(mainpath, "time_dt_", all_year, ".rds")
  # by <- c("TIME", "DATE")
  if (!is.null(scaledby)) {
    scaled <- TRUE
  }
  
  out_agg <- rep(list(data.table()), length(files))
  for (i.file in seq_along(files)) {
    # i.file <- 1
    tmp_time_dt <- readRDS(files[i.file])
    
    tmp_time_dt <- melt(tmp_time_dt,
                    id.vars = c("TIME", "DATE", "WDAY", "YEAR", "AUTOBAHN", "BRAND", "plz"),
                    measure = patterns("MEAN", "Q10", "Q90", "OBS"),
                    variable.name = "TYPE",
                    value.name = c("PRICE", "Q10", "Q90", "OBS"))
    tmp_time_dt[, TYPE := factor(TYPE,
                                 levels = as.character(seq_along(price_vars)),
                                 labels = price_vars)]
    
    this_cols <- c("PRICE", "Q10", "Q90", "OBS")
    if (any(this_cols %nin% names(tmp_time_dt))) {
      stop(paste0("missing columns: ",
                  paste0(setdiff(this_cols, names(tmp_time_dt)), collapse = ", ")))
    }
    
    if (scaled) {
      tmp_time_dt[, Q10 := Q10 - weighted.mean(PRICE, OBS, na.rm = TRUE),
                  by = scaledby]
      tmp_time_dt[, Q90 := Q90 - weighted.mean(PRICE, OBS, na.rm = TRUE),
                  by = scaledby]
      tmp_time_dt[, PRICE := PRICE - weighted.mean(PRICE, OBS, na.rm = TRUE),
                  by = scaledby]
    }
    
    
    
    out_agg[[i.file]] <- 
      tmp_time_dt[,list("PRICE" = weighted.mean(x = PRICE, w = OBS,  na.rm = TRUE),
                        #"MIN" = min(MIN, na.rm = TRUE),
                        #"MAX" = max(MAX, na.rm = TRUE),
                        "Q10" = weighted.mean(x = Q10, 1/sqrt(rank(Q10)) * OBS),
                        "Q90" = weighted.mean(x = Q90, 1/sqrt(rank(-Q90)) * OBS),
                        "OBS" = sum(OBS)),
                  by = by]
    
  }
  
  out_dt <- rbindlist(out_agg, use.names = TRUE, fill = TRUE)
  out_dt <- out_dt[, list("PRICE" = weighted.mean(x = PRICE, w = OBS,  na.rm = TRUE),
                          #"MIN" = min(MIN, na.rm = TRUE),
                          #"MAX" = max(MAX, na.rm = TRUE),
                          "Q10" = weighted.mean(x = Q10, 1/sqrt(rank(Q10)) * OBS),
                          "Q90" = weighted.mean(x = Q90, 1/sqrt(rank(-Q90)) * OBS),
                          "OBS" = sum(OBS)),
                   by = by]
  
  
  return(out_dt)
}


plotpath <- "/Users/jakobgepp/Projekte/2018/Intern/Blog/Benzinpreis/pics/"
mainpath <- "/Users/jakobgepp/Projekte/2018/Intern/Blog/Benzinpreis/"

# stations ----------------------------------------------------------------


rawdata_path <- "/Users/jakobgepp/Projekte/2018/Intern/Blog/Benzinpreis/stations/stations.csv"
stations_dt <- fread(file = rawdata_path, header = TRUE, sep = ",")

rawdata_path2 <- "/Users/jakobgepp/Projekte/2018/Intern/Blog/Benzinpreis/stations/2019/"
station_files <- list.files(path = rawdata_path2,
                            pattern = ".csv",
                            full.names = TRUE,
                            recursive = TRUE)



# reduce to Frankfurt
#stations_dt <- stations_dt[city %in% grep("Frankfurt", unique(city), value = TRUE) &
#                               post_code> "50000",]
#stations_dt <- stations_dt[post_code >= "60000" & post_code < "66000",]

#stations_dt <- stations_dt[uuid %in% c("00041414-208c-4444-8888-acdc00000414", "fff2435f-84de-4f41-b62b-9f6810169f45"),]


# filter stations without postcode
stations_dt <- stations_dt[post_code %nin% c("", "nicht", "Nicht"),]
stations_dt[, city := helfRlein::char_replace(city,
                                              to_lower = TRUE,
                                              rm_dash = TRUE,
                                              rm_space = TRUE)]
stations_dt[, street := helfRlein::char_replace(street,
                                                to_lower = TRUE,
                                                rm_dash = TRUE,
                                                rm_space = TRUE)]
stations_dt[, brand := toupper(brand)]

# adjust post_code 
plz_var <- "plz2" # or "post_code"
stations_dt[, plz2 := substr(post_code, 1, 2)]
plz_raw <- unique(stations_dt[, list("station_uuid" = uuid, "plz" = get(plz_var))])

all_stations <- stations_dt[, unique(uuid)]


# opening times -----------------------------------------------------------


# TODO check overrides
open_list <- rep(list(data.table()), length(station_files))
for (i.file in seq_along(station_files)) {
  # i.file <- 1
  this_day <- gsub("-stations.csv", "", basename(station_files[i.file]))
  helfRlein::statusbar(run = i.file, max.run = length(station_files), info = this_day)
  tmp_dt <- fread(file = station_files[i.file], header = TRUE, sep = ",")
  
  index <- tmp_dt[, which(uuid %in% all_stations)]
  
  open_dt_tmp <- fread(file = station_files[i.file],
                       header = TRUE, sep = ",",
                       select = "openingtimes_json")
  opentime <- open_dt_tmp[, gsub('""', '"', openingtimes_json)][index]
  tmp_dt <- tmp_dt[index, ]
  
  bla <- lapply(opentime, jsonlite::fromJSON)
  bla2 <- lapply(seq_along(bla),
                 function (x) {
                   # x <- 113
                   tmp <- bla[[x]]
                   out <- list("open" = data.table(),
                               "over" = data.table())
                   if (length(tmp) == 0) {
                     out$open <- data.table(nr = tmp_dt[x, uuid],
                                       days = 2^8-1, # all days
                                       startp = "00:00",
                                       endp = "23:59",
                                       DATE = this_day)
                   } else {
                     
                     if ("openingTimes" %in% names(tmp)) {
                       tmp2 <- tmp$openingTimes
                       out$open <- data.table(nr = tmp_dt[x, uuid],
                                              days = tmp2$applicable_days,
                                              rbindlist(tmp2$periods),
                                              DATE = this_day) 
                     }
                     if ("overrides" %in% names(tmp)) {
                       out$over <- data.table(nr = tmp_dt[x, uuid],
                                              tmp$overrides,
                                              DATE = this_day)
                     }
                   }
                   return(out)
                 })
  open_list[[i.file]] <- list(
    "open" = rbindlist(lapply(bla2, "[[", "open"),
                       use.names = TRUE,
                       fill = TRUE),
    "over" = rbindlist(lapply(bla2, "[[", "over"),
                       use.names = TRUE,
                       fill = TRUE)
  )
}

open_dt <- rbindlist(lapply(open_list, "[[", "open"),
                     fill = TRUE,
                     use.names = TRUE)
open_dt <- unique(open_dt[nr %in% all_stations, ],
                  by = setdiff(names(open_dt), "DATE"))

## adjust over_dt
over_dt <- rbindlist(lapply(open_list, "[[", "over"),
                     fill = TRUE,
                     use.names = TRUE)
over_dt <- unique(over_dt[nr %in% all_stations, ],
                  by = setdiff(names(over_dt), "DATE"))

over_dt[, c("START_DATE", "START_TIME") := IDateTime(startp)]
over_dt[, c("END_DATE", "END_TIME") := IDateTime(endp)]
over_dt[, c("startp", "endp") := NULL]
setnames(over_dt, "nr", "station_uuid")

#open_all <- rbindlist(list(open_dt, over_dt), fill = TRUE, use.names = TRUE)




# applicable_days
# Bit 	Zahlenwert 	Bedeutung
# 0 	1 	Montag
# 1 	2 	Dienstag
# 2 	4 	Mittwoch
# 3 	8 	Donnerstag
# 4 	16 	Freitag
# 5 	32 	Samstag
# 6 	64 	Sonntag
# 7 	128 	Feiertag

days_pattern <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "Holiday")

days_tmp <- matrix(as.numeric(intToBits(open_dt[, unique(days)])), ncol = 32, byrow = TRUE)[, 1:8]
days_tmp[days_tmp == 0] <- NA
colnames(days_tmp) <- days_pattern
days_dt <- data.table(days = open_dt[, unique(days)], days_tmp)

days_dt <- melt(data = days_dt, measure.vars =  days_pattern, variable.name = "DAY", na.rm = TRUE)
days_dt[, value := NULL]

open_dt <- merge(open_dt, days_dt, by = "days", allow.cartesian = TRUE)
open_dt[endp == "00:00", endp := "23:59"]
to_time <- c("startp", "endp") 
open_dt[, c(to_time) := lapply(.SD, function(x) as.numeric(as.ITime(x))) , .SDcols = to_time]

# check if there are multiple opening times
open_dt[, CHECK := DATE == min(DATE), by = c("nr", "DAY")]
open_dt <- open_dt[CHECK == TRUE, ]

to_remove <- c("DATE", "days", "CHECK")
open_dt[, c(to_remove) := NULL]
# only those where start is before end
open_dt <- open_dt[startp <= endp,]
setnames(open_dt, "nr", "station_uuid")

#saveRDS(open_dt, "/Users/jakobgepp/Projekte/2018/Intern/Blog/Benzinpreis/open_dt.rds")
#saveRDS(over_dt, "/Users/jakobgepp/Projekte/2018/Intern/Blog/Benzinpreis/over_dt.rds")
#open_dt <- readRDS("/Users/jakobgepp/Projekte/2018/Intern/Blog/Benzinpreis/open_dt.rds")
#over_dt <- readRDS("/Users/jakobgepp/Projekte/2018/Intern/Blog/Benzinpreis/over_dt.rds")


# Autobahn ----------------------------------------------------------------

autobahn_nr <- helfRlein::char_replace(c(
  "A 1",  "A 2",  "A 3",  "A 4",  "A 5",  "A 6",  "A 7",  "A 8",  "A 9", 
  "A 10", "A 11", "A 12", "A 13", "A 14", "A 15", "A 17", "A 19", "A 20",
  "A 21", "A 23", "A 24", "A 25", "A 26", "A 27", "A 28", "A 29", "A 30",
  "A 31", "A 33", "A 37", "A 38", "A 39", "A 40", "A 42", "A 43", "A 44",
  "A 45", "A 46", "A 48", "A 49", "A 52", "A 57", "A 59", "A 60", "A 61",
  "A 62", "A 63", "A 64", "A 65", "A 66", "A 67", "A 70", "A 71", "A 72",
  "A 73", "A 81", "A 92", "A 93", "A 94", "A 95", "A 96", "A 98", "A 99",
  "A 100", "A 103", "A 104", "A 111", "A 113", "A 114", "A 115", "A 117",
  "A 210", "A 215", "A 226", "A 250", "A 252", "A 253", "A 255", "A 261",
  "A 270", "A 280", "A 281", "A 293", "A 352", "A 391", "A 392", "A 395",
  "A 445", "A 480", "A 485", "A 516", "A 524", "A 535", "A 540", "A 542",
  "A 544", "A 553", "A 555", "A 559", "A 560", "A 143", "A 562", "A 565",
  "A 571", "A 573", "A 602", "A 620", "A 623", "A 643", "A 648", "A 650",
  "A 656", "A 659", "A 661", "A 671", "A 672", "A 831", "A 861", "A 864",
  "A 952", "A 980", "A 995",
  "BAB", "AUTOBAHN", "AUTOHOF", "RASTHOF"),
  to_lower = TRUE, rm_dash = TRUE, rm_space = TRUE)

stations_dt[, street := paste0(street, house_number)]
streets <- stations_dt[, unique(street)]
autobahn <- lapply(autobahn_nr, function(x) grep(x, streets, value = TRUE))

no_autobahn <- c("babstadterstr.66")
autobahn <- setdiff(unique(unlist(autobahn)), no_autobahn)
stations_dt[, AUTOBAHN := ifelse(street %in% autobahn, TRUE, FALSE)]

# # from ADAC list
#https://www.adac.de/_mmm/pdf/Online-Liste-Tanken-auf-Reisen-2018-07_51818.pdf
adac_files <- list.files(path = paste0(mainpath, "adac_data"),
                         recursive = TRUE,
                         full.names = TRUE)

adac_data <- helfRlein::read_files(files = adac_files, FUN = fread)
# adjust PLZ and Ort
adac_data[, PLZ_CITY := paste0(PLZ, Ort)]
adac_data[, PLZ_NEW := substr(PLZ_CITY, 1, 5)]
adac_data[, CITY := helfRlein::char_replace(gsub("[[:digit:]]", "", PLZ_CITY),
                                            to_lower = TRUE,
                                            rm_dash = TRUE,
                                            rm_space = TRUE)]
adac_data[, STREET := helfRlein::char_replace(Straße,
                                              to_lower = TRUE,
                                              rm_dash = TRUE,
                                              rm_space = TRUE)]
adac_data[, Marke := toupper(Marke)]

# fix data mistakes
adac_data[CITY == "zorbau" & PLZ_NEW == "06689", PLZ_NEW := "06686"]


adac_tmp <- unique(adac_data[, list(Marke, PLZ_NEW, CITY, STREET)])
for (i.row in 1:nrow(adac_tmp)) {
  # i.row <- 1
  helfRlein::statusbar(run = i.row, max.run = nrow(adac_tmp))
  tmp_dt <- stations_dt[city %in% grep(adac_tmp[i.row, CITY], city, value = TRUE) &
                       post_code == adac_tmp[i.row, PLZ_NEW] &
                       brand == adac_tmp[i.row, Marke], ]
  test_street <- tmp_dt[, paste0(street, " ", house_number)]
  
  street_dist <- stringdist::stringdist(a = test_street, b = adac_tmp[i.row, STREET])
  
  
  # set station as near autobahn
  stations_dt[uuid == tmp_dt[which.min(street_dist), uuid], AUTOBAHN := TRUE]
  
  adac_tmp[i.row, OBS_MIN  := sum(street_dist == min(street_dist))]
  adac_tmp[i.row, OBS      := length(street_dist)]
  adac_tmp[i.row, DIST_MIN := min(street_dist)]
}

print("how many new stations nears the autobahn by using ADAC data")
stations_dt[, table(street %in% autobahn, AUTOBAHN, exclude = NULL)]

print("table with ADAC data infos")
adac_tmp[, table(OBS, DIST_MIN, exclude = NULL)]
adac_tmp[, table(OBS, exclude = NULL)]

## checks
# adac_tmp[DIST_MIN > 20, ]
# 
# adac_tmp[OBS == 5, ]
# adac_tmp[, which(CITY == "bensheim" & Marke == "ARAL")]
# 
# stations_dt[city == "bensheim" & brand == "ARAL",]
# stations_dt[post_code == 6689,]
# adac_data[CITY == "bautzen" & Marke == "ARAL",]
# stations_dt[city %in% grep("zorbau",city, value = TRUE),]

autobahn_stations <- stations_dt[AUTOBAHN == TRUE, unique(uuid)]


# brand -------------------------------------------------------------------

stations_dt[, brand := toupper(brand)]
topbrand <- stations_dt[brand != "", .N, by = brand][order(N, decreasing = TRUE)][1:9, brand]
stations_dt[, BRAND := ifelse(brand %in% topbrand, brand, "other")]
stations_dt[, BRAND := factor(BRAND, levels = c(topbrand, "other"))]

brand_dt <- unique(stations_dt[, .SD, .SDcols = c("uuid", "BRAND")])


# prices ------------------------------------------------------------------

price_path <- "/Users/jakobgepp/Projekte/2018/Intern/Blog/Benzinpreis/prices/"
price_files <- sort(list.files(price_path, pattern = ".csv",
                               full.names = TRUE,
                               recursive = TRUE))
#price_files <- price_files[140:250]
#price_files <- price_files[1:3]
#price_files <- grep("2015-05-26", price_files, value = TRUE)

price_vars <- c("e10", "diesel", "e5")
type_color <- statworx_palette(basecolors = c(3, 6, 10))
bab_color  <- statworx_palette(basecolors = c(7, 2)) # order FALSE, TRUE

# time grid creation
minute <- 60 # grid per x minutes

timegrid <- as.ITime(c(0:(60 / minute * 24 - 1)) * 60 * minute)
timegrid_dt <- as.data.table(expand.grid(
  #TIME = as.POSIXct(paste0(this_day, " 00:00:00"),
  #                  format = "%Y-%m-%d %H:%M:%OS") +
  #  c(0:(60/timegrid*24 - 1))*60*timegrid,
  station_uuid = all_stations,  # price_dt[, unique(station_uuid)]
  TIME = timegrid)) 

#backup.open_dt <- copy(open_dt)
#open_dt<- copy(backup.open_dt)
# adjust time grid per station with opening time
this_key <- c("startp", "endp")
timegrid_dt[, c(this_key) := as.numeric(TIME)]
open_dt[, OPEN := TRUE]
setkeyv(open_dt, c("station_uuid", this_key))

timegrid_dt <- foverlaps(timegrid_dt, open_dt, type = "within")
timegrid_dt <- timegrid_dt[OPEN == TRUE, list(station_uuid, TIME, DAY)]


setkeyv(timegrid_dt, c("station_uuid", "TIME"))


out <- rep(list(data.table()), length(price_files))

last_price_raw <- data.table(station_uuid = all_stations)
last_price_dt <- data.table(station_uuid = all_stations,
                            DATE = as.IDate(NA),
                            TIME = as.ITime("23:59"))
#last_price[, c(paste0("lastprice_", price_vars)) := as.numeric(NA)]
#last_price[, c(price_vars) := as.numeric(NA)]

for (i.file in seq_along(price_files)) {
  # i.file <- grep("2015-09-04", price_files)
  # i.file <- 1
  starttime <- Sys.time()
  if (i.file != 1) {
    meantime <- mean(unlist(lapply(out, "[[", "tracktime")), na.rm = TRUE)
  } else {
    meantime <- Inf
  }
  esttime <- (length(price_files) - i.file) * meantime
  est_hour <- floor(esttime / 60)
  est_min <- floor(esttime - est_hour * 60 )
  est_sec <- (esttime - est_hour * 60  - est_min) * 60
  
  
  this_day <- gsub("-prices.csv", "", basename(price_files[i.file]), fixed = TRUE)
  statusbar(run = i.file, max.run = length(price_files),
            info = paste0(this_day, " | est. time left: ",
                          sprintf("%02.f", est_hour), ":",
                          sprintf("%02.f", est_min), ":",
                          sprintf("%02.f", est_sec), "             "))
  # load price data
  #price_dt <- read_files(price_files,FUN = fread)
  price_dt <- fread(price_files[i.file])
  
  # filter for stations
  price_dt <- price_dt[station_uuid %in% all_stations,]
  
  # check for right prices
  price_dt[, c(price_vars) := lapply(.SD, setNA), .SDcols = price_vars]
  
  if (nrow(price_dt) == 0) {
    print(paste0("skiped this day: ", this_day))
    next() #stop("check what happens next!")
  }
  
  # set time format
  #price_dt[, TIME := as.POSIXct(date)]
  #price_dt[, c("DATE2", "TIME2") := IDateTime(date)]
  price_dt[, DATE := as.IDate(this_day)]
  price_dt[, DATE := as.IDate(date)]
  #wday(this_day)
  
  #price_dt[, TIME := as.ITime(date, format = "%Y-%m-%d %H:%M:%OS")]

  hour_dt <- unique(price_dt[, .SD, .SDcol = "date"])
  #price_dt[, TIME := as.ITime(gsub(this_day, "", date))]
  hour_dt[, TIME := as.ITime(date)]
  price_dt <- merge(price_dt, hour_dt, by = "date", all.x = TRUE)
  
  # order price_dt by key for na.locf
  price_dt <- price_dt[order(station_uuid, DATE, TIME)]
  price_dt[, TIME := TIME - as.numeric(round(TIME)) %% 60]
  price_dt[, date := NULL]
  
  # na.locf 
  # https://stackoverflow.com/questions/37060211/efficiently-locf-by-groups-in-a-single-r-data-table/37068596
  # id_change <- price_dt[, c(TRUE, station_uuid[-1] != station_uuid[-.N])]
  # price_dt[, c(price_vars) := lapply(.SD, function(x) x[cummax(((!is.na(x)) | id_change) * .I)]),
  #          .SDcols = price_vars]
  # 
  
  # price_dt[, c(price_vars) := lapply(.SD, zoo::na.locf, na.rm = FALSE),
  #          .SDcols = price_vars,
  #          by = station_uuid]

  
  
  #############################################################################
  # here we make an error by not using weighted.mean() within an intervall
  # of lentgh minute, this only occurs when there are changes within this intervall
  #############################################################################
  #price_dt[, TIME := round_date(TIME, paste0(timegrid, " mins"))]
  price_dt[, TIME := as.ITime(plyr::round_any(as.numeric(TIME), minute * 60))]
  
  price_dt <- price_dt[, lapply(.SD, last_value, na.rm = TRUE),
                        .SDcols = price_vars,
                        by = c("station_uuid", "DATE", "TIME")]
  
  # add last price
  last_price_dt[is.na(DATE), DATE := as.IDate(this_day) - 1]
  
  price_dt <- rbindlist(list(last_price_dt, price_dt),
                             use.names = TRUE, fill = TRUE)
  
  
  # TODO adjust for overriding opening times
  tmp_timegrid <- timegrid_dt[DAY == weekdays(as.Date(this_day))]
  #tmp_timegrid <- timegrid_dt[DAY == get_wday(as.Date(this_day))]
  
  # this_cols <- copy(names(timegrid_dt))
  # tmp_over_dt <- copy(over_dt[START_DATE <= as.IDate(this_day) &
  #                               END_DATE >= as.IDate(this_day), ])
  # # tmp_timegrid <- tmp_timegrid[station_uuid %in% c("c0ae896c-a64e-4190-aab9-1ebf1907d85f",
  # #                                                  "483ac028-3814-4e77-81a9-3b6398b3e398",
  # #                                                  "f2c8d453-57a4-4450-a9ea-0d7adf95e872")]
  # # 
  # 
  # # adjust time if it does not start or end on this day
  # tmp_over_dt[START_DATE < as.IDate(this_day), START_TIME := as.ITime("00:00")]
  # tmp_over_dt[END_DATE   > as.IDate(this_day), END_TIME   := as.ITime("23:59")]
  # 
  # this_key <- c("START_TIME", "END_TIME")
  # tmp_timegrid[, c(this_key) := as.numeric(TIME)]
  # tmp_over_dt[, c(this_key) := lapply(.SD, as.numeric), .SDcols = this_key]
  # 
  # setkeyv(tmp_over_dt, c("station_uuid", this_key))
  # setkeyv(tmp_timegrid, c("station_uuid", this_key))
  # 
  # tmp_timegrid <- foverlaps(tmp_timegrid, tmp_over_dt, type = "within")
  # 
  # tmp_timegrid[is.na(is_close), is_close := FALSE]
  # tmp_timegrid <- tmp_timegrid[is_close == FALSE, .SD, .SDcols = this_cols]
  # 
  # setkeyv(tmp_timegrid, c("station_uuid", "TIME"))
  
  
  # data per time
  tmp_timegrid[, DATE := as.IDate(this_day)]
  price_time_dt <- merge(tmp_timegrid,
                         price_dt,
                         by = c("station_uuid", "DATE", "TIME"),
                         all = TRUE)
  
  
  
  ## TDOD add first price from last price here
  # set price at 0:0:0 to last price
  
  #last_day <- as.IDate(this_day) - 1
  #last_price[, DATE := last_day]
  
  # first_price <- price_dt[, list(TIME = min(TIME)), by = station_uuid]
  # #first_price <- first_price[ difftime(TIME, this_day, units = "min") > 0,]
  # 
  # first_price <- merge(first_price, last_price, by = "station_uuid", all.x = TRUE)
  # 
  # # if price is NA, take first one
  # first_price <- merge(first_price, price_dt,
  #                      by = c("station_uuid", "TIME"),
  #                      all.x = TRUE) 
  # for (i.var in price_vars) {
  #   # i.var <- price_vars[1]
  #   first_price[!is.na(get(paste0("lastprice_", i.var))),
  #               c(i.var) := get(paste0("lastprice_", i.var))]
  # }
  # first_price[, c(paste0("lastprice_", price_vars)) := NULL]
  # 
  # # set time to 0:0:0
  # #first_price[, TIME := as.POSIXct(paste0(this_day, " 00:00:00"))]
  # first_price[, TIME := as.ITime("00:00:00")]
  
  # update last_price
  next_price <- price_dt[DATE == this_day,
                              list(TIME = max(TIME)), by = list(station_uuid, DATE)]
  next_price <- merge(next_price, price_dt,
                      by = c("station_uuid", "DATE", "TIME"),
                      all.x = TRUE) 
  #next_price <- next_price[, .SD, .SDcols = c("station_uuid", price_vars)]
  last_price_dt <- merge(last_price_raw, next_price, by = c("station_uuid"), all.x = TRUE)
  last_price_dt[is.na(DATE), DATE := as.IDate(this_day)]
  last_price_dt[is.na(TIME), TIME := as.ITime("23:59")]
  
  
  # last_price <- merge(last_price, next_price, by = "station_uuid", all.x = TRUE)
  # for (i.var in price_vars) {
  #   # i.var <- price_vars[1]
  #   last_price[!is.na(get(i.var)), c(paste0("lastprice_", i.var)) := get(i.var)]
  # }
  # last_price[, c(price_vars) := NULL]
  # 
  # combine with price_dt
  #price_dt <- rbind(first_price, price_dt, use.names = TRUE, fill = TRUE)

  
  price_time_dt <- price_time_dt[order(station_uuid, DATE, TIME)]
  
  id_change <- price_time_dt[, c(TRUE, station_uuid[-1] != station_uuid[-.N])]
  price_time_dt[, c(price_vars) := lapply(.SD, function(x) x[cummax(((!is.na(x)) | id_change) * .I)]),
                .SDcols = price_vars]
  
  # remove last day
  price_time_dt <- price_time_dt[DATE == this_day,]
  
  # price_time_dt[, c(price_vars) := lapply(.SD, zoo::na.locf, na.rm = FALSE),
  #               .SDcols = price_vars,
  #               by = station_uuid]
  
  # add autobahn info
  price_time_dt[, AUTOBAHN := FALSE]
  price_time_dt[station_uuid %in% autobahn_stations, AUTOBAHN := TRUE]
  
  # add brand info
  price_time_dt <- merge(price_time_dt, brand_dt,
                         by.x = c("station_uuid"),
                         by.y = c("uuid"),
                         all.x = TRUE)
  
  # add post_code info
  price_time_dt <- merge(price_time_dt,
                         plz_raw,
                         by = "station_uuid", all.x = TRUE)
  
  this_by <- c("DATE", "TIME", "AUTOBAHN", "BRAND", "plz")
  
  
  # price_time_dt22 <- price_time_dt[, stat_fun(.SD),
  #                                 .SDcols = price_vars,
  #                                 by = this_by]
  # 
  
  # setting names = FALSE ist faster than TRUE

  x_mean <- price_time_dt[, lapply(.SD, mean, na.rm = TRUE),
                                  .SDcols = price_vars,
                                  by = this_by]

  x_quant <- price_time_dt[, lapply(.SD, quantile, probs = c(0.1, 0.9),
                                    na.rm = TRUE, names = FALSE),
                           .SDcols = price_vars,
                           by = this_by]
  x_quant[, QUANT := c("Q10", "Q90")]
  eq_quant <- paste0(paste0(this_by, collapse = "+"), "~QUANT")
  x_quant <- dcast(data = x_quant, formula = eq_quant, value.var = price_vars)
  setkeyv(x_quant, NULL)
  
  # x_quant1 <- price_time_dt[, lapply(.SD, quantile, probs = 0.1,
  #                                    na.rm = TRUE, names = FALSE),
  #                             .SDcols = price_vars,
  #                             by = this_by]
  # x_quant9 <- price_time_dt[, lapply(.SD, quantile, probs = 0.9,
  #                                    na.rm = TRUE, names = FALSE),
  #                           .SDcols = price_vars,
  #                           by = this_by]


  # x_min <- suppressWarnings(
  #   price_time_dt[, lapply(.SD, min, na.rm = TRUE),
  #                 .SDcols = price_vars,
  #                 by = this_by])
  # x_max <- suppressWarnings(
  #   price_time_dt[, lapply(.SD, max, na.rm = TRUE),
  #                 .SDcols = price_vars,
  #                 by = this_by])
  x_obs <- price_time_dt[, lapply(.SD, function(y) sum(!is.na(y))),
                            .SDcols = price_vars,
                            by = this_by]
  
  #setnames(x_quant1, price_vars, paste0(price_vars, "_Q10"))
  #setnames(x_quant9, price_vars, paste0(price_vars, "_Q90"))
  setnames(x_mean, price_vars, paste0(price_vars, "_MEAN"))
  #setnames(x_min, price_vars, paste0(price_vars, "_MIN"))
  #setnames(x_max, price_vars, paste0(price_vars, "_MAX"))
  setnames(x_obs, price_vars, paste0(price_vars, "_OBS"))
  
  setkeyv(price_time_dt, this_by)
  #price_time_dt2 <- Reduce(merge, list(x_mean, x_quant1, x_quant9, x_obs))
  price_time_dt2 <- Reduce(merge, list(x_mean, x_quant, x_obs))
  
  # price_time_dt2 <- price_time_dt[, lapply(.SD, mean, na.rm = TRUE),
  #                                 .SDcols = price_vars,
  #                                 by = c("TIME", "AUTOBAHN")]
  # price_time_dt2[, DATE := as.IDate(this_day)]
  # 
  # price_time_dt3 <- price_time_dt[, lapply(.SD, mean, na.rm = TRUE),
  #                                .SDcols = price_vars,
  #                                by = c("station_uuid")]
  # # price_time_dt3 <- price_time_dt[, lapply(.SD, quantile, probs = 0.1, na.rm = TRUE),
  # #                                 .SDcols = price_vars,
  # #                                 by = c("station_uuid")]
  # # price_time_dt3 <- price_time_dt[, stat_fun(.SD),
  # #                                .SDcols = price_vars,
  # #                                by = c("station_uuid")]
  # # 
  # # date per post code
  # price_plz <- merge(price_time_dt3,
  #                    plz_raw,
  #                    by = "station_uuid", all.x = TRUE)
  # price_plz <- price_plz[, lapply(.SD, mean, na.rm = TRUE),
  #                        .SDcols = price_vars,
  #                        by = c("plz")]
  # 
  # price_plz[, DATE := as.IDate(this_day)]
  # 
  
  out[[i.file]] <- list(
    time = copy(price_time_dt2),
    tracktime = as.numeric(difftime(Sys.time(), starttime, units = "min")))
    #plz  = copy(price_plz))
  
  rm(price_time_dt, price_time_dt2, price_dt)
}
cat("\n")

cat("time:", round(sum(unlist(lapply(out, "[[", "tracktime")))), "mins")

saveRDS(out, "/Users/jakobgepp/Projekte/2018/Intern/Blog/Benzinpreis/out_all.rds")
# out <- readRDS("/Users/jakobgepp/Projekte/2018/Intern/Blog/Benzinpreis/out_all.rds")
rm(open_list, bla, bla2, open_dt_tmp, id_change, index, opentime)

all_year <- unique(basename(dirname(dirname(price_files))))
for (i.year in all_year) {
  #i.year <- "2019"
  idx <- grepl(i.year, basename(price_files))
  saveRDS(out[idx], paste0(mainpath, "out_", i.year, ".rds"))
}

this_cols <- setdiff(names(out[[1]]$time), this_by)

nrows <- sum(sapply(out, function(x) nrow(x$time)))
time_dt <- data.table(ID = 1:nrows)
start_row <- 0

rm(out)

for (i.year in all_year) {
  # i.year <- "2014"
  helfRlein::statusbar(run = i.year, max = all_year)
  tmp_out <- readRDS(paste0(mainpath, "out_", i.year, ".rds"))
  
  tmp_out_dt <- rbindlist(lapply(tmp_out, "[[", "time"), use.names = TRUE, fill = TRUE)
  
  end_row <- nrow(tmp_out_dt) + start_row
  time_dt[(start_row + 1):end_row, c(names(tmp_out_dt)) := tmp_out_dt]
  start_row <- end_row
  rm(tmp_out_dt, tmp_out)
}

# removing lines with only 0
# this_cols <- setdiff(names(out[[1]]$time), this_by)
# 
# nrows <- sum(sapply(out, function(x) nrow(x$time)))
# time_dt <- data.table(ID = 1:nrows)
# start_row <- 0
# print("combining the daily results into one table")
# for (i.out in seq_along(out)) {
#   # i.out <- 1
#   helfRlein::statusbar(run = i.out, max.run = length(out))
#   # indx <- out[[i.out]]$time[, rowSums(.SD, na.rm = TRUE) == 0,
#   #                           .SD = this_cols]
#   # if (any(indx)) {
#   #   out[[i.out]]$time <- out[[i.out]]$time[!indx, ]
#   # }
#   # 
#   # time_dt <- rbindlist(list(time_dt, out[[i.out]]$time),
#   #                      use.names = TRUE, fill = TRUE)
#   # 
#   # out[[i.out]]$time <- NULL
#   
# }
# 
# time_dt <- rbindlist(lapply(out, "[[", "time"), use.names = TRUE, fill = TRUE)
# time_dt <- melt(time_dt, id.vars = c("TIME", "DATE", "AUTOBAHN"),
#                 variable.name = "TYPE", value.name = "PRICE")

# TODO can be moven in loop before - to save one reading and writing
# save time_dt by year since RAM is to small
time_dt[, YEAR := year(DATE)]
time_dt[, WDAY := wday(DATE)]

#all_year <- time_dt[, unique(YEAR)]
for (i.year in time_dt[, unique(YEAR)]) {
  # i.year <- time_dt[, unique(YEAR)][1]
  saveRDS(time_dt[YEAR == i.year,], paste0(mainpath, "time_dt_", i.year, ".rds"))
}
rm(time_dt)


# maps --------------------------------------------------------------------

#german <- geocode_OSM("Frankfurt")$bbox
german <- tmaptools::geocode_OSM("Germany")$bbox

# getting map
# plot_map_z7 <- get_stamenmap(as.numeric(german), 
#                              #zoom = "auto",
#                              #scale = 1L,
#                              #force = TRUE, 
#                              maptype = "terrain")

plot_map_z7 <- ggmap::get_map(as.numeric(german),
                              scale = "auto",
                              maptype = "terrain",
                              source = "google")
# ggmap(plot_map_z7) +
#   theme(axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text = element_blank())


# plot stations on the map by autobahn
m1_bab <- ggmap::ggmap(plot_map_z7) +
  geom_point(data = stations_dt[order(AUTOBAHN)], 
             aes(x = longitude, 
                 y = latitude,
                 color = AUTOBAHN)#,
             #color = BRAND) 
             #image = image, 
             #size = I(snow/20))
  ) + # rescaling to get valid size values
  #scale_color_manual(values = rev(statworx_palette(basecolors = c(4, 5))),
  scale_color_manual(values = bab_color,
                     name = "gas staition is",
                     breaks = c("TRUE", "FALSE"),
                     labels = c("near a motorway", "somewhere else")) + 
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank())

plot(m1_bab)
ggsave(plot = m1_bab, filename = paste0(plotpath, "station-location-bab.png"))

# plot stations on the map by brand
m2_brand <- ggmap::ggmap(plot_map_z7) +
  geom_point(data = stations_dt, 
             aes(x = longitude, 
                 y = latitude,
                 color = BRAND)#,
             #color = BRAND) 
             #image = image, 
             #size = I(snow/20))
  ) + # rescaling to get valid size values
  #scale_color_manual(values = rev(statworx_palette(basecolors = c(4, 5))),
  # scale_color_manual(values = bab_color,
  #                    name = "gas staition is",
  #                    breaks = c("TRUE", "FALSE"),
  #                    labels = c("near a motorway", "somewhere else")) + 
  scale_color_manual(values = statworx_palette(10), name = "") + 
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "right",
        panel.grid.minor = element_blank())

plot(m2_brand)
ggsave(plot = m2_brand, filename = paste0(plotpath, "station-location-brand.png"))



# plot prices -------------------------------------------------------------
time_dt_files <- paste0(mainpath, "time_dt_", all_year, ".rds")

# prices over dates
#plotdata <- agg_price(x = time_dt, by = c("DATE", "TYPE"))
plotdata <- agg_price2(files = time_dt_files, by = c("DATE", "TYPE"))

g0 <- ggplot(data = plotdata, aes(x = DATE)) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = TYPE, group = TYPE), alpha = 0.1) + 
  geom_line(aes(y = PRICE, color = TYPE)) +
  ggtitle("gas prices from 2014 till 2019") +
  xlab(NULL) + 
  ylab("Euro") + 
  theme_minimal() +
  scale_color_manual(values = type_color, name = "") + 
  scale_fill_manual(values = type_color, name = "") + 
  scale_y_continuous(labels = scaleFUN) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())
plot(g0)
ggsave(plot = g0, filename = paste0(plotpath, "price-timeseries.png"))



# prices during the day by year
#plotdata <- agg_price(x = time_dt, by = c("TIME", "TYPE", "YEAR"))
plotdata <- agg_price2(files = time_dt_files, by = c("TIME", "TYPE", "YEAR"))

plotdata[, TIME := as.POSIXct(paste0("2019-02-11 ",
                                     hour(TIME), ":",
                                     minute(TIME), ":",
                                     second(TIME)))]

g1 <- ggplot(data = plotdata, aes(x = TIME)) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = TYPE, group = TYPE), alpha = 0.1) + 
  geom_line(aes(y = PRICE, color = TYPE)) +
  facet_wrap(~YEAR) +
  scale_x_datetime(date_labels = "%H:%M") +
  ggtitle("gas prices over the period of one day") +
  xlab(NULL) + 
  ylab("Euro") + 
  theme_minimal() +
  scale_color_manual(values = type_color, name = "") + 
  scale_fill_manual(values = type_color, name = "") + 
  scale_y_continuous(labels = scaleFUN) +
  theme(legend.position = "bottom",
      panel.grid.minor = element_blank())
plot(g1)
ggsave(plot = g1, filename = paste0(plotpath, "price-by-daytime-year.png"))


# scaled prices
plotdata <- agg_price2(files = time_dt_files, by = c("TIME", "TYPE", "YEAR"),
                       scaled = TRUE, scaledby = c("TYPE", "DATE"))

# plotdata <- copy(time_dt)
# plotdata[, PRICE_scale := PRICE - weighted.mean(PRICE, OBS, na.rm = TRUE),
#          by = list(TYPE, DATE)]
# plotdata <- plotdata[, list("PRICE_scale" = weighted.mean(PRICE_scale, OBS, na.rm = TRUE)),
#                      by = list(TIME, TYPE, YEAR)]
plotdata[, TIME := as.POSIXct(paste0("2019-02-11 ",
                                     hour(TIME), ":",
                                     minute(TIME), ":",
                                     second(TIME)))]

g1_norm <- ggplot(plotdata, aes(x = TIME)) +
  #geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = TYPE, group = TYPE), alpha = 0.1) + 
  geom_line(aes(y = PRICE, color = TYPE)) +
  facet_wrap(~YEAR) +
  scale_x_datetime(date_labels = "%H:%M") +
  ggtitle("scaled gas prices over the period of one day") +
  theme_minimal() +
  xlab(NULL) + 
  ylab("difference to the daily mean in Euro") + 
  theme_minimal() +
  scale_color_manual(values = type_color, name = "") + 
  scale_fill_manual(values = type_color, name = "") + 
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())
plot(g1_norm)
ggsave(plot = g1_norm, filename = paste0(plotpath, "price-norm-by-daytime.png"))

## by BAB vs. non BAB
# TODO check colors and values
#plotdata <- agg_price(x = time_dt, by = c("TIME", "TYPE", "YEAR", "AUTOBAHN"))
plotdata <- agg_price2(files = time_dt_files, by = c("TIME", "TYPE", "YEAR", "AUTOBAHN"))

plotdata <- agg_price2(files = time_dt_files, by = c("TYPE", "YEAR", "AUTOBAHN", "BRAND"))

plotdata[, TIME := as.POSIXct(paste0("2019-02-11 ",
                                     hour(TIME), ":",
                                     minute(TIME), ":",
                                     second(TIME)))]

g1_bab <- ggplot(data = plotdata, aes(x = TIME)) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, group = AUTOBAHN, fill = AUTOBAHN), alpha = 0.1) + 
  geom_line(aes(y = PRICE, color = AUTOBAHN)) +
  facet_grid(TYPE ~ YEAR) +
  scale_fill_manual(values = bab_color, name = "") + 
  scale_color_manual(values = bab_color,
                     name = "gas staition is",
                     breaks = c("TRUE", "FALSE"),
                     labels = c("near a motorway", "somewhere else")) +
  guides(fill = "none") +
  scale_x_datetime(date_labels = "%H:%M") +
  xlab(NULL) + 
  ylab("Euro") + 
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())
plot(g1_bab)
ggsave(plot = g1_bab, filename = paste0(plotpath, "price-by-daytime-year-bab.png"))

# plotdata <- copy(plz_dt)
# g2 <- ggplot(data = plotdata, aes(x = DATE, y = PRICE, color = plz)) +
#    geom_line(aes(group = plz)) +
#    facet_wrap(~TYPE)


## by weekdays
# plotdata <- copy(time_dt)
# plotdata[, WDAY := wday(DATE)]
# plotdata <- agg_price(x = plotdata, by = c("TIME", "TYPE", "WDAY"))
# 
# plotdata[, TIME := as.POSIXct(paste0("2018-07-0", WDAY, " ",
#                                      hour(TIME), ":",
#                                      minute(TIME), ":",
#                                      second(TIME)))]
# 
# g1_wday <- ggplot(data = plotdata, aes(x = TIME)) +
#   geom_ribbon(aes(ymin = Q10, ymax = Q90, group = TYPE, fill = TYPE), alpha = 0.1) +
#   geom_line(aes(y = PRICE, color = TYPE)) +
#   scale_x_datetime(date_labels = "%A") +
#   ggtitle("Price pattern over weekdays") +
#   xlab(NULL) + 
#   ylab("Euro") + 
#   theme_minimal() +
#   scale_color_manual(values = type_color, name = "") + 
#   scale_fill_manual(values = type_color, name = "") + 
#   scale_y_continuous(labels = scaleFUN) +
#   theme(legend.position = "bottom",
#         panel.grid.minor = element_blank())

#plotdata <- agg_price(x = plotdata, by = c("TIME", "WDAY", "YEAR"))
plotdata <- agg_price2(files = time_dt_files, by = c("TIME", "WDAY", "YEAR", "TYPE"))
bla <- copy(plotdata)
#plotdata <- copy(bla)
plotdata[, TIME := as.POSIXct(paste0("2019-02-11 ",
                                     hour(TIME), ":",
                                     minute(TIME), ":",
                                     second(TIME)))]
plotdata <- plotdata[order(WDAY, TIME, YEAR, TYPE)]
#plotdata[, CHEAPDAY := which.min(PRICE) == WDAY, by = list(TIME, YEAR, TYPE)]
plotdata[, CHEAPDAY := PRICE == min(PRICE), by = list(TIME, YEAR, TYPE)]
plotdata <- plotdata[CHEAPDAY == TRUE, ]
wday_label <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
                "Friday", "Saturday")

plotdata[, WDAY := factor(WDAY, levels = 1:7, labels = wday_label, ordered = TRUE)]
plotdata[, YEAR := as.ordered(YEAR)]

g1_wday <- ggplot(plotdata, aes(x = TIME, y = YEAR)) +
  geom_tile(aes(fill = WDAY)) +
  scale_fill_manual(values = statworx_palette(7, basecolors = c(4,9,3))) +
  #scale_fill_brewer(palette = "Greens") +
  scale_x_datetime(date_labels = "%H:%M") +
  ggtitle("cheapest weekdays") +
  xlab(NULL) + 
  ylab(NULL) + 
  theme_minimal() +
  theme(legend.position = "right") +
  facet_wrap(~TYPE)
plot(g1_wday)
ggsave(plot = g1_wday, filename = paste0(plotpath, "price-by-week-year.png"))

# 
# 
# plotdata[, WDAY := as.factor(WDAY)]
# g1_wday <- ggplot(data = plotdata, aes(x = TIME)) +
#   #geom_ribbon(aes(ymin = Q10, ymax = Q90, group = WDAY, fill = WDAY), alpha = 0.1) + 
#   geom_line(aes(y = PRICE, color = WDAY)) +
#   #scale_fill_manual(values = type_color) +
#   #scale_color_manual(values = type_color) +
#   scale_x_datetime(date_labels = "%H") +
#   ggtitle("Price pattern over weekdays") +
#   facet_grid(TYPE ~YEAR) +
#   xlab(NULL) + 
#   ylab("Euro") + 
#   theme_minimal() +
#   scale_color_manual(values = type_color, name = "") + 
#   scale_fill_manual(values = type_color, name = "") + 
#   scale_y_continuous(labels = scaleFUN) +
#   theme(legend.position = "bottom",
#         panel.grid.minor = element_blank())
# ggsave(plot = g1_wday, filename = paste0(plotpath, "price-by-week-year.png"))


## by brands
plotdata <- agg_price2(files = time_dt_files, by = c("DATE", "TYPE", "BRAND"),
                       scaledby = c("TYPE", "DATE"))

# plotdata <- copy(time_dt)
# plotdata[, PRICE_scale := PRICE - weighted.mean(PRICE, OBS, na.rm = TRUE),
#          by = list(TYPE, DATE)]
# plotdata <- plotdata[, list("PRICE_scale" = weighted.mean(PRICE_scale, OBS, na.rm = TRUE)),
#                      by = list(DATE, TYPE, BRAND)]

#plotdata <- agg_price(x = time_dt, by = c("DATE", "TYPE", "BRAND", "YEAR"))
topbrand_tmp <- stations_dt[, .N, by = BRAND][order(BRAND)]
topbrand_tmp[, LABEL := paste0(BRAND, " (", N, ")")]
plotdata[, BRAND := factor(BRAND, 
                           levels = topbrand_tmp$BRAND,
                           labels = topbrand_tmp$LABEL)]


g1_brand <- ggplot(data = plotdata, aes(x = DATE)) +
  #geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = BRAND, group = BRAND), alpha = 0.1) + 
  geom_line(aes(y = PRICE, color = TYPE)) +
  geom_hline(yintercept = 0, color = "grey70") + 
  facet_wrap(~BRAND, nrow = 2) +
  ggtitle("brand differences to daily mean") +
  xlab(NULL) + 
  ylab("difference in Euro") + 
  theme_minimal() +
  scale_color_manual(values = type_color, name = "") + 
  #scale_fill_manual(values = type_color, name = "") + 
  #scale_y_continuous(labels = scaleFUN) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())
plot(g1_brand)
ggsave(plot = g1_brand, filename = paste0(plotpath, "price-brands.png"))



## difference between price_vars
#plotdata <- agg_price(x = time_dt, by = c("DATE", "TYPE"))[order(DATE, TYPE)]
plotdata <- agg_price2(files = time_dt_files, by = c("DATE", "TYPE"))[order(DATE, TYPE)]

basetype <- "e10"
plotdata[, BASE := mean(ifelse(TYPE == basetype, PRICE, NA ), na.rm = TRUE), by = DATE]
plotdata[, PRICE_DIFF := (PRICE - BASE) / BASE * 100]


g1_diff <- ggplot(data = plotdata, aes(x = DATE)) +
  geom_line(aes(y = PRICE_DIFF, color = TYPE)) +
  ggtitle("differences in gas types") +
  xlab(NULL) + 
  ylab("%") + 
  theme_minimal() +
  scale_color_manual(values = type_color, name = "") + 
  # scale_fill_manual(values = type_color, name = "") + 
  scale_y_continuous(labels = scaleFUN) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())
plot(g1_diff)
ggsave(plot = g1_diff, filename = paste0(plotpath, "type-difference.png"))


#  zip codes --------------------------------------------------------------

# https://arilamstein.com/blog/2016/05/16/use-case-study-mapping-german-zip-codes/

# https://cebus.net/de/plz-bundesland.htm
#shapefile from https://datahub.io/de/dataset/postal-codes-de
#http://www.suche-postleitzahl.org/downloads?download=zuordnung_plz_ort.csv
#post questions here: http://gis.stackexchange.com/

#ger_plz <- readOGR(dsn = "/Users/jakobgepp/Projekte/2018/Intern/Blog/Benzinpreis/plz-gebiete.shp/",
#                   layer = "plz-gebiete")
ger_plz <- rgdal::readOGR(dsn = "/Users/jakobgepp/Projekte/2018/Intern/Blog/Benzinpreis/plz-2stellig.shp/",
                          layer = "plz-2stellig")

#gpclibPermit()
#convert the raw data to a data.frame as ggplot works on data.frames
ger_plz@data$id <- rownames(ger_plz@data)
ger_plz.point <- ggplot2::fortify(ger_plz, region="id")
ger_plz.df <- as.data.table(dplyr::inner_join(ger_plz.point, ger_plz@data, by="id"))


used_plz <- copy(ger_plz.df)
used_plz[, plz_char := as.character(plz)]



# plz_dt <- rbindlist(lapply(out, "[[", "plz"), use.names = TRUE, fill = TRUE)
# plz_dt <- melt(plz_dt, id.vars = c("plz", "DATE"),
#                variable.name = "TYPE", value.name = "PRICE")


#plz_dt <- agg_price(time_dt, by = c("plz", "TYPE", "YEAR"))
plz_dt <- agg_price2(files = time_dt_files, by = c("plz", "TYPE", "YEAR"))
#used_plz <- used_plz[plz_char %in% plz_dt[, unique(post_code)],]
used_plz <- used_plz[plz_char %in% unique(substr(plz_dt[, unique(plz)], 1,2)),]

# mean over DATE by year
plz_dt2 <- copy(plz_dt)
#plz_dt2[, YEAR := year(DATE)]
# plz_dt2 <- plz_dt2[, list("PRICE" = mean(PRICE, na.rm = TRUE)),
#                      by = c("plz", "YEAR", "TYPE")]

used_plz <- merge(used_plz, plz_dt2,
                              by.x = "plz_char",
                              by.y = "plz",
                              all.x = TRUE,
                              allow.cartesian = TRUE)

g3 <- ggplot(used_plz[order %% 40 == 0 & TYPE == "e10",],
       aes(long, lat, group=group, fill = PRICE)) +
  geom_polygon() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  ggtitle("mean e10 price by post code") +
  facet_wrap(~YEAR)
plot(g3)
ggsave(plot = g3, filename = paste0(plotpath, "meanprice-by-plz.png"))

