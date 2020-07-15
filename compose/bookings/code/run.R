library(plumber)

# create api
hotel_api <- plumber::plumb("code/api.R")

# run api
hotel_api$run(host = "0.0.0.0", port = 6060)