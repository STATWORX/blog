library(dplyr)
library(jsonlite)
library(httr)
library(knitr)
library(ggplot2)
library(kableExtra)
library(forcats)

# load data
bookings <- read.csv("data/hotel_bookings.csv")

# get total number of bookings by hotel and country
no_bookings <- bookings %>% 
  dplyr::filter(is_canceled == 0) %>% 
  dplyr::group_by(hotel, country) %>% 
  dplyr::count() %>% 
  dplyr::ungroup() %>% 
  dplyr::rename(bookings = n)

# total revenue table by country
revenue_table <- bookings %>% 
  dplyr::filter(is_canceled == 0 & country != "NULL") %>% 
  dplyr::group_by(hotel, market_segment, distribution_channel,
                  deposit_type, customer_type, country) %>% 
  dplyr::summarise(revenue = sum(adr, na.rm = TRUE)) %>% 
  dplyr::ungroup()

#' Log request information
#' @filter log
function(req){
  cat(
    "Time of request:", as.character(Sys.time()),"\n",
    "Method:", req$REQUEST_METHOD, "\n",
    "Path info:", req$PATH_INFO, "\n",
    "User agent:", req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n"
  )
  plumber::forward()
}

#' Barchart
#' @png (width=800, height=600)
#' @param min_bookings
#' @param my_country
#' @get /plot
function(req, res, min_bookings, my_country){
  
  # throw error
  if(missing(min_bookings)){
    stop(message("min_bookings is missing"))
  }
  
  # cast input to numeric
  if(!is.numeric(min_bookings)){
    min_bookings <- as.numeric(min_bookings)
  }

  # generate barchart
  p1 <- no_bookings %>% 
    dplyr::filter(bookings >= min_bookings) %>% 
    ggplot2::ggplot(
      mapping = aes(
        x = forcats::fct_reorder(country, bookings),
        y = bookings, 
        fill = hotel
      )
    ) + 
    ggplot2::geom_bar(stat="identity", position = "dodge") + 
    ggplot2::coord_flip() + 
    ggplot2::theme_minimal() +
    ggplot2::labs(
      y = "Number of bookings", 
      x = "Country", 
      fill = "Hotel type", 
      title = paste("Countries with at least ", min_bookings, "bookings")
    )
  
  # use print to render plot
  print(p1)
}


#' Filter revenue table
#' @serializer html
#' @param mycountry
#' @get /revenue
#' @post /revenue
function(req, res, mycountry){
  
  # throw error
  if(missing(mycountry)){
    stop(message("country is missing"))
  }
  
  # render html table
  revenue_table %>% 
    dplyr::filter(country == mycountry) %>% 
    knitr::kable(format = "html")
}
