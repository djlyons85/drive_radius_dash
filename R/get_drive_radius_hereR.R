#rm(list = ls())
library(tidyverse)
library(hereR)
library(sf)
library(lubridate)
readRenviron(".Renviron")
HERE_KEY <- Sys.getenv("HERER_KEY")
set_key(HERE_KEY)
get_drive_radius_hereR <- function(long, lat){
  point <- data.frame(lat = lat, long = long) %>%
    st_as_sf(coords = c("long", "lat"), remove = FALSE, crs="EPSG:4326")
  traffic_date <- as.POSIXct(paste(floor_date(Sys.Date(), "weeks", 
                                              week_start = 3), 
                                   "17:00:00 EST5EDT", sep = " "))
  isoline(point, range = seq(from = 20, to = 100, 20) * 60, 
                 datetime = traffic_date) %>%
    rename(radius_range = range) %>%
    select(-c(departure, arrival)) %>%
    return()
}
