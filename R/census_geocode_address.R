library(httr)
library(jsonlite)
library(stringr)

##code is set to only handle SC addresses
##add a state field to expand to other states
##returns try-catch error if still error after 6 tries varying the input filters
#address <- "5855 n melvina"
#city <- "hillsdale"
#state <- "il"

census_geocode_address <- function(address, city, state){
  address <- str_replace_all(str_remove(str_trim(address), "\\."), 
                                       " ", "+")
  city <- str_replace_all(str_trim(str_remove(str_remove(city, ",.+")
                                                        ,"\\..+")), " ", "+")

  request <- paste0("https://geocoding.geo.census.gov/geocoder/geographies/address?",
                    "street=", address, "&city=", city,
                    "&state=", state, "&benchmark=Public_AR_Census2020&vintage=",
                    "Census2020_Census2020&layers=10&format=json",
                    sep = "")
  census_results <- GET(request)
  results <- try(as.data.frame(fromJSON(rawToChar(census_results$content))))
  if("try-error" %in% class(results)){
    return("There are no results for this address.")
  }
  if(nrow(results) != 1){
    return("There are no results for this address.")
  }
  return(data.frame("long" = results$result.addressMatches.coordinates$x, 
                     "lat" = results$result.addressMatches.coordinates$y,
                     "matched_address" = 
                       results$result.addressMatches.matchedAddress))
  }

#test <- census_geocode_address("5855 n melvina", "columbia", "IL")
#address <- "5855 n melvina"
#city <- "columbia"
#state <- "IL"

