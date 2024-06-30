#rm(list=ls())
library(tigris)
library(sf)
library(leaflet)
library(tidycensus)
library(osrm)
library(acs)
library(tidyr)
library(dplyr)
library(DT)
library(sjmisc)
###Used renv package to isolate project folder. Ran install, init, and isolate in succession
###to bring packages with correct versions to internal folders that can be brought to server
#renv::install()
#renv::init()
#renv::isolate()
#renv::status()


readRenviron(".Renviron")
KEY <- Sys.getenv("KEY")
recent_year <- 2022
###here here###here platform
api.key.install(key=KEY)
census_api_key(KEY)
sf_use_s2(FALSE)

tab_names <- c(
  "GEOID", "Census Tract", "Population", "Associate Degrees",
  "Bachelor's Degrees", "Master's Degrees", "Professional Degrees",
  "Doctorate Degrees", "All with at Least Bachalor's Degrees", 
  "STEM Bachelor's: Computers, Mathematics, and Statistics",
  "STEM Bachelor's: Biological, Agricultural, and Environmental Sciences",
  "STEM Bachelor's: Physical and Related Sciences",
  "STEM Bachelor's: Psychology",
  "STEM Bachelor's: Social Sciences",
  "STEM Bachelor's: Engineering",
  "STEM Bachelor's: Multidisciplinary Studies",
  "STEM Bachelor's: Other Science and Engineering Fields",
  "All Bachelor's: Business",
  "All Bachelor's: Education",
  "Arts and Humanities Bachelor's: Literature and Languages",
  "Arts and Humanities Bachelor's: Liberal Arts and History",
  "Arts and Humanities Bachelor's: Visual and Performing Arts",
  "Arts and Humanities Bachelor's: Communications",
  "Arts and Humanities Bachelor's: Other Arts and Humanities",
  "Veterans",
  "Labor Force",
  "Employed",
  "Unemployed",
  "High School or Less", "Some College", 
  "All Bachelor's: STEM",
  "All Bachelor's: Arts and Humanities",
  "geometry")

get_drive_radius <- function(long, lat){
  osrmIsochrone(loc = c(long, lat),
                breaks = seq(from = 20, to = 100, 20), res = 25)
}
create_drive_map <- function(radius_shapes, long, lat, view) {
  leaflet() %>%
    setView(long, lat, zoom = view) %>%
    addProviderTiles(providers$Stadia.OSMBright) %>%
    addMarkers(lng = long, lat = lat) %>%
    addPolygons(fill=TRUE, stroke=TRUE, color = "black",
                weight=0.5, fillOpacity=0.2,
                data = radius_shapes) %>%
    addScaleBar() %>%
    return()
}
get_states_vector <- function(sf_dr){
  states_sf <-states() %>%
    st_transform(crs = st_crs(sf_dr)) %>%
    st_filter(sf_dr)
  return(states_sf$STATEFP)
}
get_acs_data <- function(states, sf_dr){
  variables <- load_variables(year = recent_year, "acs5", cache = TRUE) %>%
    mutate(label = paste0(str_replace_all(label, "!+|:!+", "_"),
                          str_replace_all(str_replace_all(concept,
                                                          "Educational Attainment for the Population 25 Years and Over",
                                                          "ed_att"),
                                          "Total Fields",
                                          ""), sep = "_")) %>%
    select(c(name, label)) %>%
    rename(variable = name) %>%
    mutate(label = str_replace_all(label, "Estimate_Total_", ""))
  census_request <- function(state){
    df <- get_acs(geography = 'tract', variables = c("B21003_002", "B15003_001",
                                                     "B15003_002", "B15003_003",
                                                     "B15003_004", "B15003_005",
                                                     "B15003_006", "B15003_007",
                                                     "B15003_008", "B15003_009",
                                                     "B15003_010", "B15003_011",
                                                     "B15003_012", "B15003_013",
                                                     "B15003_014", "B15003_015",
                                                     "B15003_016", "B15003_017",
                                                     "B15003_018", "B15003_019",
                                                     "B15003_020", "B15003_021",
                                                     "B15003_022", "B15003_023",
                                                     "B15003_024", "B15003_025",
                                                     "B15012_001", "B15012_002",
                                                     "B15012_003", "B15012_004",
                                                     "B15012_005", "B15012_006",
                                                     "B15012_007", "B15012_008",
                                                     "B15012_009", "B15012_010",
                                                     "B15012_011", "B15012_012",
                                                     "B15012_013", "B15012_014",
                                                     "B15012_015", "B15012_016",
                                                     "B23025_003",
                                                     "B23025_004", "B23025_005"),
                  state = state, year = 2022) %>%
      select(-c(moe))
    left_join(df, variables, by = c("variable")) %>%
      pivot_wider(names_from = label, values_from = estimate,
                  values_fn = ~sum(.x, na.rm = TRUE)) %>%
      select(-c(variable)) %>%
      group_by(GEOID, NAME) %>%
      summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) %>%
      return()
  }
  for(state in states){
    if(!exists("state_acs")){
      state_acs <- census_request(as.character(state))
    }
    else{
      state_acs <- rbind(state_acs,
                         census_request(as.character(state)))
    }
    if(!exists("tracts_sf")){
      tracts_sf <- tracts(state)
    }
    else{
      tracts_sf <- rbind(tracts_sf, tracts(state))
    }
  }
  sf_merged <- merge(tracts_sf, state_acs, by = "GEOID") %>%
    st_transform(crs = st_crs(sf_dr))
  return(sf_merged)
}
get_output_sf <- function(sf_acs, sf_dr){
  sf_acs %>%
    st_filter(sf_dr) %>%
    return()
}
create_tract_map <- function(sf_output, long, lat){
  add_point_pop <- function(census_tract, labor_force, employed, unemployed, all_at_least_bachelors){
    paste(census_tract, "<br>",
          "Labor Force: ", big_mark(labor_force), 
          "<br>Employed: ", big_mark(employed),
          "<br>Unemployed: ", big_mark(unemployed),
          "<br>Total Population with Bachelor's Degree: ", big_mark(all_at_least_bachelors))
  }
  leaflet() %>%
    #setView(lng = long, lat = lat, zoom = 9) %>%
    #flyToBounds(lng1 = st_bbox(sf_output)$xmin, lng2 = st_bbox(sf_output)$xmax,
    #          lat1 = st_bbox(sf_output)$ymin, lat2 = st_bbox(sf_output)$ymax) %>%
    addProviderTiles(providers$Stadia.OSMBright) %>%
    addMarkers(lng = long, lat = lat, 
               popup = paste("<b>Drive Radius Totals</b><br>Labor Force: ",
                             big_mark(max(sf_output$`Labor Force`)),
                             "<br>Employed: ", 
                             big_mark(max(sf_output$Employed)),
                             "<br>Unemployed: ",
                             big_mark(max(sf_output$Unemployed)),
                             "<br>Total Population with Bachelor's Degree: ",
                             big_mark(max(sf_output$`All with at Least Bachalor's Degrees`)))) %>%
    addPolygons(fill = TRUE, data = sf_output,
                popup = ~add_point_pop(`Census Tract`, `Labor Force`,
                                       Employed, Unemployed, 
                                       `All with at Least Bachalor's Degrees`)) %>%
    addScaleBar() %>%
    return()
}
create_table_data <- function(sf_output){
  
  return_df <- sf_output %>%
    mutate(`High School or less` = rowSums(across(
      `No schooling completeded_att_`:`GED or alternative credentialed_att_`)),
      `Some College` = rowSums(across(`Some college, less than 1 yeared_att_`:
                                        `Some college, 1 or more years, no degreeed_att_`)),
      `Science and Engineering` = rowSums(across(`Science and Engineering_Computers, Mathematics and Statistics of Bachelor's Degrees Reported_`:
                                                   `Science and Engineering Related Fields of Bachelor's Degrees Reported_`)),
      `Arts, Humanities, and Other` = rowSums(across(`Arts, Humanities, and Other_Literature and Languages of Bachelor's Degrees Reported_`:
                                                       `Arts, Humanities, and Other_Other of Bachelor's Degrees Reported_`)),
      GEOID = as.character(GEOID)) %>%
    select(-c(`No schooling completeded_att_`:`GED or alternative credentialed_att_`,
              `Some college, less than 1 yeared_att_`:
                `Some college, 1 or more years, no degreeed_att_`,
              ALAND, AWATER, INTPTLAT, INTPTLON, STATEFP, COUNTYFP,
              TRACTCE, NAME.x, NAMELSAD, MTFCC, FUNCSTAT))
  
  names(return_df) <- tab_names
  
  return_df <- return_df %>% select(
    `Census Tract`, `Labor Force`, Employed, Unemployed, 
    `Population`,`Veterans`, `High School or Less`, 
    `Some College`,  `Associate Degrees`,
    `Bachelor's Degrees`, `Master's Degrees`, `Professional Degrees`,
    `Doctorate Degrees`, 
    `All with at Least Bachalor's Degrees`, 
    `All Bachelor's: STEM`,
    `All Bachelor's: Arts and Humanities`,
    `All Bachelor's: Business`,
    `All Bachelor's: Education`,
    `STEM Bachelor's: Computers, Mathematics, and Statistics`,
    `STEM Bachelor's: Biological, Agricultural, and Environmental Sciences`,
    `STEM Bachelor's: Physical and Related Sciences`,
    `STEM Bachelor's: Psychology`,
    `STEM Bachelor's: Social Sciences`,
    `STEM Bachelor's: Engineering`,
    `STEM Bachelor's: Multidisciplinary Studies`,
    `STEM Bachelor's: Other Science and Engineering Fields`,
    `Arts and Humanities Bachelor's: Literature and Languages`,
    `Arts and Humanities Bachelor's: Liberal Arts and History`,
    `Arts and Humanities Bachelor's: Visual and Performing Arts`,
    `Arts and Humanities Bachelor's: Communications`,
    `Arts and Humanities Bachelor's: Other Arts and Humanities`, GEOID,
    geometry) %>%
    summarise(across(where(is.character), ~"Totals"),
              across(where(is.numeric), sum)) %>%
    rbind(return_df) %>%
    select(
      `Census Tract`, `Labor Force`, Employed, Unemployed, 
      `Population`,`Veterans`, `High School or Less`, 
      `Some College`,  `Associate Degrees`,
      `Bachelor's Degrees`, `Master's Degrees`, `Professional Degrees`,
      `Doctorate Degrees`, 
      `All with at Least Bachalor's Degrees`, 
      `All Bachelor's: STEM`,
      `All Bachelor's: Arts and Humanities`,
      `All Bachelor's: Business`,
      `All Bachelor's: Education`,
      `STEM Bachelor's: Computers, Mathematics, and Statistics`,
      `STEM Bachelor's: Biological, Agricultural, and Environmental Sciences`,
      `STEM Bachelor's: Physical and Related Sciences`,
      `STEM Bachelor's: Psychology`,
      `STEM Bachelor's: Social Sciences`,
      `STEM Bachelor's: Engineering`,
      `STEM Bachelor's: Multidisciplinary Studies`,
      `STEM Bachelor's: Other Science and Engineering Fields`,
      `Arts and Humanities Bachelor's: Literature and Languages`,
      `Arts and Humanities Bachelor's: Liberal Arts and History`,
      `Arts and Humanities Bachelor's: Visual and Performing Arts`,
      `Arts and Humanities Bachelor's: Communications`,
      `Arts and Humanities Bachelor's: Other Arts and Humanities`, GEOID,
      geometry) %>%
    mutate(GEOID = str_replace(GEOID, "Totals", " ")) %>%
    return()
}
create_dt_table <- function(sf_output){
  dt_names <- head(names(sf_output)[-1], -1)
  table_head <- htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Census Tract'),
        th(colspan = 3, 'Labor Force Ages 16 and Over'),
        th(colspan = 9, 'Age 25 and Older'),
        th(colspan = 19, '')
      ),
      tr(
        lapply(dt_names, th)
      )
    )
  ))
  
  sf_output %>%
    st_drop_geometry() %>%
    datatable(rownames = FALSE,
              extensions = c("Responsive"),
              container = table_head, 
              options = list(
                pageLength = 50)) %>%
    formatRound(head(dt_names, -1), digits = 0) %>%
    formatStyle(c(4, 13), `border-right` = "solid 2px") %>%
    return()
}

#point <- c(-81.04689881123234, 34.03247918860177)
#radius <- get_drive_radius_hereR(point[1], point[2])

##retrieve sf drive radius
#sf_drive_radius <- get_drive_radius(point[1], point[2])

##graph drive time geographies
#create_drive_map(sf_drive_radius, point[1], point[2])
###get states
#radius_states <- get_states_vector(sf_drive_radius)

##get acs data
#sf_acs <- get_acs_data(radius_states, sf_drive_radius)

##filter by
#output_sf <- get_output_sf(sf_acs, sf_drive_radius)

##create visually acceptable table shape object
#output_sf <- create_table_data(output_sf)
#sf_output <- output_sf
#class(st_bbox(sf_output)$xmin)
#create_tract_map(output_sf, point[1], point[2])

##turn shape 
#output_dt <- create_dt_table(output_sf)
usethis::use_git_config(user.name = "Daniel Lyons", 
                         user.email = "djlyons85@gmail.com")
