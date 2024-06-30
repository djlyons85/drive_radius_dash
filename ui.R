#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shinydashboard)
library(shinyWidgets)
library(DT)
library(mapview)

#usethis::edit_r_environ('project')
# Define UI for application that draws a histogram
?dashboardHeader
dashboardPage(
  dashboardHeader(title = "Drive Time Workforce Information",
                  tags$li(class = "dropdown",
                          dropMenu(
                            dropdownButton("Info", status = 'success', icon = icon('info')),
                            h3(strong('How to use')),
                            br(),
                            h4("Explanation of Panes"),
                            h5('SELECT LOCATION: Enter a central location by address or coordinates and the maximum drive time. The maximum drive time is an amount of time in minutes that is used to calculate the drive time radius'),
                            br(),
                            h5("DRIVE TIME AREAS: Displays the drive time radius, the area that can be reached by motor vehicle in the amount of time selected as the maximum drive time. Successive shades indicate 20 minute travel time intervals with the complete shaded area representing the maximum drive time radius."),
                            br(),
                            h5("OVERLAPPING CENSUS TRACTS: Displays the census tracts that overlap with the drive time radius. Tracts are matched if any area of the tract touches the drive time radius."),
                            br(),
                            h5("CENSUS TRACT DATA TABLE: Displays the sum totals and individual tract level census ACS data in the overlapping census tracts. The table currently covers labor force and education data. Please contact us to request additional Census data."),
                            br(),
                            h5("Sources: U.S Census Bureau. 2018-2022 American Community Survey 5-year estimates."),
                            br(),
                            h5("Here Inc. https://developer.here.com/develop/rest-apis"),
                            br(),
                            h5("CONTACT US: lmicustomerservice@dew.sc.gov"),
                            br(),
                            h5(""),
                            placement = "left",
                            arrow = TRUE)
                          
                  )),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Select Location", tabName = "set_coords", icon = icon("th")),
      menuItem("Drive Time Areas", tabName = "raw_radius_map"),
      menuItem("Overlapping Census Tracts", tabName = "radius_data_map"),
      menuItem("Census Tract Data Table", tabName = "radius_data_table"))),
  dashboardBody(
    tabItems(
      tabItem(tabName = "set_coords", h2("Select Location and Drive Time"),
              h5("This dashboard allows you to select any location in the United States and a drive time ranging from 20 to 120 minutes, view interactive maps representing the drive time area, and return relevant workforce census data within the drive time areas. Click on the 'i' for additional information."),
              fluidRow(
                tabBox(title = "Enter Location", id = "location_tabs",
                       side = "right", width=12,
                       tabPanel(
                         "Find Location by Address",
                         textInput("address", "Enter Address", value = "",
                                   width = NULL, placeholder = NULL),
                         textInput("city", "Enter City Name", value = "",
                                   width = NULL, placeholder = NULL),
                         selectInput("state", "State:",
                                     choices = state.abb, selected = "SC"),
                         actionButton("locate", "Get Drive Radius"),
                         textOutput("address_out"),
                         textOutput("coordinates_long"),
                         textOutput("coordinates_lat"),
                         htmlOutput("retrieved_drive_radius_address")),
                       tabPanel(
                         title = "Directly Enter Coordinates",
                         numericInput("longitude", "Longitude", value = "",
                                      width = NULL),
                         numericInput("latitude", "Latitude", value = "",
                                      width = NULL),
                         actionButton("submit_coords", "Get Drive Radius"),
                         htmlOutput("retrieved_drive_radius_coords"))
                ),
                box("Search Options", sliderInput("distance_range", "Maximum Drive Time",
                                                  min = 20, max = 120, value = 60, step = 20,
                                                  width = "100%"))
              )
      ),
      tabItem(tabName = "raw_radius_map", h2("Drive Time Areas"),
              h3("By 20 Minute Increments"),
              fluidRow(downloadButton("user_view_download", "Download")),
              fluidRow(
                box(width=11,
                    shinycssloaders::withSpinner(
                      leafletOutput("drive_radius_map", height = 1000))
                ))),
      tabItem(tabName = "radius_data_map", h2("Overlapping Census Tracts"),
              h5("This map displays the census tracts that overlap with your selected maximum drive time radius"),
              fluidRow(downloadButton("tract_radius_download", "Download")),
              fluidRow(
                box(width=11, shinycssloaders::withSpinner(
                  leafletOutput("acs_map", height = 1000))
                ))),
      tabItem(tabName = "radius_data_table", h2("Census Tract Data Table"),
              h5("This table provides census tract level workforce information, covering all tracts that overlap with your maximum drive time radius."),
              fluidRow(column(width = 10),
                       downloadButton("download_dt", "Download", side = "right")),
              fluidRow(
                box(width=11, shinycssloaders::withSpinner(
                  DTOutput("acs_data_dt", height = 1000)
                )
                )
              )
      )
    )
  )
)