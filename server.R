#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shinydashboard)
library(DT)
library(webshot)
library(tigris)

function(input, output, session) {
  coordinates <- reactive({
    census_geocode_address(input$address, input$city, input$state)
  })
  drive_radius_shape <- eventReactive(submit_button(), ignoreInit = TRUE,{
    get_drive_radius_hereR(input$longitude, input$latitude) %>%
      return()
  })
  slid_radius_shape <- reactive({
    drive_radius_shape() %>%
      filter(radius_range <= input$distance_range * 60) %>%
      return()
  })
  submit_button <- reactive({
    paste(input$locate, input$submit_coords)
  })
  table_data <- reactive({
    tracts_data_sf() %>% create_table_data() %>% return()
  })
  output$coordinates_long <- renderText(
    ifelse("character" %in% class(coordinates()),
           paste("Please Enter a Valid Address or Coordinates."),
           paste0("Longitude: ",coordinates()$long)))
  output$coordinates_lat <- renderText(
    ifelse("character" %in% class(coordinates()),
           paste(""),
           paste0("Latitude: ", 
                  coordinates()$lat)))
  output$address_out <- renderText(
    ifelse("character" %in% class(coordinates()),
           "Unable to Identify This Address.",
           paste0("Address: ", coordinates()$matched_address)))
  output$retrieved_drive_radius_address <- renderText({
    if(!is.null(drive_radius_shape()) & "sf" %in% class(drive_radius_shape())){
      paste0("<b>Successfully Retrieved Drive Radius! Select one of the tabs in the left panel to view your maps and data.</b>")
    }
  })
  output$retrieved_drive_radius_coords <- renderText({
    if(!is.null(drive_radius_shape()) & "sf" %in% class(drive_radius_shape())){
      paste0("<b>Successfully Retrieved Drive Radius! Select one of the tabs in the left panel to view your maps and data.</b>")
    }
  })
  observe({
    if(!"character" %in% class(coordinates()) & !is.null(coordinates())){
      updateTextInput(session, "longitude", value = as.numeric(coordinates()$long))
      updateTextInput(session, "latitude", value = as.numeric(coordinates()$lat))}
  })
  drive_radius_object <- reactive({
    if(is.null(drive_radius_shape()) | !"sf" %in% class(drive_radius_shape())){
      leaf <- leaflet() %>% setView(-81.04689881123234, 
                                    34.03247918860177, zoom = 8) %>%
        addProviderTiles("CartoDB.Positron", group="Greyscale")
    }
    else{
      leaf <- create_drive_map(slid_radius_shape(), input$longitude, 
                               input$latitude,
                               view = ifelse(input$distance_range > 60, 8, 
                                             ifelse(input$distance_range < 40, 
                                                    10, 9)))
    }
    return(leaf)
  })
  output$drive_radius_map <- renderLeaflet({
    drive_radius_object()
  })
  output$user_view_download <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_drive_radius_map.png")
    },
    content = function(file) {
      mapshot(drive_radius_object() %>% 
                addProviderTiles("OpenStreetMap.Mapnik", group="Greyscale"),
              file = file,
              cliprect = "viewport"
      ) 
    }
  )
  states_list <- reactive(get_states_vector(slid_radius_shape())
  )
  tracts_data_sf <- reactive({get_acs_data(states_list(), 
                                           slid_radius_shape()) %>%
      get_output_sf(slid_radius_shape())})
  acs_map_react <- reactive({
    if(is.null(tracts_data_sf())){
      return(leaflet() %>% setView(-81.04689881123234, 
                                   34.03247918860177, zoom = 8) %>%
               addProviderTiles("CartoDB.Positron", group="Greyscale"))
    }
    else{
      return(create_tract_map(table_data(), input$longitude, input$latitude))
    }
  })
  output$acs_map <- renderLeaflet({
    acs_map_react()
  })
  output$acs_data_dt <- renderDT({
    table_data() %>% create_dt_table()
  })
  output$download_dt <- downloadHandler(
    filename = function() {
      "drive_time_acs_tracts.csv"
    },
    content = function(file) {
      write.csv(table_data() %>% st_drop_geometry(), file)
    }
  )
  output$tract_radius_download <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_tract_radius_map.png")
    },
    content = function(file) {
      mapshot(acs_map_react() %>% 
                addProviderTiles("OpenStreetMap.Mapnik"),
              file = file,
              cliprect = "viewport"
      ) 
    }
  )
}