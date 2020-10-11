#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(readr)
library(leaflet)
library(dplyr)
library(geojsonio)
library(sp)
library(maps)
library(raster)
library(htmltools)
library(maptools)
rgdal_show_exportToProj4_warnings = "none"
library(rgdal)
options(tigris_use_cache = TRUE)
library(tigris)
library(RCurl)


source("global.R") 
rawfile <- "https://raw.githubusercontent.com/zlj-0131/Data/main/allstate.csv"
allstate <- read_csv(rawfile)

states <- 
    geojson_read( 
        x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
        , what = "sp"
    )


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$m0 <- renderLeaflet({
        input1 <- input$data
        input2 <- input$DatesMerge
        
        if (input1 == "Incidence Rate"){
            
            us <- allstate %>%
                filter(is.na(Incidence_Rate) == FALSE) %>%
                filter(Last_Update <= input2) %>%
                group_by(Province_State) %>%
                mutate(sum_incidence_rate = sum(Incidence_Rate)) 
            
            states_us <- geo_join(states, us, "name", "Province_State")
            
            
            bins <- c(0, 100000, 200000, 500000, 1000000, 2000000, 5000000, 10000000, Inf)
            pal <- colorBin("YlOrRd", domain = states_us$sum_incidence_rate, bins = bins)
            
            
            labels <- sprintf(
                "<strong>%s</strong><br/>%g",
                states_us$name, states_us$sum_incidence_rate
            ) %>% lapply(htmltools::HTML)
            
            m <- leaflet(states_us) %>%
                setView(-96, 37.8, 4) %>%
                addProviderTiles("MapBox", options = providerTileOptions(
                    id = "mapbox.light",
                    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% 
                addPolygons(
                    fillColor = ~pal(sum_incidence_rate),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = labels,
                    labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"))
            m
        } else if (input1 == "Confirmed") {
            
            us1 <- allstate %>%
                filter(is.na(Incidence_Rate) == FALSE) %>%
                filter(Last_Update <= input2) %>%
                group_by(Province_State) %>%
                mutate(sum_confirmed = sum(Confirmed))
            
            states_us1 <- geo_join(states, us1, "name", "Province_State")
            
            
            bins <- c(0, 100000, 200000, 500000, 1000000, 2000000, 5000000, 10000000, Inf)
            pal <- colorBin("YlOrRd", domain = states_us1$sum_confirmed, bins = bins)
            
            labels <- sprintf(
                "<strong>%s</strong><br/>%g",
                states_us1$name, states_us1$sum_confirmed
            ) %>% lapply(htmltools::HTML)
            
            m2 <- leaflet(states_us1) %>%
                setView(-96, 37.8, 4) %>%
                addProviderTiles("MapBox", options = providerTileOptions(
                    id = "mapbox.light",
                    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% 
                addPolygons(
                    fillColor = ~pal(sum_confirmed),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = labels,
                    labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"))
            m2
            
        } else if (input1 == "Deaths") {
            
            us2 <- allstate %>%
                filter(is.na(Incidence_Rate) == FALSE) %>%
                filter(Last_Update <= input2) %>%
                group_by(Province_State) %>%
                mutate(sum_deaths = sum(Deaths))
            
            states_us2 <- geo_join(states, us2, "name", "Province_State")
            
            
            bins <- c(0, 10000, 20000, 50000, 100000, 200000, 500000, 1000000, Inf)
            pal <- colorBin("YlOrRd", domain = states_us2$sum_deaths, bins = bins)
            
            labels <- sprintf(
                "<strong>%s</strong><br/>%g",
                states_us2$name, states_us2$sum_deaths
            ) %>% lapply(htmltools::HTML)
            
            m3 <- leaflet(states_us2) %>%
                setView(-96, 37.8, 4) %>%
                addProviderTiles("MapBox", options = providerTileOptions(
                    id = "mapbox.light",
                    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% 
                addPolygons(
                    fillColor = ~pal(sum_deaths),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = labels,
                    labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"))
            m3
            
        } else if (input1 == "Active") {
            
            us3 <- allstate %>%
                filter(is.na(Incidence_Rate) == FALSE) %>%
                filter(Last_Update <= input2) %>%
                group_by(Province_State) %>%
                mutate(sum_active = sum(Active))
            
            states_us3 <- geo_join(states, us3, "name", "Province_State")
            
            
            bins <- c(0, 10000, 20000, 50000, 100000, 200000, 500000, 1000000, Inf)
            pal <- colorBin("YlOrRd", domain = states_us3$sum_active, bins = bins)
            
            labels <- sprintf(
                "<strong>%s</strong><br/>%g",
                states_us3$name, states_us3$sum_active
            ) %>% lapply(htmltools::HTML)
            
            m4 <- leaflet(states_us3) %>%
                setView(-96, 37.8, 4) %>%
                addProviderTiles("MapBox", options = providerTileOptions(
                    id = "mapbox.light",
                    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% 
                addPolygons(
                    fillColor = ~pal(sum_active),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = labels,
                    labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"))
            m4
            
            
        } else if (input1 == "Case Fatality Ratio") {
            
            us4 <- allstate %>%
                filter(is.na(Incidence_Rate) == FALSE) %>%
                filter(Last_Update <= input2) %>%
                group_by(Province_State) %>%
                mutate(fatality_ratio = sum(Deaths)/sum(Confirmed))
            
            states_us4 <- geo_join(states, us4, "name", "Province_State")
            
            
            bins <- c(0, 0.05, 0.01, 0.015, 0.035, 0.075, 0.1)
            pal <- colorBin("YlOrRd", domain = states_us4$fatality_ratio, bins = bins)
            
            labels <- sprintf(
                "<strong>%s</strong><br/>%g",
                states_us4$name, states_us4$fatality_ratio
            ) %>% lapply(htmltools::HTML)
            
            m5 <- leaflet(states_us4) %>%
                setView(-96, 37.8, 4) %>%
                addProviderTiles("MapBox", options = providerTileOptions(
                    id = "mapbox.light",
                    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% 
                addPolygons(
                    fillColor = ~pal(fatality_ratio),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = labels,
                    labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"))
            m5
            
        } else if (input1 == "Recovered") {
            
            us5 <- allstate %>%
                filter(is.na(Incidence_Rate) == FALSE) %>%
                filter(Last_Update <= input2) %>%
                group_by(Province_State) %>%
                mutate(sum_recoverd = sum(Recovered))
            
            states_us5 <- geo_join(states, us5, "name", "Province_State")
            
            
            bins <- c(0, 0.05, 0.01, 0.015, 0.035, 0.075, 0.1)
            pal <- colorBin("YlOrRd", domain = states_us5$sum_recoverd, bins = bins)
            
            labels <- sprintf(
                "<strong>%s</strong><br/>%g",
                states_us5$name, states_us5$sum_recoverd
            ) %>% lapply(htmltools::HTML)
            
            m6 <- leaflet(states_us5) %>%
                setView(-96, 37.8, 4) %>%
                addProviderTiles("MapBox", options = providerTileOptions(
                    id = "mapbox.light",
                    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% 
                addPolygons(
                    fillColor = ~pal(sum_recoverd),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = labels,
                    labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"))
            m6
        }
    }) 
#----------------------------------------------------------------------------------# First Page end Here
    covid_by_date <- covid %>% 
        filter(Last_Update == format.Date("2020-09-01", '%Y-%m-%d'))
    pal <- colorBin("Greens", NULL, bins = 5)
    output$map <- renderLeaflet({
        country_popup <- paste0("<strong>Country: </strong>",
                                covid_by_date$NAME,
                                "<br><strong>",
                                "Total Cases: ",
                                covid_by_date$Incidence_Rate,
                                "<br><strong>")
        leaflet(covid_by_date) %>%
            addProviderTiles("CartoDB.Positron", options = providerTileOptions(minZoom = 6, maxZoom = 10)) %>%
            setView(lat = 40.75042, lng = -73.98928, 10) %>%
            addPolygons(
                fillColor = ~pal(covid_by_date$Incidence_Rate),
                fillOpacity = 0.6,
                weight = 2,
                color = "white",
                popup = country_popup
            ) %>%
            addMarkers(lat = long, lng = lati)
    })
    
    
    
    observe({
        if(!is.null(input$date_map)){
            select_date <- format.Date(input$date_map,'%Y-%m-%d')
        }
        covid_by_date <- covid %>% 
            filter(Last_Update == input$date_map) 
        print(covid_by_date$Last_Update)
        country_popup <- paste0("<strong>Country: </strong>",
                                covid_by_date$NAME,
                                "<br><strong>",
                                "Total Cases: ",
                                covid_by_date$Incidence_Rate,
                                "<br><strong>")
        leafletProxy("map", data = covid_by_date) %>%
            addPolygons(
                fillColor = ~pal(covid_by_date$Incidence_Rate),
                fillOpacity = 0.6,
                weight = 2,
                color ="white",
                popup = country_popup,
                layerId = ~NAME
            )
    })
#--------------------------------------------------------------------------------------# Second Page Ends Here
})

