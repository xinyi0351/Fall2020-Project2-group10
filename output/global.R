if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}
if (!require("shinythemes")) {
  install.packages("shinythemes")
  library(shinythemes)
}

if (!require("raster")) {
  install.packages("raster")
  library(raster)
}
if (!require("RCurl")) {
  install.packages("RCurl")
  library(RCurl)
}
if (!require("maps")) {
  install.packages("maps")
  library(map)
}
if (!require("maptools")) {
  install.packages("maptools")
  library(maptools)
}
if (!require("rgdal")) {
  install.packages("rgdal")
  library(rgdal)
}
if (!require("leaflet")) {
  install.packages("leaflet")
  library(leaflet)
}
if (!require("shiny")) {
  install.packages("shiny")
  library(shiny)
}
if (!require("shinythemes")) {
  install.packages("shinythemes")
  library(shinythemes)
}
if (!require("plotly")) {
  install.packages("plotly")
  library(plotly)
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require("tigris")) {
  install.packages("tigris")
  library(tigris)
}
if (!require("geojsonio")) {
  install.packages("geojsonio")
  library(geojsonio)
}
if (!require("rgdal")) {
  install.packages("rgdal")
  library(rgdal)
}
if (!require("htmltools")) {
  install.packages("htmltools")
  library(htmltools)
}
if (!require("sp")) {
  install.packages("sp")
  library(sp)
}


covid <- read_csv('covid_cleaned.csv')
att <- read_csv('Project 2 State Attractions.csv')
covid <-  covid %>%
  filter(!is.na(Long_)) %>%
  filter(!is.na(Lat)) %>%
  filter(!is.na(Incidence_Rate)) %>%
  filter(Admin2 != 'Unassigned')

date_choices <- as.Date(covid$Last_Update,format = 'X%m.%d.%y')

geo_try <- counties(c('New York','New Jersey','Massachusetts','Virginia',
                      'Maryland','Pennsylvania','Connecticut','Delaware',
                      'Rhode Island','West Virginia'), cb =TRUE)

#geo_try_2 <- merge(geo_try, covid, by.x = "NAME", by.y = "Province_State")

geo_try_2 <- geo_join(geo_try, covid, "NAME", "Admin2")


covid <- merge(geo_try,
               covid,
               by.x = 'NAME',
               by.y = 'Admin2',sort = FALSE)

state <- tigris::states() %>% filter(NAME %in% c('New York','New Jersey','Massachusetts','Virginia',
                                                 'Maryland','Pennsylvania','Connecticut','Delaware',
                                                 'Rhode Island','West Virginia'))


randomData <- rnorm(n=nrow(geo_try_2), 150, 30)
long <- att$`Longitude (N/S)`
lati <- -att$`Latitude (E/W)`