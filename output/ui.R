#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(leaflet)



# Define UI for application that draws a histogram
ui <- dashboardPage(
    skin = 'black',
    dashboardHeader(title = 'Covid-19 Getaway Advisor'),
    dashboardSidebar(
        sidebarMenu(
            menuItem(tags$embed('Overview'),icon = icon('virus'),tabName = 'covidmap'),
            menuItem(tags$em('Choropleths'),icon = icon('head-side-mask'),tabName = 'choro'),
            #menuItem(tags$em('clustermap'),icon = icon('lungs'),tabName = 'cluster'),
            menuItem(tags$em('Popular destinations'),icon = icon('lightbulb'),tabName = 'destination')
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = 'covidmap',
                tags$h3('This would be a nationwide overview on how the epidemic spreading. With a side bar providing options of indice for users to choose. And perhaps a time slider to show time series data.'),
                tags$h4('Interactive cluster map with popups show detailed numbers. Zoom in to check on details'),
                sidebarPanel( width = "100%",
                    selectInput('data',
                                label = 'Please select an item',
                                choices = c('Confirmed','Deaths','Recovered','Active','Incidence Rate','Case Fatality Ratio')
                    ),
                fluidRow(box(width = "100%", leafletOutput(outputId = 'm0')))
            )),
            tabItem(
                tabName = 'choro', 
                tags$h3('This will be a choropleth map with different colors representing the number of cases within that area. With a side bar for users to choose a specific state to show.'),
                tags$h4('Interactive choropleth map with popups showing nearest tourist attractions.'),
                sidebarPanel(
                    selectInput('pro',
                                label = 'Please select a state',
                                choices = c('Connecticut','Delaware','Maryland','Massachusetts','New Jersey','New York','Pennsylvania','Rhode Island','Virginia','Washington DC','West Virginia'))
                )
            ),
            tabItem(
                tabName = 'desitination',
                tags$h3('Destinations travelers love'),
                tags$h4('Tables / Maps filtering by entered distance or category'),
                sidebarPanel(
                    selectInput('distance',
                                label = 'Please select a distance',
                                choices = c('<10 miles','10-20 miles','etc'))
                )
            )
            
        ),
        #fluidRow(box(width = 12, leafletOutput(outputId = 'mymap'))),
        #fluidRow(box(width = 12, dataTableOutput(outputId = 'summary_table')))
    )
    )


