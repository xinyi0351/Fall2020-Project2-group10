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
            menuItem(tags$em('Find your destinations'),icon = icon('lightbulb'),tabName = 'destination')
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = 'covidmap',
                tags$h1('COVID trend'),
                tags$h4('A nationwide overview on how the epidemic spreading. Discover how will the different indices change over the latest month. 
                        Slide the time bar to discover the trends.'),
                sidebarPanel( width = "100%",
                    selectInput('data',
                                label = 'Please select an item',
                                choices = c('Confirmed','Deaths','Recovered','Active','Incidence Rate','Case Fatality Ratio')
                    ),
                    sliderInput("DatesMerge",
                                "Dates:",
                                min = as.Date("2020-09-01", "%Y-%m-%d"),
                                max = as.Date("2020-10-04", "%Y-%m-%d"),
                                value = as.Date("2020-10-04"), timeFormat="%Y-%m-%d"),
                    fluidRow(box(width = "100%", leafletOutput(outputId = 'm0')))
            )),
            tabItem(
                tabName = 'choro', 
                tags$h1('Where do you want to visit'),
                tags$h3('How does indecident rates in each county changes? Select on time scale and distance scale'),
                tags$h3('Check out the trend of covid-cases and make your decision!'),
                #sidebarPanel(id = "control", class = "panel panel-default", fixed = TRUE, draggable = TRUE,
                # top = 300, left = 20, right = "auto", bottom = "auto", width = 100, height = "auto",
                # selectInput('choices','Which data to visualize:',
                #             choices = c('New York','New Jersey','Massachusetts','Virginia',
                #                         'Maryland','Pennsylvania','Connecticut','Delaware',
                #                         'Rhode Island','West Virginia'),
                #             selected = c('New York')),
                sidebarPanel(id = "control",
                              sliderInput('date_map','Input Date:',
                                          #first day of data recording
                                          min = as.Date(date_choices[1]),
                                          #present day of data recording
                                          max = as.Date(tail(date_choices,1)),
                                          value = as.Date('2020-09-01','%Y-%m-%d'),
                                          timeFormat = "%Y-%m-%d",
                                          animate = TRUE, step = 1),
                              sliderInput('distance_map', 'Distance From New York: (Miles)',
                                          min = 0,
                                          max = round(max(att$distance), -2),
                                          value = 500,
                                          animate = FALSE,
                                          step = 10),
                              style = "opacity: 0.80"),
                mainPanel(
                    leafletOutput("map", width = "80%", height = "800"),
                )
            ),
            
            # function here: Explore by states
            tabItem(
                tabName = 'destination',
                tags$h1('Destinations travelers love'),
                tags$h3('Choose the state you want to explore! Check the detail in the table.'),
                sidebarPanel(
                    selectInput('States',
                                label = 'Please select a states',
                                choices = c('New York','New Jersey','Massachusetts','Virginia',
                                            'Maryland','Pennsylvania','Connecticut','Delaware',
                                            'Rhode Island','West Virginia'))
                ),
                mainPanel(
                    leafletOutput(outputId = 'desty'),
                    DT::dataTableOutput(outputId = 'destable')
                )
            )
            
        ),
    )
    )


