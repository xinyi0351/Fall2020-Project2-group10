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
            menuItem("Home", tabName = "Home", icon = icon("home")),
            menuItem(tags$embed('Overview'),icon = icon('virus'),tabName = 'covidmap'),
            menuItem(tags$em('Choropleths'),icon = icon('head-side-mask'),tabName = 'choro'),
            #menuItem(tags$em('clustermap'),icon = icon('lungs'),tabName = 'cluster'),
            menuItem(tags$em('Find your destinations'),icon = icon('lightbulb'),tabName = 'destination')
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "Home",
                    fluidPage(
                        fluidRow(
                            box(width = 15, title = "Introduction", status = "primary",
                                solidHeader = TRUE, h1("Nearby Travel Destinations for New Yorkers during COVID-19"),
                                h3("By Charles Shin, Xinyi Zhang, Wei Yin, Mubai Liu, Lingjia Zhang"),
                                h3(''),
                                h4("The ability to travel in the United States during the year 2020 has been impacted significantly due to the COVID-19 outbreak. As the country recently experienced grim milestones of almost 8 million confirmed cases and over 200,000 deaths, 
                                   many people share the conflict in interest between the urge to travel after months of social isolation and the desire to prevent exposure to the coronavirus. This sentiment is perhaps most sharply felt in New York City, a place that initially was one of the global hotspots of the pandemic before cases started to recede due to effective social distancing policies. Such policies were instrumental in curbing the climbing rates of COVID-19, but at the same time ensured that millions of residents work from home and restrict time outdoors to the bare minumum. As schools and universities gradually reopen, and with the holiday seasons right around the corner, this conflict of interest threatens to only exacerbate."),
                                h4("This application is specifically designed for New Yorkers who are eager to experience a short break from confinement while at the same time keep safety from COVID-19 as their highest priority. Users will be able use an interactive map to explore 11 nearby states all within driving distance, thereby eliminating the need to fly and potentially risk more COVID-19 exposure. Each state has a breakdown of up-to-date COVID-19 statistics that can better inform whether a user would want to travel there, coupled with the ability to view the safest cities within that state. The application also provides information on the top 5, currently open attractions for each state, ranked and reviewed by Tripadvisor. 
                                   Users will be able to filter searches based on their preference of which type of state attractions they wish to visit, and NYC quarantine policies for the given states will be provided."), 
                                img(src=b64,style = "display: block; margin-left: auto; margin-right: auto;" ),
                                )),
                    )),
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
                                max = as.Date("2020-10-11", "%Y-%m-%d"),
                                value = as.Date("2020-10-11"), timeFormat="%Y-%m-%d"),
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


