#READ CSV FILE AND CONVERT TO ONE DATA FRAME
col_name <- c("","Trip.Start.Timestamp","Trip.Seconds","Trip.Miles","Pickup.Community.Area","Dropoff.Community.Area","Company")
# setwd("bigYellowTaxi/Data/")
myfiles <- list.files(pattern="*.csv", full.names=TRUE)
myfiles
data <- do.call(rbind, lapply(myfiles, read.csv, header = FALSE))
colnames(data) <- col_name

#ADDED
n_date <- parse_date_time(data$Trip.Start.Timestamp,
                          orders = 'mdY IMS %p', truncated = 3) #PARSE DATE FROM TIME STAMP
data$new_date <- n_date
data$Hour <- hour(data$new_date)

#libraries to include

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(gridExtra)
library(dplyr)
library(rlang)

community_list <- c("Rogers Park", "West Ridge", "Uptown",
                    "Lincoln Square", "North Center", "Lake View",
                    "Lincoln Park", "Near North Side", "Edison Park",
                    "Norwood Park", "Jefferson Park", "Forest Glen",
                    "North Park", "Albany Park", "Portage Park",
                    "Irving Park", "Dunning", "Montclare", "Belmont Cragin",
                    "Hermosa", "Avondale", "Logan Square", "Humboldt Park",
                    "West Town", "Austin", "West Garfield Park",
                    "East Garfield Park", "Near West Side", "North Lawndale",
                    "South Lawndale", "Lower West Side", "Loop", "Near South Side",
                    "Armour Square", "Douglas", "Oakland", "Fuller Park",
                    "Grand Boulevard", "Kenwood", "Washington Park", "Hyde Park",
                    "Woodlawn", "South Shore", "Chatham", "Avalon Park", "South Chicago",
                    "Burnside", "Calumet Heights", "Roseland", "Pullman", "South Deering",
                    "East Side", "West Pullman", "Riverdale", "Hegewisch", "Garfield Ridge",
                    "Archer Heights", "Brighton Park", "Mckinley Park", "Bridgeport",
                    "New City", "West Elsdon", "Gage Park", "Clearing", "West Lawn",
                    "Chicago Lawn", "West Englewood", "Englewood", "Greater Grand Crossing",
                    "Ashburn", "Auburn Gresham", "Beverly", "Washington Heights",
                    "Mount Greenwood", "Morgan Park", "Ohare", "Edgewater", "City of Chicago") # R IS INDEX STARTING 1 NOT 0
sorted_community_list <- sort(community_list)
new_sorted_community_list <- append(sorted_community_list, "All (City of Chicago)", 0)

company_list <- c("1085 - 72312 N and W Cab Co", "1469 - 64126 Omar Jada","2092 - 61288 Sbeih company", "24 Seven Taxi",
                  "2733 - 74600 Benny Jona", "3011 - 66308 JBL Cab Inc.", "3094 - 24059 G.L.B. Cab Co",
                  "312 Medallion Management Corp", "3556 - 36214 RC Andrews Cab", "3591 - 63480 Chuks Cab",
                  "3620 - 52292 David K. Cab Corp.", "3623 - 72222 Arrington Enterprises",
                  "3721 - Santamaria Express, Alvaro Santamaria", "4053 - 40193 Adwar H. Nikola",
                  "4623 - 27290 Jay Kim", "5 Star Taxi", "5006 - 39261 Salifu Bawa", "5062 - 34841 Sam Mestas",
                  "5074 - 54002 Ahzmi Inc", "5874 - 73628 Sergey Cab Corp.", "6574 - Babylon Express Inc.",
                  "6742 - 83735 Tasha ride inc", "6743 - 78771 Luhak Corp", "American United",
                  "American United Taxi Affiliation", "Blue Diamond", "Blue Ribbon Taxi Association Inc.",
                  "Checker Taxi", "Checker Taxi Affiliation", "Chicago Carriage Cab Corp",
                  "Chicago Independents", "Chicago Medallion Management", "Chicago Star Taxicab",
                  "Chicago Taxicab", "Choice Taxi Association", "City Service", "Flash Cab", "Globe Taxi",
                  "Gold Coast Taxi", "KOAM Taxi Association", "Leonard Cab Co", "Medallion Leasin",
                  "Metro Jet Taxi A", "Nova Taxi Affiliation Llc", "Patriot Taxi Dba Peace Taxi Associat",
                  "Setare Inc", "Star North Management LLC", "Sun Taxi", "Taxi Affiliation Service Yellow",
                  "Taxi Affiliation Services", "Taxicab Insurance Agency, LLC", "Top Cab Affiliation", "Yellow Cab",
                  "Petani Cab Corp", "U Taxicab")

new_company_list <- append(company_list, "All Taxis", 0)

distributionType <- c("By Day", "By Hour of Day", "By Day of Week", "By Month", "By Binned Mileage", "By Binned Trip Time")


ui <- shinyUI(
  navbarPage("OddTaxi", position = "fixed-bottom",
             tabPanel("Graphs",
                      fluidPage(tags$style(HTML("
                                  #scopes_graph {
                                      border: 4px double red;
                                  }
                                  #communities_graph {
                                      border: 2px dashed blue;
                                  }
                                  #table_box {
                                      border: 2px dashed green;
                                  }
                                  #leaflet_box {
                                      border: 2px dashed orange;
                                  }
                                ")),
                                column(1, style = "height:1620px;background-color: orange",
                                       column(12,
                                              br(),br(),br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
                                              br(),br(),br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
                                              selectInput("select_community", "Select Community", new_sorted_community_list, selected="All (City of Chicago)"),

                                              selectInput("select_company", "Select Company", new_company_list, selected = "All Taxis"),

                                              selectInput("select_distribution", "Select Distribution Type", distributionType, selected = "By Day"),

                                              radioButtons("measurement", "Measurement:",
                                                           c("Kilometer (km)" = "km",
                                                             "Miles (mi)" = "miles")
                                              ),
                                              radioButtons("time", "Change Time Format:",
                                                           c("12 Hr" = "twelveHr",
                                                             "24 Hr" = "twentyFourHr")
                                              ),
                                              radioButtons("destination", "Destination:",
                                                           c("To" = "goingTo",
                                                             "From" = "comingFrom")
                                              )
                                       )
                                ),
                                column(11, br(), br(),
                                    fluidRow(id="top-row",
                                        column(10,id="scopes_graph",
                                               box(
                                                   title = "Bar Chart (for different scopes of the day)",solidHeader = TRUE, status = "primary", width = "100%", height = 750,
                                                   plotOutput("scopechart", width = "100%", height = 700)
                                               )
                                        ),
                                        column(2,id="table_box",
                                               box(
                                                  title = "Table ", width = "20%", height = 700
                                                )
                                        )
                                    ), br(),
                                    fluidRow(id="top-row",
                                             column(9, id="communities_graph",
                                                    box(
                                                      title = "Bar Chart (for all communities)", width = "100%", height = 750
                                                    )
                                             ),
                                             column(3, id="leaflet_box",
                                                    box(
                                                      title = "Leaflet", width = "20%", height = 700
                                                    )
                                             )
                                    )
                                )

                      )
             ),
             tabPanel("About",
                      fluidPage(
                        fluidRow(style="font-size: 40px; padding-bottom: 15%",
                                 h1("Chicago Taxi Trip Data"),
                                 h4("Author: Wayne Kao and Raphael Genova"),
                                 h4("Dataset: https://data.cityofchicago.org/Transportation/Taxi-Trips-2019/h4cq-z3dy (Warning: This dataset is about 7 GB"),
                                 div("The data was taken from the city of chicago page. This app was written to compare the amount of riders from Jan 1, 2001- November 21, 2021 along with the locations of all CTA stops by longitude and latitude.
          from all CTA stations. Each page has its own unique functionality to fit the proper visualizations and format. By default, the site goes to the 'By Date' tab. The 'By Date' tab contains a bar graph that has all of the ridership
          of each stop at a particular date and there is also a table that gives more detail about what is displayed on the bar graph. This tab also contains a leaflet which shows geographically where each CTA stop is (which is indicated
          by a blue point). The 'By Station' tab allows the use to compare between two stations at specific years. The bar graphs give ridership numbers per Day, Month, and Weekday. The 'Compare' tab contains two bar graphs and tables each
          graph/table set represents a particular date the user would like to explore. The graphs and tables display the total ridership of each stop at that particular day.")
                        )
                      )
             ),
             tags$style(type="text/css",
                        '.navbar{cel
    font-size: 20px;
    }')
  )
)

server <- function(input, output, session) {
  community <- "All" # input$select_community
  trip <- "To"
  company <- "All Taxi"
  units <- "MI"
  distribution <- "All" # intput$select_distribution

  graph_scope_chart <- function(){

    # d_graph <- allStops1()
    g <- NULL
    # i_order = orderOpt2()



    return(g)
  }

  output$scopechart <- renderPlot({
    noFilter_byDay <- aggregate(data[,1], by=list(date(data$new_date)), FUN=length)
    colnames(noFilter_byDay) <- c("Date", "Rides")

    ggplot(data=noFilter_byDay, aes(x=noFilter_byDay$Date, y=noFilter_byDay$Rides)) + geom_bar(stat="identity")

    })

}

shinyApp(ui = ui, server = server)
