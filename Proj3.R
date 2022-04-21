# Reads in csv
# taxi <- read.csv("Taxi_Trips_-_2019.csv")
# head(taxi)

#Gets rid of unneeded columns
# reduce_col <- taxi[c(3,5,6,9,10,17)]
# head(reduce_col)

#Get rid of very short trips and very long trips
# reduce_miles <- reduce_col[which(reduce_col$Trip.Miles >= 0.5 & reduce_col$Trip.Miles <= 100),]
# head(reduce_miles)

#Get rid of very short time and long time trips
# reduce_time <- reduce_miles[which(reduce_miles$Trip.Seconds >= 60 & reduce_miles$Trip.Seconds <= 18000),]
# head(reduce_time)

#Get rid of any with NA community area
# reduce_area <- na.omit(reduce_time)
# 
# summary(reduce_area)
# tail(reduce_area)
# 
# remove(reduce_col)
# remove(reduce_miles)
# remove(reduce_time)

#Added New Date, Date, and Hour column
library(lubridate)
# n_date <- parse_date_time(reduce_area$Trip.Start.Timestamp,
#                 orders = 'mdY IMS %p', truncated = 3) #PARSE DATE FROM TIME STAMP
# reduce_area$new_date <- n_date


#ADD COLUMNS FOR DATE AND HOUR
# reduce_area$Date <- date(reduce_area$new_date)
# reduce_area$Hour <- hour(reduce_area$new_date)
# 
# data <- reduce_area[1:6]
# head(data)

# write.csv(data, "modified_taxi.csv")

#Split file into 7 files?
library(NCmisc)
# setwd("/Users/waynekao/CS424/Project 3/Data/")
# file.split("modified_taxi.csv", size = 700000, verbose = TRUE,suf = "part", win = TRUE)


#READ CSV FILE AND CONVERT TO ONE DATA FRAME
col_name <- c("","Trip.Start.Timestamp","Trip.Seconds","Trip.Miles","Pickup.Community.Area","Dropoff.Community.Area","Company")
# setwd("bigYellowTaxi/Data/")
myfiles <- list.files(pattern="*.csv", full.names=TRUE)
myfiles
data <- do.call(rbind, lapply(myfiles, read.csv, header = FALSE))
colnames(data) <- col_name

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

communityName <- c("Palatine", "Arlington Heights")
companyName <- c("OddTaxi", "OddTaxi part2")
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
                                              selectInput("select_community", "Select Community", communityName),
                                              
                                              selectInput("select_company", "Select Company", companyName),
                                              
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
                                column(8,
                                       br(), 
                                       fluidRow( id="scopes_graph",
                                         box(
                                           title = "Bar Chart (for different scopes of the day) ", width = "100%", height = 750
                                         ),
                                       ),
                                       br(),
                                       fluidRow( id="communities_graph",
                                         box(
                                           title = "Bar Chart (for all communities) ", width = "100%", height = 700
                                         )
                                       )
                                       
                                ),
                                column(3,
                                       br(),
                                       fluidRow( id="table_box",
                                           box(
                                             title = "Table ", width = "25%", height = 750
                                           )
                                       ),
                                       fluidRow( id="leaflet_box",
                                                 box(
                                                   title = "Leaflet", width = "25%", height = 730
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
  
}

shinyApp(ui = ui, server = server)