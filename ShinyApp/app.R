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

#READ CSV FILE AND CONVERT TO ONE DATA FRAME
col_name <- c("Trip.Seconds","Trip.Miles","Pickup.Community.Area","Dropoff.Community.Area","Company","Date","Hour")
myfiles <- list.files(pattern="*.csv", full.names=TRUE)
data <- do.call(rbind, lapply(myfiles, read.csv, header = FALSE))
colnames(data) <- col_name


by_day_data <- read.csv("D/by_day.csv")
by_binned_data <- read.csv("D/by_binned.csv")
by_hour_data <- read.csv("D/by_hour.csv")
by_dayofweek_data <- read.csv("D/by_DayofWeek.csv")
by_month_data <- read.csv("D/by_month.csv")
by_mileage_data <- read.csv("D/by_Mileage.csv")
# data <- read.csv("modified_taxi.csv")
#


# #ADDED
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
new_sorted_community_list <- sort(community_list)
new_sorted_community_list <- append(new_sorted_community_list, "All (City of Chicago)", 0)

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

chi_sp <-  rgdal::readOGR("Boundaries - Community Areas (current).geojson")

distributionType <- c("By Day", "By Hour of Day", "By Day of Week", "By Month", "By Binned Mileage", "By Binned Trip Time")


ui <- shinyUI(
  navbarPage("OddTaxi", position = "fixed-bottom",
             tabPanel("Graphs",
                      fluidPage(tags$style(HTML("

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
                                                  title = "Table ", width = "20%", height = 700,
                                                  dataTableOutput("table_scopes")
                                                )
                                        )
                                    ), br(),
                                    fluidRow(id="top-row",
                                             column(9, id="communities_graph",
                                                    box(
                                                      title = "Bar Chart (for all communities)", width = "100%", height = 750,
                                                      plotOutput("communitychart", width = "100%", height = 700)
                                                    )
                                             ),
                                             column(3, id="leaflet_box",
                                                    leafletOutput("mymap3", height=700, width="100%")
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
                                 div("The data was taken from the city of chicago page. This app was written to visualize the data of taxi ridership in Chicago since the rise of Uber, Lyft and other ride sharing services. This data
                                     is from 2019 so that this can be more representative of a 'typical' year. Upon loading this page, the first tab you'll encounter is the Graphs tab. This tab has 2 graphs (one for showing the
                                     different scopes and the other for all the communities) and a table that gives more detail about the scopes and a leaflet to show a heat map of the community areas showing their ridership
                                     percentage. On the sidebar, you can change the graphs accordingly by changing the 'Select Community', 'Select Company', or 'Select Distribution Type' dropdowns. You can also change things by
                                     pressing different radio buttons like 'Measurement', 'Change Time Format', and 'Destination'.")
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

  # reactive_by_day <- reactive({
  #   community <- input$select_community
  #   trip <- input$destination
  #   units <- input$measurement
  #   company <- input$select_company
  #   if(community == "All (City of Chicago)" & company == "All Taxis"){
  #     return(by_day_data)
  #   }
  #
  #   updateData <- data
  #   if(community != "All (City of Chicago)"){
  #         if(trip == "goingTo"){
  #           updateData <- subset(updateData, updateData$Dropoff.Community.Area == match(community, community_list))
  #         }
  #         else{ #FROM
  #           updateData <- subset(updateData, updateData$Pickup.Community.Area == match(community, community_list))
  #         }
  #   }
  #   if(company != "All Taxis"){
  #       updateData <- subset(updateData, updateData$Company == company)
  #   }
  #
  #   noFilter_byDay <- aggregate(updateData[,1], by=list(date(updateData$Date)), FUN=length)
  #   colnames(noFilter_byDay) <- c("Date", "Rides")
  #   return(noFilter_byDay)
  # })
  # reactive_by_hour <- reactive({
  #   noFilter_byHour <- aggregate(data[,1], by=list(data$Hour), FUN=length)
  #   colnames(noFilter_byHour) <- c("Hour", "Rides")
  #   return(noFilter_byHour)
  # })

  # distribution <- "All" NOTE: I realize this isn't gonna work. Put this in the scope chart method instead

  filterData <- reactive({
    updateData <- data
    community <- input$select_community
    trip <- input$destination
    units <- input$measurement
    company <- input$select_company
    hourtime <- input$time

    if(community != "All (City of Chicago)"){
      if(trip == "goingTo"){
        updateData <- subset(updateData, updateData$Dropoff.Community.Area == match(community, community_list))
      }
      else{ #FROM
        updateData <- subset(updateData, updateData$Pickup.Community.Area == match(community, community_list))
      }
    }
    if(company != "All Taxis"){
        updateData <- subset(updateData, updateData$Company == company)
    }

    if(units == "km"){
      updateData$Trip.Miles <- updateData$Trip.Miles * 1.609344
    }

    return(updateData)
  })
#
# #-------- FOR DAY ----------------
  # byDayNoFilter <- reactive({
  #   noFilter_byDay <- aggregate(data[,1], by=list(date(data$Date)), FUN=length)
  #   colnames(noFilter_byDay) <- c("Date", "Rides")
  #   return(noFilter_byDay)
  # })

  byDay <- function(){
    #If you don't include this, going back to city of chicago is slow
    if(input$select_community == "All (City of Chicago)" && input$select_company == "All Taxis"){
      return(by_day_data[c(2,3)])
    }

    updateData <- filterData()
    # updateData <- data
    noFilter_byDay <- aggregate(updateData[,1], by=list(date(updateData$Date)), FUN=length)
    colnames(noFilter_byDay) <- c("Date", "Rides")
    return(noFilter_byDay)
  } #Aggregates all stations by specific date
#
#
# #-------- FOR HOUR ------------------
#   # byHourNoFilter <- reactive({
#   #   noFilter_byHour <- aggregate(data[,1], by=list(data$Hour), FUN=length)
#   #   colnames(noFilter_byHour) <- c("Hour", "Rides")
#   #   return(noFilter_byHour)
#   # })
#
  byHour <- reactive({
    #If you don't include this, going back to city of chicago is slow
    # if(input$select_community == "All (City of Chicago)" && input$select_company == "All Taxis"){
    #   return(by_hour_data[c(2,3)])
    # }
    #

    updateData <- filterData()
    noFilter_byHour <- aggregate(updateData[,1], by=list(updateData$Hour), FUN=length)
    colnames(noFilter_byHour) <- c("Hour", "Rides")
    if(input$time == "twelveHr"){
      twelve_hr_time <- c("12 AM", "1 AM" , "2 AM", "3 AM", "4 AM", "5 AM", "6 AM", "7 AM", "8 AM", "9 AM", "10 AM", "11 AM",
        "12 PM", "1 PM" , "2 PM", "3 PM", "4 PM", "5 PM", "6 PM", "7 PM", "8 PM", "9 PM", "10 PM", "11 PM")
      noFilter_byHour$Hour[noFilter_byHour["Hour"] == 0] <- "12 AM"
      noFilter_byHour$Hour[noFilter_byHour["Hour"] == 1] <- "1 AM"
      noFilter_byHour$Hour[noFilter_byHour["Hour"] == 2] <- "2 AM"
      noFilter_byHour$Hour[noFilter_byHour["Hour"] == 3] <- "3 AM"
      noFilter_byHour$Hour[noFilter_byHour["Hour"] == 4] <- "4 AM"
      noFilter_byHour$Hour[noFilter_byHour["Hour"] == 5] <- "5 AM"
      noFilter_byHour$Hour[noFilter_byHour["Hour"] == 6] <- "6 AM"
      noFilter_byHour$Hour[noFilter_byHour["Hour"] == 7] <- "7 AM"
      noFilter_byHour$Hour[noFilter_byHour["Hour"] == 8] <- "8 AM"
      noFilter_byHour$Hour[noFilter_byHour["Hour"] == 9] <- "9 AM"
      noFilter_byHour$Hour[noFilter_byHour["Hour"] == 10] <- "10 AM"
      noFilter_byHour$Hour[noFilter_byHour["Hour"] == 11] <- "11 AM"
      noFilter_byHour$Hour[noFilter_byHour["Hour"] == 12] <- "12 PM"
      noFilter_byHour$Hour[noFilter_byHour["Hour"] == 13] <- "1 PM"
      noFilter_byHour$Hour[noFilter_byHour["Hour"] == 14] <- "2 PM"
      noFilter_byHour$Hour[noFilter_byHour["Hour"] == 15] <- "3 PM"
      noFilter_byHour$Hour[noFilter_byHour["Hour"] == 16] <- "4 PM"
      noFilter_byHour$Hour[noFilter_byHour["Hour"] == 17] <- "5 PM"
      noFilter_byHour$Hour[noFilter_byHour["Hour"] == 18] <- "6 PM"
      noFilter_byHour$Hour[noFilter_byHour["Hour"] == 19] <- "7 PM"
      noFilter_byHour$Hour[noFilter_byHour["Hour"] == 20] <- "8 PM"
      noFilter_byHour$Hour[noFilter_byHour["Hour"] == 21] <- "9 PM"
      noFilter_byHour$Hour[noFilter_byHour["Hour"] == 22] <- "10 PM"
      noFilter_byHour$Hour[noFilter_byHour["Hour"] == 23] <- "11 PM"
      noFilter_byHour$Hour <- factor(noFilter_byHour$Hour, levels = twelve_hr_time, ordered = TRUE)
    }

    return(noFilter_byHour)
  }) #Aggregates all stations by specific hour
#
# #-------- FOR DAY OF WEEK -----------------
#   #* STARTS MONDAY TO SUNDAY WITH 1 BEING MONDAY
#
#   # byDayOfWeekNoFilter <- reactive({
#   #   noFilter_byDayOfWeek <- aggregate(data[,1], by=list(wday(data$Date, week_start=1)), FUN=length)
#   #   colnames(noFilter_byDayOfWeek) <- c("Day Of Week", "Rides")
#   #   return(noFilter_byDayOfWeek)
#   # })
#
  byDayOfWeek <- function(){
    #If you don't include this, going back to city of chicago is slow
    if(input$select_community == "All (City of Chicago)" && input$select_company == "All Taxis"){
      colnames(by_dayofweek_data) <- c("", "Day Of Week", "Rides")
      return(by_dayofweek_data[c(2,3)])
    }

    updateData <- filterData()
    #Aggregate by weekday
    noFilter_byDayOfWeek <- aggregate(updateData[,1], by=list(wday(updateData$Date, week_start=1)), FUN=length)
    colnames(noFilter_byDayOfWeek) <- c("Day Of Week", "Rides")

    return(noFilter_byDayOfWeek)
  } #Aggregates all stations by specific day of week
#
# #-------- FOR MONTH ----------------------
#
#   # byMonthNoFilter <- function(){
#   #   noFilter_byMonth <- aggregate(data[,1], by=list(month(data$Date)), FUN=length)
#   #   colnames(noFilter_byMonth) <- c("Month", "Rides")
#   #   return(noFilter_byMonth)
#   # }
#
  byMonth <- function(){
    #If you don't include this, going back to city of chicago is slow
    if(input$select_community == "All (City of Chicago)" && input$select_company == "All Taxis"){
      return(by_month_data[c(2,3)])
    }

    updateData <- filterData()
    #Aggregate by month
    noFilter_byMonth<- aggregate(updateData[,1], by=list(month(updateData$Date)), FUN=length)
    colnames(noFilter_byMonth) <- c("Month", "Rides")

    return(noFilter_byMonth)
  } #Aggregates all stations by specific month
#
#
# #-------- FOR MILEAGE ----------------------
#
#   # byMileagehNoFilter <- function(){
#   #   #Aggregate by binned mileage
#   #
#   #   #Add to global
#   #   mile_bin <- c("0.5 to < 10", "10 to < 20", "20 to < 30", "30 to < 40", "40 to < 50", "50 to < 60", "60 to < 70", "70 to < 80", "80 to < 90", "90 to 100")
#   #
#   #   #Actual code to get data frame
#   #   bin_data = data.frame(data)
#   #   bin_data$Mile_Bin <- NA
#   #   bin_data$Mile_Bin[bin_data$Trip.Miles >= 0.5 & bin_data$Trip.Miles < 10] <- mile_bin[1]
#   #   bin_data$Mile_Bin[bin_data$Trip.Miles >= 10 & bin_data$Trip.Miles < 20] <- mile_bin[2]
#   #   bin_data$Mile_Bin[bin_data$Trip.Miles >= 20 & bin_data$Trip.Miles < 30] <- mile_bin[3]
#   #   bin_data$Mile_Bin[bin_data$Trip.Miles >= 30 & bin_data$Trip.Miles < 40] <- mile_bin[4]
#   #   bin_data$Mile_Bin[bin_data$Trip.Miles >= 40 & bin_data$Trip.Miles < 50] <- mile_bin[5]
#   #   bin_data$Mile_Bin[bin_data$Trip.Miles >= 50 & bin_data$Trip.Miles < 60] <- mile_bin[6]
#   #   bin_data$Mile_Bin[bin_data$Trip.Miles >= 60 & bin_data$Trip.Miles < 70] <- mile_bin[7]
#   #   bin_data$Mile_Bin[bin_data$Trip.Miles >= 70 & bin_data$Trip.Miles < 80] <- mile_bin[8]
#   #   bin_data$Mile_Bin[bin_data$Trip.Miles >= 80 & bin_data$Trip.Miles < 90] <- mile_bin[9]
#   #   bin_data$Mile_Bin[bin_data$Trip.Miles >= 90 & bin_data$Trip.Miles <= 100] <- mile_bin[10]
#   #   bin_data$Mile_Bin <- as.factor(bin_data$Mile_Bin)
#   #
#   #   noFilter_byBinnedMileage <- aggregate(bin_data[,1], by=list(bin_data$Mile_Bin), FUN=length)
#   #   colnames(noFilter_byBinnedMileage) <- c("Miles", "Rides")
#   #   return(noFilter_byBinnedMileage)
#   # }
#
  byMileage <- reactive({
    # #If you don't include this, going back to city of chicago is slow
    # if(input$select_community == "All (City of Chicago)" && input$select_company == "All Taxis"){
    #   return(by_mileage_data)
    # }
    units <- input$measurement
    updateData <- filterData()
    #Aggregate by binned mileage

    #Add to global
    mile_bin <- c("0.5 to < 2", "2 to < 4", "4 to < 6", "6 to < 8", "8 to < 10", "10 to < 20", "20 to < 30", "30 to < 40", "40 to < 50", "50 to < 60", "60 to < 70", "70 to < 80", "80 to < 90", "90 to 100")
    km_bin <- c("0.5 to < 3", "3 to < 6", "6 to < 9", "9 to < 12", "12 to < 15", "15 to < 30", "30 to < 45", "45 to < 60", "60 to < 75", "75 to < 90", "90 to < 105", "105 to < 120", "120 to < 135", "135 to 161")

    bin_data = data.frame(updateData)
    bin_data$Mile_Bin <- NA
    noFilter_byBinnedMileage <- NULL
     #Actual code to get data frame
    if(units == "miles"){
      bin_data$Mile_Bin[bin_data$Trip.Miles >= 0.5 & bin_data$Trip.Miles < 2] <- mile_bin[1]
      bin_data$Mile_Bin[bin_data$Trip.Miles >= 2 & bin_data$Trip.Miles < 4] <- mile_bin[2]
      bin_data$Mile_Bin[bin_data$Trip.Miles >= 4 & bin_data$Trip.Miles < 6] <- mile_bin[3]
      bin_data$Mile_Bin[bin_data$Trip.Miles >= 6 & bin_data$Trip.Miles < 8] <- mile_bin[4]
      bin_data$Mile_Bin[bin_data$Trip.Miles >= 8 & bin_data$Trip.Miles < 10] <- mile_bin[5]
      bin_data$Mile_Bin[bin_data$Trip.Miles >= 10 & bin_data$Trip.Miles < 20] <- mile_bin[6]
      bin_data$Mile_Bin[bin_data$Trip.Miles >= 20 & bin_data$Trip.Miles < 30] <- mile_bin[7]
      bin_data$Mile_Bin[bin_data$Trip.Miles >= 30 & bin_data$Trip.Miles < 40] <- mile_bin[8]
      bin_data$Mile_Bin[bin_data$Trip.Miles >= 40 & bin_data$Trip.Miles < 50] <- mile_bin[9]
      bin_data$Mile_Bin[bin_data$Trip.Miles >= 50 & bin_data$Trip.Miles < 60] <- mile_bin[10]
      bin_data$Mile_Bin[bin_data$Trip.Miles >= 60 & bin_data$Trip.Miles < 70] <- mile_bin[11]
      bin_data$Mile_Bin[bin_data$Trip.Miles >= 70 & bin_data$Trip.Miles < 80] <- mile_bin[12]
      bin_data$Mile_Bin[bin_data$Trip.Miles >= 80 & bin_data$Trip.Miles < 90] <- mile_bin[13]
      bin_data$Mile_Bin[bin_data$Trip.Miles >= 90 & bin_data$Trip.Miles <= 100] <- mile_bin[14]
      bin_data$Mile_Bin <- factor(bin_data$Mile_Bin, levels = mile_bin, ordered = TRUE)
      noFilter_byBinnedMileage <- aggregate(bin_data[,1], by=list(bin_data$Mile_Bin), FUN=length)
      colnames(noFilter_byBinnedMileage) <- c("Miles", "Rides")
    }else{
      bin_data$Mile_Bin[bin_data$Trip.Miles >= 0.5 & bin_data$Trip.Miles < 3] <- km_bin[1]
      bin_data$Mile_Bin[bin_data$Trip.Miles >= 3 & bin_data$Trip.Miles < 6] <- km_bin[2]
      bin_data$Mile_Bin[bin_data$Trip.Miles >= 6 & bin_data$Trip.Miles < 9] <- km_bin[3]
      bin_data$Mile_Bin[bin_data$Trip.Miles >= 9 & bin_data$Trip.Miles < 12] <- km_bin[4]
      bin_data$Mile_Bin[bin_data$Trip.Miles >= 12 & bin_data$Trip.Miles < 15] <- km_bin[5]
      bin_data$Mile_Bin[bin_data$Trip.Miles >= 15 & bin_data$Trip.Miles < 30] <- km_bin[6]
      bin_data$Mile_Bin[bin_data$Trip.Miles >= 30 & bin_data$Trip.Miles < 45] <- km_bin[7]
      bin_data$Mile_Bin[bin_data$Trip.Miles >= 45 & bin_data$Trip.Miles < 60] <- km_bin[8]
      bin_data$Mile_Bin[bin_data$Trip.Miles >= 60 & bin_data$Trip.Miles < 75] <- km_bin[9]
      bin_data$Mile_Bin[bin_data$Trip.Miles >= 75 & bin_data$Trip.Miles < 90] <- km_bin[10]
      bin_data$Mile_Bin[bin_data$Trip.Miles >= 90 & bin_data$Trip.Miles < 105] <- km_bin[11]
      bin_data$Mile_Bin[bin_data$Trip.Miles >= 105 & bin_data$Trip.Miles < 120] <- km_bin[12]
      bin_data$Mile_Bin[bin_data$Trip.Miles >= 120 & bin_data$Trip.Miles < 135] <- km_bin[13]
      bin_data$Mile_Bin[bin_data$Trip.Miles >= 135 & bin_data$Trip.Miles <= 161] <- km_bin[14]
      bin_data$Mile_Bin <- factor(bin_data$Mile_Bin, levels = km_bin, ordered = TRUE)
      noFilter_byBinnedMileage <- aggregate(bin_data[,1], by=list(bin_data$Mile_Bin), FUN=length)
      colnames(noFilter_byBinnedMileage) <- c("KM", "Rides")
    }

    return(noFilter_byBinnedMileage)
  }) #Aggregates all stations by mileage
#
# #-------- FOR TIME ----------------------
#
#   # byTimeNoFilter <- function(){
#   #   # #Add to global
#   #   time_bin <- c("1 minute to < 5 minutes", "5 minute to < 10 minutes", "10 minute to < 30 minutes", "30 minute to < 1 hr", "1 hr to < 2 hr", "2 hr to < 3 hr", "3 hr to < 4 hr", "4 hr to 5 hr")
#   #
#   #   # #Actual code to get data frame
#   #   bin_data = data.frame(data)
#   #   bin_data$Time_Bin <- NA
#   #   bin_data$Time_Bin[bin_data$Trip.Seconds >= 60 & bin_data$Trip.Seconds < 300] <- time_bin[1]
#   #   bin_data$Time_Bin[bin_data$Trip.Seconds >= 300 & bin_data$Trip.Seconds < 600] <- time_bin[2]
#   #   bin_data$Time_Bin[bin_data$Trip.Seconds >= 600 & bin_data$Trip.Seconds < 1800] <- time_bin[3]
#   #   bin_data$Time_Bin[bin_data$Trip.Seconds >= 1800 & bin_data$Trip.Seconds < 3600] <- time_bin[4]
#   #   bin_data$Time_Bin[bin_data$Trip.Seconds >= 3600 & bin_data$Trip.Seconds < 7200] <- time_bin[5]
#   #   bin_data$Time_Bin[bin_data$Trip.Seconds >= 7200 & bin_data$Trip.Seconds < 10800] <- time_bin[6]
#   #   bin_data$Time_Bin[bin_data$Trip.Seconds >= 10800 & bin_data$Trip.Seconds < 14400] <- time_bin[7]
#   #   bin_data$Time_Bin[bin_data$Trip.Seconds >= 14400 & bin_data$Trip.Seconds < 18000] <- time_bin[8]
#   #   bin_data$Time_Bin <- factor(bin_data$Time_Bin, levels = time_bin, ordered = TRUE)
#   #   noFilter_byBinnedTime <- aggregate(bin_data[,1], by=list(bin_data$Time_Bin), FUN=length)
#   #   colnames(noFilter_byBinnedTime) <- c("Time", "Rides")
#   #   return(noFilter_byBinnedTime)
#   # }
#
  byTime <- function(){
    #If you don't include this, going back to city of chicago is slow
    if(input$select_community == "All (City of Chicago)" && input$select_company == "All Taxis"){
      return(by_binned_data[c(2,3)])
    }

    updateData <- filterData()
    #Aggregate by Trip Time
    print(min(updateData$Trip.Seconds))
    max(updateData$Trip.Seconds)

    # #Add to global
    time_bin <- c("1 minute to < 5 minutes", "5 minute to < 10 minutes", "10 minute to < 30 minutes", "30 minute to < 1 hr", "1 hr to < 2 hr", "2 hr to < 3 hr", "3 hr to < 4 hr", "4 hr to 5 hr")

    # #Actual code to get data frame
    bin_data = data.frame(updateData)
    bin_data$Time_Bin <- NA
    bin_data$Time_Bin[bin_data$Trip.Seconds >= 60 & bin_data$Trip.Seconds < 300] <- time_bin[1]
    bin_data$Time_Bin[bin_data$Trip.Seconds >= 300 & bin_data$Trip.Seconds < 600] <- time_bin[2]
    bin_data$Time_Bin[bin_data$Trip.Seconds >= 600 & bin_data$Trip.Seconds < 1800] <- time_bin[3]
    bin_data$Time_Bin[bin_data$Trip.Seconds >= 1800 & bin_data$Trip.Seconds < 3600] <- time_bin[4]
    bin_data$Time_Bin[bin_data$Trip.Seconds >= 3600 & bin_data$Trip.Seconds < 7200] <- time_bin[5]
    bin_data$Time_Bin[bin_data$Trip.Seconds >= 7200 & bin_data$Trip.Seconds < 10800] <- time_bin[6]
    bin_data$Time_Bin[bin_data$Trip.Seconds >= 10800 & bin_data$Trip.Seconds < 14400] <- time_bin[7]
    bin_data$Time_Bin[bin_data$Trip.Seconds >= 14400 & bin_data$Trip.Seconds < 18000] <- time_bin[8]
    bin_data$Time_Bin <- factor(bin_data$Time_Bin, levels = time_bin, ordered = TRUE)
    noFilter_byBinnedTime <- aggregate(bin_data[,1], by=list(bin_data$Time_Bin), FUN=length)
    colnames(noFilter_byBinnedTime) <- c("Time", "Rides")
    return(noFilter_byBinnedTime)
  } #Aggregates all stations by specific month
#
# #-------- FOR SCOPES GRAPH AND TABLE FUNCTIONS -------------------
  graph_scope_chart <- reactive({

    distribution <- input$select_distribution # distributionType <- c("By Day", "By Hour of Day", "By Day of Week", "By Month", "By Binned Mileage", "By Binned Trip Time")

    g <- NULL

    if(distribution == "By Day")
    {
      noFilter_byDay <- byDay()
      g <- ggplot(data=noFilter_byDay, aes(x=ymd(`Date`), y=`Rides`)) + geom_bar(stat="identity", fill="lightgreen") + theme(axis.text.x = element_text(angle = 90)) +
        xlab("Date (Per Day)")
    }
    else if(distribution == "By Hour of Day")
    {
      noFilter_byHour <- byHour()
      if(input$time == "twentyFourHr"){
        g <- ggplot(data=noFilter_byHour, aes(x=`Hour`, y=`Rides`)) + geom_bar(stat="identity", fill="blue")
      }
      else{
        label <- c("12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM", "6 AM", "7 AM", "8 AM", "9 AM", "10 AM", "11 AM", "12 PM",
                   "1 PM","2 PM","3 PM","4 PM","5 PM","6 PM","7 PM","8 PM","9 PM","10 PM","11 PM")
        g <- ggplot(data=noFilter_byHour, aes(x=`Hour`, y=`Rides`, fill=label)) + geom_bar(stat="identity", fill="blue")
      }
    }
    else if(distribution == "By Day of Week")
    {
      noFilter_byDayOfWeek <- byDayOfWeek()
      DaysOfTheWeek <- c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun")
      # noFilter_byDayOfWeek <- cbind(noFilter_byDayOfWeek, DaysOfTheWeek)
      noFilter_byDayOfWeek$DaysOfTheWeek[noFilter_byDayOfWeek["Day Of Week"] == 1] <- "Mon"
      noFilter_byDayOfWeek$DaysOfTheWeek[noFilter_byDayOfWeek["Day Of Week"] == 2] <- "Tue"
      noFilter_byDayOfWeek$DaysOfTheWeek[noFilter_byDayOfWeek["Day Of Week"] == 3] <- "Wed"
      noFilter_byDayOfWeek$DaysOfTheWeek[noFilter_byDayOfWeek["Day Of Week"] == 4] <- "Thur"
      noFilter_byDayOfWeek$DaysOfTheWeek[noFilter_byDayOfWeek["Day Of Week"] == 5] <- "Fri"
      noFilter_byDayOfWeek$DaysOfTheWeek[noFilter_byDayOfWeek["Day Of Week"] == 6] <- "Sat"
      noFilter_byDayOfWeek$DaysOfTheWeek[noFilter_byDayOfWeek["Day Of Week"] == 7] <- "Sun"
      noFilter_byDayOfWeek$DaysOfTheWeek <- factor(noFilter_byDayOfWeek$DaysOfTheWeek, levels = DaysOfTheWeek, ordered = TRUE)
      print(noFilter_byDayOfWeek)
      g <- ggplot(data=noFilter_byDayOfWeek, aes(x=`Day Of Week`, y=`Rides`, fill=DaysOfTheWeek)) + geom_bar(stat="identity")
    }
    else if(distribution == "By Month")
    {
      noFilter_byMonth<- byMonth()
      g <- ggplot(data=noFilter_byMonth, aes(x=`Month`, y=`Rides`)) + geom_bar(stat="identity", fill="purple")
    }
    else if(distribution == "By Binned Mileage")
    {
      noFilter_byMileage <- byMileage()
      if(input$measurement == "miles"){
        g <- ggplot(data=noFilter_byMileage, aes(x=`Miles`, y=`Rides`)) + geom_bar(stat="identity", fill="red")
      }else{
        g <- ggplot(data=noFilter_byMileage, aes(x=`KM`, y=`Rides`)) + geom_bar(stat="identity", fill="red")
      }
    }
    else if(distribution == "By Binned Trip Time")
    {
      noFilter_byTime<- byTime()
      g <- ggplot(data=noFilter_byTime, aes(x=`Time`, y=`Rides`)) + geom_bar(stat="identity", fill="orange")
    }

    return(g)
  })
#
  scope_table <- reactive({
    distribution <- input$select_distribution # distributionType <- c("By Day", "By Hour of Day", "By Day of Week", "By Month", "By Binned Mileage", "By Binned Trip Time")

    g <- NULL

    if(distribution == "By Day")
    {
      g <- byDay()
    }
    else if(distribution == "By Hour of Day")
    {
      g <- byHour()
    }
    else if(distribution == "By Day of Week")
    {
      g <- byDayOfWeek()
    }
    else if(distribution == "By Month")
    {
      g <- byMonth()
    }
    else if(distribution == "By Binned Mileage")
    {
      g <- byMileage()
    }
    else if(distribution == "By Binned Trip Time")
    {
      g <- byTime()
    }

  })
#
 output$scopechart <- renderPlot({   graph_scope_chart()  })
#
  output$table_scopes <- renderDataTable(scope_table(), options = list(pageLength = 10))

#--------------- FOR COMMUNITIES -----------------

graph_community_chart <- function(){
  g <- NULL

  community <- community_data()
  community <- community[order(community$Community),, drop=FALSE]

  g <- ggplot(community, aes(x=`Community`, y=`Percentage`)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90))

  return(g)

}
#
  output$communitychart <- renderPlot({ graph_community_chart() })
# #
# # #--------------- FOR LEAFLET -----------------
  community_data <- reactive({
    eachCommunity <- data.frame(data)
    comm_range <- 1:77
    company <- input$select_company
    trip <- input$destination

    if(company != "All Taxis"){
      eachCommunity <- subset(eachCommunity, eachCommunity$Company == company)
    }

    if(trip == "goingTo"){
      eachCommunity <- aggregate(eachCommunity[,1], by=list(eachCommunity$Dropoff.Community.Area), FUN=length)
    }else{
      eachCommunity <- aggregate(eachCommunity[,1], by=list(eachCommunity$Pickup.Community.Area), FUN=length)
    }

    colnames(eachCommunity) <- c("Community", "Percentage")
    #Fill in missing communities with 0's
    for(i in 1:77){
      hasCommunity <- FALSE
      for(j in 1:length(eachCommunity[,1])){
        if(eachCommunity[j,1] == i){
          hasCommunity <- TRUE
          break
        }
      }

      if(!hasCommunity){
        eachCommunity[nrow(eachCommunity) + 1,] = c(i, 0)
      }
    }

    eachCommunity <- eachCommunity[order(eachCommunity$Community),]
    rownames(eachCommunity) <- 1:nrow(eachCommunity)

    #CONVERT COMMUNITY ID TO NAMES
    for(i in 1:77){
      eachCommunity[i,1] <- community_list[i]
    }

    #Table them as percentages
    count_sum <- sum(eachCommunity$Percentage)
    eachCommunity$Percentage <- eachCommunity$Percentage/count_sum

    return(eachCommunity)
  })

  observeEvent(input$mymap3_shape_click,{
    p <- input$mymap3_shape_click
    updateSelectInput(session, "select_community", selected= p$id)
  })

  output$mymap3 <- renderLeaflet({
    comm_df <- community_data()
    pal <- colorBin("YlOrRd", comm_df$Percentage, 9, pretty = FALSE)
    leaflet(chi_sp) %>%
      addTiles() %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                  fillColor = ~pal(comm_df[area_num_1, 2]),
                  popup = ~paste(sep="<br/>", community, comm_df[area_num_1, 2]),
                  layerId = ~comm_df[area_num_1, 1]) %>%
      addLegend(pal = pal, values = comm_df$Percentage, opacity = 1.0)
  })

#-------- END OF EVERYTHING --------------------

}

shinyApp(ui = ui, server = server)
