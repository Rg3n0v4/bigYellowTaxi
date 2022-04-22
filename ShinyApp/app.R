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

  # distribution <- "All" NOTE: I realize this isn't gonna work. Put this in the scope chart method instead

  filterData <- reactive({
    updateData <- data.frame(data)
    community <- input$select_community
    trip <- input$destination
    units <- "MI"
    company <- input$select_company

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

    #TODO
    #add Mile / KM data changing option
    #add 12 hr and 24 hr data changing option

    return(updateData)
  })

#------------------ FOR DAY ----------------  
  byDayNoFilter <- reactive({
    noFilter_byDay <- aggregate(data[,1], by=list(date(data$new_date)), FUN=length)
    colnames(noFilter_byDay) <- c("Date", "Rides")
    return(noFilter_byDay)
  })
  
  byDay <- reactive({
    #If you don't include this, going back to city of chicago is slow
    if(input$select_community == "All (City of Chicago)" && input$select_company == "All Taxis"){
      return(byDayNoFilter())
    }
    
    updateData <- filterData()
    noFilter_byDay <- aggregate(updateData[,1], by=list(date(updateData$new_date)), FUN=length)
    colnames(noFilter_byDay) <- c("Date", "Rides")
    return(noFilter_byDay)
  }) #Aggregates all stations by specific date


#------------------ FOR HOUR ------------------
  byHourNoFilter <- reactive({
    noFilter_byHour <- aggregate(data[,1], by=list(data$Hour), FUN=length)
    colnames(noFilter_byHour) <- c("Hour", "Rides")
    return(noFilter_byHour)
  })
  
  byHour <- reactive({
    #If you don't include this, going back to city of chicago is slow
    if(input$select_community == "All (City of Chicago)" && input$select_company == "All Taxis"){
      return(byHourNoFilter())
    }
    
    updateData <- filterData()
    noFilter_byHour <- aggregate(updateData[,1], by=list(updateData$Hour), FUN=length)
    colnames(noFilter_byHour) <- c("Hour", "Rides")
    return(noFilter_byHour)
  }) #Aggregates all stations by specific hour
  
#----------- FOR DAY OF WEEK -----------------
  #* STARTS MONDAY TO SUNDAY WITH 1 BEING MONDAY
  
  byDayOfWeekNoFilter <- reactive({
    noFilter_byDayOfWeek <- aggregate(data[,1], by=list(wday(data$new_date, week_start=1)), FUN=length)
    colnames(noFilter_byDayOfWeek) <- c("Day Of Week", "Rides")
    return(noFilter_byDayOfWeek)
  })
  
  byDayOfWeek <- reactive({
    #If you don't include this, going back to city of chicago is slow
    if(input$select_community == "All (City of Chicago)" && input$select_company == "All Taxis"){
      return(byDayOfWeekNoFilter())
    }
    
    updateData <- filterData()
    #Aggregate by weekday
    noFilter_byDayOfWeek <- aggregate(updateData[,1], by=list(wday(updateData$new_date, week_start=1)), FUN=length)
    colnames(noFilter_byDayOfWeek) <- c("Day Of Week", "Rides")
    
    return(noFilter_byDayOfWeek)
  }) #Aggregates all stations by specific day of week
  
#-------- FOR MONTH ----------------------
  
  byMonthNoFilter <- reactive({
    noFilter_byMonth <- aggregate(data[,1], by=list(month(data$new_date)), FUN=length)
    colnames(noFilter_byMonth) <- c("Month", "Rides")
    return(noFilter_byMonth)
  })
  
  byMonth <- reactive({
    #If you don't include this, going back to city of chicago is slow
    if(input$select_community == "All (City of Chicago)" && input$select_company == "All Taxis"){
      return(byMonthNoFilter())
    }
    
    updateData <- filterData()
    #Aggregate by month
    noFilter_byMonth<- aggregate(updateData[,1], by=list(month(updateData$new_date)), FUN=length)
    colnames(noFilter_byMonth) <- c("Month", "Rides")
    
    return(noFilter_byMonth)
  }) #Aggregates all stations by specific month
  
  
#-------- FOR MILEAGE ----------------------
  
  byMileagehNoFilter <- reactive({
    #Aggregate by binned mileage 
    
    #Add to global
    mile_bin <- c("0.5 to < 10", "10 to < 20", "20 to < 30", "30 to < 40", "40 to < 50", "50 to < 60", "60 to < 70", "70 to < 80", "80 to < 90", "90 to 100")
    
    #Actual code to get data frame
    bin_data = data.frame(data)
    bin_data$Mile_Bin <- NA
    bin_data$Mile_Bin[bin_data$Trip.Miles >= 0.5 & bin_data$Trip.Miles < 10] <- mile_bin[1]
    bin_data$Mile_Bin[bin_data$Trip.Miles >= 10 & bin_data$Trip.Miles < 20] <- mile_bin[2]
    bin_data$Mile_Bin[bin_data$Trip.Miles >= 20 & bin_data$Trip.Miles < 30] <- mile_bin[3]
    bin_data$Mile_Bin[bin_data$Trip.Miles >= 30 & bin_data$Trip.Miles < 40] <- mile_bin[4]
    bin_data$Mile_Bin[bin_data$Trip.Miles >= 40 & bin_data$Trip.Miles < 50] <- mile_bin[5]
    bin_data$Mile_Bin[bin_data$Trip.Miles >= 50 & bin_data$Trip.Miles < 60] <- mile_bin[6]
    bin_data$Mile_Bin[bin_data$Trip.Miles >= 60 & bin_data$Trip.Miles < 70] <- mile_bin[7]
    bin_data$Mile_Bin[bin_data$Trip.Miles >= 70 & bin_data$Trip.Miles < 80] <- mile_bin[8]
    bin_data$Mile_Bin[bin_data$Trip.Miles >= 80 & bin_data$Trip.Miles < 90] <- mile_bin[9]
    bin_data$Mile_Bin[bin_data$Trip.Miles >= 90 & bin_data$Trip.Miles <= 100] <- mile_bin[10]
    bin_data$Mile_Bin <- as.factor(bin_data$Mile_Bin)
    
    noFilter_byBinnedMileage <- aggregate(bin_data[,1], by=list(bin_data$Mile_Bin), FUN=length)
    colnames(noFilter_byBinnedMileage) <- c("Miles", "Rides")
    return(noFilter_byBinnedMileage)
  })
  
  byMileage <- reactive({
    #If you don't include this, going back to city of chicago is slow
    if(input$select_community == "All (City of Chicago)" && input$select_company == "All Taxis"){
      return(byMileagehNoFilter())
    }
    
    updateData <- filterData()
    #Aggregate by binned mileage 
    
    #Add to global
    mile_bin <- c("0.5 to < 10", "10 to < 20", "20 to < 30", "30 to < 40", "40 to < 50", "50 to < 60", "60 to < 70", "70 to < 80", "80 to < 90", "90 to 100")
    
    #Actual code to get data frame
    bin_data = data.frame(updateData)
    bin_data$Mile_Bin <- NA
    bin_data$Mile_Bin[bin_data$Trip.Miles >= 0.5 & bin_data$Trip.Miles < 10] <- mile_bin[1]
    bin_data$Mile_Bin[bin_data$Trip.Miles >= 10 & bin_data$Trip.Miles < 20] <- mile_bin[2]
    bin_data$Mile_Bin[bin_data$Trip.Miles >= 20 & bin_data$Trip.Miles < 30] <- mile_bin[3]
    bin_data$Mile_Bin[bin_data$Trip.Miles >= 30 & bin_data$Trip.Miles < 40] <- mile_bin[4]
    bin_data$Mile_Bin[bin_data$Trip.Miles >= 40 & bin_data$Trip.Miles < 50] <- mile_bin[5]
    bin_data$Mile_Bin[bin_data$Trip.Miles >= 50 & bin_data$Trip.Miles < 60] <- mile_bin[6]
    bin_data$Mile_Bin[bin_data$Trip.Miles >= 60 & bin_data$Trip.Miles < 70] <- mile_bin[7]
    bin_data$Mile_Bin[bin_data$Trip.Miles >= 70 & bin_data$Trip.Miles < 80] <- mile_bin[8]
    bin_data$Mile_Bin[bin_data$Trip.Miles >= 80 & bin_data$Trip.Miles < 90] <- mile_bin[9]
    bin_data$Mile_Bin[bin_data$Trip.Miles >= 90 & bin_data$Trip.Miles <= 100] <- mile_bin[10]
    bin_data$Mile_Bin <- as.factor(bin_data$Mile_Bin)
    
    noFilter_byBinnedMileage <- aggregate(bin_data[,1], by=list(bin_data$Mile_Bin), FUN=length)
    colnames(noFilter_byBinnedMileage) <- c("Miles", "Rides")
    noFilter_byBinnedMileage
    
    return(noFilter_byBinnedMileage)
  }) #Aggregates all stations by mileage
  
#-------- FOR TIME ----------------------
  
  byTimeNoFilter <- reactive({
    # #Add to global
    time_bin <- c("1 minute to < 5 minutes", "5 minute to < 10 minutes", "10 minute to < 30 minutes", "30 minute to < 1 hr", "1 hr to < 2 hr", "2 hr to < 3 hr", "3 hr to < 4 hr", "4 hr to 5 hr")
    
    # #Actual code to get data frame
    bin_data = data.frame(data)
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
  })
  
  byTime <- reactive({
    #If you don't include this, going back to city of chicago is slow
    if(input$select_community == "All (City of Chicago)" && input$select_company == "All Taxis"){
      return(byTimeNoFilter())
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
  }) #Aggregates all stations by specific month

#--------------- graph_scope_chart function -------------------
  graph_scope_chart <- function(){
    
    distribution <- input$select_distribution # distributionType <- c("By Day", "By Hour of Day", "By Day of Week", "By Month", "By Binned Mileage", "By Binned Trip Time")

    # d_graph <- allStops1()
    g <- NULL
    # i_order = orderOpt2()
    
    if(distribution == "By Day")
    {
      noFilter_byDay <- byDay()
      g <- ggplot(data=noFilter_byDay, aes(x=`Date`, y=`Rides`)) + geom_bar(stat="identity")
    }
    else if(distribution == "By Hour of Day")
    {
      noFilter_byHour <- byHour()
      g <- ggplot(data=noFilter_byHour, aes(x=`Hour`, y=`Rides`)) + geom_bar(stat="identity")
    }
    else if(distribution == "By Day of Week")
    {
      noFilter_byDayOfWeek <- byDayOfWeek()
      g <- ggplot(data=noFilter_byDayOfWeek, aes(x=`Day Of Week`, y=`Rides`)) + geom_bar(stat="identity")
    }
    else if(distribution == "By Month")
    {
      noFilter_byMonth<- byMonth()
      g <- ggplot(data=noFilter_byMonth, aes(x=`Month`, y=`Rides`)) + geom_bar(stat="identity")
    }
    else if(distribution == "By Binned Mileage")
    {
      noFilter_byMileage <- byMileage()
      g <- ggplot(data=noFilter_byMileage, aes(x=`Miles`, y=`Rides`)) + geom_bar(stat="identity")
    }
    else if(distribution == "By Binned Trip Time")
    {
      noFilter_byTime<- byTime()
      g <- ggplot(data=noFilter_byTime, aes(x=`Time`, y=`Rides`)) + geom_bar(stat="identity")
    }



    return(g)
  }

  output$scopechart <- renderPlot({   graph_scope_chart()  })

}

shinyApp(ui = ui, server = server)
