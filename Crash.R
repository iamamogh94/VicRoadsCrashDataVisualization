
# Shiny URL
# https://iamamogh.shinyapps.io/CrashDataVisualization/

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(magrittr)
library(haven)
library(lubridate)
library(tidyverse)
library(rsconnect)
library(readr)
library(ggplot2)
library(shiny)
library(plotly)
library(grid)
library(leaflet)


crash_df <-
  read.csv("Crashes_Last_Five_Years.csv", na.strings = c("", " ", "NA"))


mapgraph_df <-
  crash_df %>%
  select(
    ACCIDENT_DATE,
    DAY_OF_WEEK,
    ROAD_GEOMETRY,
    LONGITUDE,
    LATITUDE,
    ALCOHOL_RELATED,
    ACCIDENT_TYPE,
    ACCIDENT_TIME
  )


mapgraph_df$ACCIDENT_TIME <-
  str_replace_all(mapgraph_df$ACCIDENT_TIME, "\\.", ":")
mapgraph_df$ACCIDENT_TIME <- substr(mapgraph_df$ACCIDENT_TIME, 1, 5)

mapgraph_df$ACCIDENT_DATE <-
  str_replace_all(mapgraph_df$ACCIDENT_DATE, "/", "-")
mapgraph_df = mapgraph_df %>%
  mutate(ACCIDENT_DATE = dmy(ACCIDENT_DATE)) %>%
  mutate_at(vars(ACCIDENT_DATE), funs(day, month, year))

mapgraph_df$DAY_OF_WEEK <- mapgraph_df$DAY_OF_WEEK %>% as.factor()
mapgraph_df$ROAD_GEOMETRY <-
  mapgraph_df$ROAD_GEOMETRY %>% as.factor()
mapgraph_df$ALCOHOL_RELATED <-
  mapgraph_df$ALCOHOL_RELATED %>% as.factor()
mapgraph_df$ACCIDENT_TYPE <-
  mapgraph_df$ACCIDENT_TYPE %>% as.factor()

mapgraph_df <- mapgraph_df %>%
  rename(YEAR = year,
         MONTH = month,
         DAY = day)

mapgraph_df <- mapgraph_df[complete.cases(mapgraph_df),]
# --------------------------------


# Data PreProcessing for InjuriesData
injuriesData <-
  crash_df %>%
  select(ACCIDENT_DATE,
         INJ_OR_FATAL,
         FATALITY)

injuriesData$ACCIDENT_DATE <-
  str_replace_all(injuriesData$ACCIDENT_DATE, "/", "-")
injuriesData = injuriesData %>%
  mutate(ACCIDENT_DATE = dmy(ACCIDENT_DATE)) %>%
  mutate_at(vars(ACCIDENT_DATE), funs(day, month, year))

injuriesData <- injuriesData %>%
  rename(YEAR = year,
         MONTH = month,
         DAY = day)

injuriesData$YEAR <- injuriesData$YEAR %>% as.factor()
injuriesData <-
  injuriesData %>% mutate(MONTH = factor(
    MONTH,
    levels = c("1", "2",
               "3", "4",
               "5", "6",
               "7", "8",
               "9", "10",
               "11", "12"),
    labels = c(
      "Jan",
      "Feb",
      "Mar",
      "Apr",
      "May",
      "Jun",
      "Jul",
      "Aug",
      "Sep",
      "Oct",
      "Nov",
      "Dec"
    ),
    ordered = TRUE
  ))


injuriesData$DAY <- injuriesData$DAY %>% as.factor()



ui1 <- fluidPage(
  theme = shinytheme("united"),
  
  titlePanel("Victoria Crash Data"),
  
  sidebarPanel(
    p(
      "This ShinnyApp provides you information on the crashes on Victorian Roads."
    ),
    p(
      "Please choose your preference from the below radio button to view the data."
    ),
    p(
      "__________________________________________________________"
    ),
    radioGroupButtons(
      "CH",
      "Select an option:",
      choices = as.vector(c("Details", "Map", "Graph")),
      selected = "Details"
    ),
    
    conditionalPanel(
      condition = "input.CH == 'Map'",
      
      sliderInput(
        "Y",
        "Select range of Years:",
        min = 2013,
        max = 2019,
        value = c(2013, 2019),
        sep = "",
        ticks = FALSE
      ),
      
      selectInput(
        "D",
        "Select Day of Week:",
        c(
          "Sunday",
          "Monday",
          "Tuesday",
          "Wednesday",
          "Thursday",
          "Friday",
          "Saturday"
        ),
        selected = "Sunday"
      ),
      
      selectInput(
        "RG",
        "Select the Road Geometry:",
        c(
          "Cross intersection",
          "Dead end",
          "Multiple intersection",
          "Not at intersection",
          "T intersection",
          "Unknown",
          "Y intersection"
        ),
        selected = "Not at intersection"
      ),
      
      selectInput(
        "AT",
        "Select Accident type:",
        choices = list(
          "Struck Pedestrian" = "Struck Pedestrian",
          "Collision with vehicle" = "Collision with vehicle",
          "Collision with a fixed object" = "Collision with a fixed object",
          "collision with some other object" = "collision with some other object",
          "Fall from or in moving vehicle" = "Fall from or in moving vehicle",
          "No collision and no object struck" = "No collision and no object struck",
          "Other accident" = "Other accident",
          "Struck animal" = "Struck animal",
          "Vehicle overturned (no collision)" = "Vehicle overturned (no collision)"
        ),
        selected = "CNo collision and no object struck"
      ),
      
      
      radioButtons(
        "AR",
        "Alcohol Related:",
        choices = list("Alcholic" = "Yes",
                       "Non Alcholic" = "No"),
        selected = "No"
      )
    ),
    
    conditionalPanel(
      condition = "input.CH == 'Graph'",
      p(
        "Please select 'Year' and 'Month' to get the day to day number of casualties across Victoria."
      ),
      selectInput(
        "YE",
        "Select Year:",
        c("2013",
          "2014",
          "2015",
          "2016",
          "2017",
          "2018",
          "2019"),
        selected = "2013"
      ),
      
      selectInput("MO", "Select the Month", choices = NULL)
    ),
    
    conditionalPanel(
      condition = "input.CH == 'Details'",
      tags$b("Details:"),
      p("Detailed description of data and assignment submission."),
      tags$b("Map:"),
      p("Visualizes the crash locations on the Victoria State map."),
      tags$b("Graph:"),
      p(
        "Graphical repesentation of casualties that occured on each day of the selected month and year."
      )
      
    )
  ),
  
  mainPanel(
    conditionalPanel(
      condition = "input.CH == 'Map'",
      p(
        "The below map shows the location of crashes occured. Please Double-click on the map to zoom-in. Single-click on the location point to know the time of the accident."
      ),
      leafletOutput("aussiemap", width = "900px", height = "600px"),
      p(" "),
      p("Follow Traffic Rules...!!!"),
      p("Normal speed meets every need...!! Stop accidents before they stop you.")
    ),
    conditionalPanel(
      condition = "input.CH == 'Graph'",
      p(
        "Interactive graph: Each bar denotes the number of casualities each day of the selected month and year.
        Hover on each bar to know the injury and fatality count."
      ),
      plotlyOutput("injuries", width = "900px", height = "600px"),
      p(" "),
      p("Those are big numbers...!! We do not want you to count; We want you to think the count."),
      p("Drive Slow. Have another day :)")
    ),
    conditionalPanel(
      condition = "input.CH == 'Details'",
      h1("MATH2270, Data Visualization and Communication"),
      h5("Semeter 1, 2020 :: Assignment 03"),
      h5("Student Details: Amogha Amaresh, s3789170"),
      p(" "),
      p(" "),
      h1("Data Description"),
      p(
        "The data is of Fatal and injuries crashes on Victorian roads during 2013 July to 2019 March.
                      This visualization allows you to analyse Victoria's fatal and injury crash data based on Year,
                      Day of the week,
                      Road geometry, Accident type, Alcohol related."
      ),
      p(" "),
      p(" "),
      h1("Visualization Description"),
      p(
        "This Data visualization aims to spread awareness about the road accidents by visualizing the crash data on a day to day
        basis an also by pointing the crash locations on the Victorian state map."
      ),
      p(" "),
      p(" "),
      h1("Reference"),
      p(
        "Crashes Last Five Years. (2020, Jun 05). Retrieved from Data Vic:",
        tags$a(
          href = "https://discover.data.vic.gov.au/dataset/crashes-last-five-years1",
          "https://discover.data.vic.gov.au/dataset/crashes-last-five-years1"
        )
      ),
      p(" "),
      p(
        "Shiny. (n.d.). Retrieved from Shiny Rstudio:",
        tags$a(href = "https://shiny.rstudio.com/", "https://shiny.rstudio.com/")
      )
    )
  )
)


server1 <- function(input, output, session) {
  observe({
    year <- input$YE
    injuries_year_Data <-
      injuriesData %>%
      filter(YEAR == year,)
    uniq_months <- unique(injuries_year_Data$MONTH)
    print(uniq_months)
    updateSelectInput(session, "MO", "Select the Month", choices = uniq_months)
  })
  
  
  output$injuries <- renderPlotly({
    
    year <- input$YE
    mon <- input$MO
    
    injuries_year_Data <-
      injuriesData %>%
      filter(YEAR == year,)
    
    injuries_month_Data <-
      injuries_year_Data %>%
      filter(MONTH == mon,)
    
    injuries_month_Data <-
      injuries_month_Data %>% mutate(injured = INJ_OR_FATAL - FATALITY)
    
    injuries_final_Data <-
      injuries_month_Data %>%
      group_by(DAY)  %>%
      summarise(
        FATALITY = sum(FATALITY),
        INJURED = sum(injured),
        TOTAL = sum(INJ_OR_FATAL)
      )
    
    f <- list(family = "Courier New, monospace",
              size = 18,
              color = "black")
    x <- list(title = "Day of the Month",
              titlefont = f)
    y <- list(title = "Total Number of Casualties",
              titlefont = f)
    
    fig <-
      plot_ly(
        injuries_final_Data,
        x = ~ DAY,
        y = ~ TOTAL,
        marker = list(
          color = 'rgba(222,45,38,0.6)',
          line = list(color = 'rgb(8,48,107)', width = 1.5)
        ),
        type = "bar",
        text = ~paste("This includes <br> Injuries :", INJURED,
                      "<br> Fatality :", FATALITY),
        hoverinfo = "text"
      )
    fig <- fig %>% layout(xaxis = x, yaxis = y)
    
  })
  
  
  output$aussiemap <- renderLeaflet({
    minyear <- input$Y[1]
    maxyear <- input$Y[2]
    rg <- input$RG
    dayofweek <- input$D
    alcohol <- input$AR
    acctype <- input$AT
    
    mapgraph_final_df <-
      mapgraph_df %>%
      filter(
        YEAR >= minyear,
        YEAR <= maxyear,
        ROAD_GEOMETRY == rg,
        DAY_OF_WEEK == dayofweek,
        ALCOHOL_RELATED == alcohol,
        ACCIDENT_TYPE == acctype
      )
    
    leaflet(mapgraph_final_df) %>%
      addCircles(lng = ~ LONGITUDE, lat = ~ LATITUDE) %>%
      addTiles()  %>%
      addCircleMarkers(
        data = mapgraph_final_df,
        lat =  ~ LATITUDE,
        lng =  ~ LONGITUDE,
        radius = 3,
        popup = ~ as.character(ACCIDENT_TIME),
        stroke = FALSE,
        fillOpacity = 0.8
      ) %>%
      setView(lng = 144.96,
              lat = -37.81,
              zoom = 8)
    
  })
  
}


shinyApp(ui1, server1)

  