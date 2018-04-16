#
#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(magrittr)
library(gepaf)
library(geosphere)

df_Path <-read.csv('Path_Dump.csv')
df_Path$date <- as.POSIXct(strptime(df_Path$date, "%Y-%m-%d"))

mydf <- read.csv('Data_Dump.csv')

mydf$created_at <- as.POSIXct(strptime(mydf$created_at, "%Y-%m-%d %H:%M:%S"))
mydf <- mydf[ order(mydf$created_at , decreasing = TRUE ),]

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  

    pathData <- reactive({
      if (input$date == '2018-01-31'){
        start = as.POSIXct('2018-01-31 14:36:00')
        end = as.POSIXct('2018-01-31 17:30:00')
      }
      else if (input$date == '2018-02-01'){
        start = as.POSIXct('2018-02-01 14:25:00')
        end = as.POSIXct('2018-02-01 17:10:00')
      }
      else if (input$date == '2018-02-02'){
        start = as.POSIXct('2018-02-02 14:25:00')
        end = as.POSIXct('2018-02-02 16:35:00')
      }
      else if (input$date == '2018-02-05'){
        start = as.POSIXct('2018-02-05 14:28:00')
        end = as.POSIXct('2018-02-05 17:18:00')
      }
      else if (input$date == '2018-02-06'){
        start = as.POSIXct('2018-02-06 14:30:00')
        end = as.POSIXct('2018-02-06 17:01:00')
      }
      else if (input$date == '2018-02-07'){
        start = as.POSIXct('2018-02-07 14:24:00')
        end = as.POSIXct('2018-02-07 17:03:00')
      }
      else if (input$date == '2018-02-08'){
        start = as.POSIXct('2018-02-08 14:30:00')
        end = as.POSIXct('2018-02-08 16:51:00')
      }
      else if (input$date == '2018-02-09'){
        start = as.POSIXct('2018-02-09 14:40:00')
        end = as.POSIXct('2018-02-09 17:11:00')
      }
      else if (input$date == '2018-02-12'){
        start = as.POSIXct('2018-02-12 14:24:00')
        end = as.POSIXct('2018-02-12 17:36:00')
      }
      else if (input$date == '2018-02-13'){
        start = as.POSIXct('2018-02-13 14:27:00')
        end = as.POSIXct('2018-02-13 17:21:00')
      }
      else if (input$date == '2018-03-13'){
        start = as.POSIXct('2018-03-13 13:12:00')
        end = as.POSIXct('2018-03-13 15:28:00')
      }
      else if (input$date == '2018-03-15'){
        start = as.POSIXct('2018-03-15 13:25:00')
        end = as.POSIXct('2018-03-15 15:50:00')
      }
      else if (input$date == '2018-03-19'){
        start = as.POSIXct('2018-03-19 13:25:00')
        end = as.POSIXct('2018-03-19 15:44:00')
      }
      else if (input$date == '2018-03-20'){
        start = as.POSIXct('2018-03-20 13:20:00')
        end = as.POSIXct('2018-03-20 15:47:00')
      }
      else if (input$date == '2018-03-21'){
        start = as.POSIXct('2018-03-21 13:51:00')
        end = as.POSIXct('2018-03-21 16:00:00')
      }
      else if (input$date == '2018-03-22'){
        start = as.POSIXct('2018-03-22 13:00:00')
        end = as.POSIXct('2018-03-22 16:03:00')
      }
      else if (input$date == '2018-03-23'){
        start = as.POSIXct('2018-03-23 13:22:00')
        end = as.POSIXct('2018-03-23 15:35:00')
      }
      else if (input$date == '2018-03-27'){
        start = as.POSIXct('2018-03-27 13:24:00')
        end = as.POSIXct('2018-03-27 16:05:00')
      }
      else if (input$date == '2018-04-09'){
        start = as.POSIXct('2018-04-09 13:57:00')
        end = as.POSIXct('2018-04-09 16:16:00')
      }
      else if (input$date == '2018-04-10'){
        start = as.POSIXct('2018-04-10 13:31:00')
        end = as.POSIXct('2018-04-10 15:31:00')
      }
        with(mydf, subset(mydf, 
                          created_at >= start & 
                            created_at <= end & latitude > 53 & longitude < -113 & 
                            accuracy <= 150))
    })
    
  suggestedPath <- reactive({
    if (input$date == '2018-03-19'){
      with(df_Path, subset(df_Path, date == input$date & date_feature == 3))
    }
    else if (input$date == '2018-04-10'){
      with(df_Path, subset(df_Path, date == input$date & date_feature == 2))
    }
    else{
      with(df_Path, subset(df_Path, date == input$date, date_feature = 0))
    }
  })
  
  output$mymap <- renderLeaflet({
    
  if (input$date >= '2018-03-07'){
    data <- pathData()
    path <- suggestedPath()
    a <- as.character(path[1, 'path'])
    path <- decodePolyline(a)
    
    distance <- 0
    for (i in 1:(nrow(data) - 1)){
      distance <- distance + distm(c(data[i,]$longitude, data[i,]$latitude), 
                                   c(data[i+1,]$longitude, data[i+1,]$latitude), 
                                   fun=distVincentySphere)[1]
    }
    
    distance <- distance/1000
    
    content <- paste(sep = "",
                     "Time = ", round(max(data$created_at) - min(data$created_at), digits = 2), 
                     " hr<br> Distance = ", round(distance, 2), " KM")
    
    leaflet()%>%
      addTiles() %>%
      addPolylines(data = data, lng = ~longitude, lat = ~latitude, color='#0C0C26', 
                   opacity = 1, weight=3) %>%
      addPolylines(data = path, lng = ~lon, lat = ~lat,  
                   color = '#3F7FBF', opacity = 0.8) %>%
      addControl(content, 
                 position = "bottomleft") %>%
      addLegend("bottomright", colors = c('#0C0C26', '#3F7FBF'), 
                labels = c('Path Followed', 'Suggested Path'),
                title = "Path",
                opacity = 1)
  }
    else {
      data <- pathData()
      
      distance <- 0
      for (i in 1:(nrow(data) - 1)){
        distance <- distance + distm(c(data[i,]$longitude, data[i,]$latitude), 
                                     c(data[i+1,]$longitude, data[i+1,]$latitude), 
                                     fun=distVincentySphere)[1]
      }
      
      distance <- distance/1000
      
      content <- paste(sep = "",
                       "Time = ", round(max(data$created_at) - min(data$created_at), digits = 2), 
                       " hr<br> Distance = ", round(distance, 2), " KM")
      
      leaflet()%>%
        addTiles() %>%
        addPolylines(data = data, lng = ~longitude, lat = ~latitude, color='#0C0C26', 
                     opacity = 1, weight=3) %>%
        addControl(content, 
                   position = "bottomleft") %>%
        addLegend("bottomright", colors = c('#0C0C26'), 
                  labels = c('Path Followed'),
                  title = "No optimization",
                  opacity = 1)
      
    }
      
  })
})

