#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
# Define UI for application that draws a histogram

shinyUI(fluidPage(
  
  titlePanel("EPark COin Pick Up Path Review"),
    
    sidebarPanel(
      selectInput('date', 'Date', c('2018-01-31', '2018-02-01', '2018-02-02',
                                    '2018-02-05', '2018-02-06', '2018-02-07',
                                    '2018-02-08', '2018-02-09', '2018-02-12',
                                    '2018-02-13', 
                                    '2018-03-13', '2018-03-15', 
                                    '2018-03-19', '2018-03-20', '2018-03-21', 
                                    '2018-03-22', '2018-03-23', '2018-03-27', 
                                    '2018-04-09', '2018-04-10'))
    ),
    
      leafletOutput("mymap", height = 800)
  )
)
#    leafletOutput("mymap"),
#    p(),
#     selectInput('date', 'Date', c('2018-03-07', '2018-03-13', '2018-03-15', 
#                                   '2018-03-19', '2018-03-20', '2018-03-21', 
#                                   '2018-03-22', '2018-03-23', '2018-03-27', '2018-04-09'))
# ))
#   actionButton("recalc", "New points")
# ))
