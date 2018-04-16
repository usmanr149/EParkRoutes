library(leaflet)
library(magrittr)
library(gepaf)
library(geosphere)


df_Path <-read.csv('Path_Dump.csv')

df_Path$date <- as.POSIXct(strptime(df_Path$date, "%Y-%m-%d"))

df_Path <- with(df_Path, subset(df_Path, 
                          date == '2018-03-19' & date_feature == 3 )) 

a <- as.character(df_Path[1, 'path'])

path <- decodePolyline(a)

mydf <- read.csv('Data_Dump.csv')
mydf$created_at <- as.POSIXct(strptime(mydf$created_at, "%Y-%m-%d %H:%M:%S"))
mydf <- mydf[ order(mydf$created_at , decreasing = TRUE ),]


date_test <- with(mydf, subset(mydf, 
                               created_at >= as.POSIXct('2018-03-19 13:25:00') & 
                                 created_at <= as.POSIXct('2018-03-19 15:44:00') &
                                 accuracy <= 150)) 

#mydf$created_at <- as.POSIXct(strptime(mydf$created_at, "%Y-%m-%d %H:%M:%S"))

content <- paste(sep = "",
                 "Time = ", round(as.POSIXct('2018-03-19 13:25:00') - 
                                    as.POSIXct('2018-03-19 15:44:00'), digits = 2),
                 "<br/>606 5th Ave. S",
                 "<br/>Seattle, WA 98138"
)

mydf2 <- data.frame(lat = c(date_test$latitude, date_test$NewLat),
                    long = c(date_test$longitude, date_test$NewLong))

leaflet()%>%
  addTiles() %>%
  addPolylines(data = mydf2, lng = ~long, lat = ~lat, color='#0C0C26', 
               opacity = 1, weight=1) %>% 
  addPolylines(data = path, lng = ~lon, lat = ~lat, color = '#3F7FBF', opacity = 0.8) %>%
  addControl(content, 
             position = "bottomleft") %>%
  addLegend("bottomright", colors = c('#0C0C26', '#3F7FBF'), 
            labels = c('Suggested Path', 'Path Followed'),
            title = "Path",
            opacity = 1)


distance = 0
for (i in 1:(nrow(date_test) - 1)){
  distance <- distance + distm(c(date_test[i,]$longitude, date_test[i,]$latitude), 
                               c(date_test[i+1,]$longitude, date_test[i+1,]$latitude), 
                               fun=distVincentySphere)[1]
}

for (i in range(1:10 - 1)){
  print(i)
}