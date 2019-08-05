########## Visualization
Seattle <- read_csv("seattle.csv")
QueenAnne.tg = dplyr::filter(Seattle, neighbourhood_group_cleansed == "Queen Anne")

#install.packages("ggmap")
library(ggmap)
library(ggplot2)
qa.room.map<- qmplot(longitude, latitude,data = QueenAnne.tg,extent = "panel",colour =room_type, size = I(2),
       xlab = "Longitude", ylab = "Latitude", size = 2) +ggtitle( 'Map of Room Type')
qa.room.map

qa.property.map<- qmplot(longitude, latitude,data = QueenAnne.tg,extent = "panel",colour =property_type, size = I(2),
                     xlab = "Longitude", ylab = "Latitude", size = 2) +ggtitle( 'Map of Property Type')
qa.property.map
