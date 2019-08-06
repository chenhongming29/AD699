########## Visualization
Seattle <- read_csv("seattle.csv")
QueenAnne.tg = dplyr::filter(Seattle, neighbourhood_group_cleansed == "Queen Anne")

#frequency Polygon
qa.host.since.plot <- ggplot(QueenAnne.tg,aes(x=host_since))+geom_freqpoly(binwidth=10)
qa.host.since.plot 
#scatter plot
ggplot(data=QueenAnne.tg, aes(x=review_scores_rating, y=reviews_per_month,color = room_type,shape = room_type)) + geom_point(size = 2) + xlim(c(70,100))+
  ggtitle('The reviews per month vs. review scores')

#install.packages("ggmap")
library(ggmap)
library(ggplot2)
#map plot
qa.room.map<- qmplot(longitude, latitude,data = QueenAnne.tg,extent = "panel",colour =room_type, size = I(2),
       xlab = "Longitude", ylab = "Latitude", size = 2) +ggtitle( 'Map of Room Type')
qa.room.map

qa.property.map<- qmplot(longitude, latitude,data = QueenAnne.tg,extent = "panel",colour =property_type, size = I(2),
                     xlab = "Longitude", ylab = "Latitude", size = 2) +ggtitle( 'Map of Property Type')
qa.property.map

