library(MASS)
library(ISLR)
library(dplyr)
library(GGally)
library(ggplot2)
library(forecast)
library(readr)
library(naniar)

library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(biclust)
library(cluster)
library(igraph)
library(fpc)

#Read file
Seattle <- read_csv("seattle.csv")
colnames(Seattle)
View(Seattle)

na_strings = c("NA","N A",""," ","  ","N/A","N / A")
Seattle = Seattle %>%
  replace_with_na_all(condition = ~.x %in% na_strings)


selected <- which(colnames(Seattle) %in% c("id",
                                           "transit",
                                           "host_id",
                                           "host_since",
                                           "host_response_time",
                                           "host_response_rate",
                                           "host_is_superhost",
                                           "neighbourhood_group_cleansed",
                                           "is_location_exact",
                                           "property_type",
                                           "room_type",
                                           "accommodates",
                                           "bathrooms",
                                           "bedrooms",
                                           "beds",
                                           "bed_type",
                                           "amenities",
                                           "price",
                                           "security_deposit",  ##Replace NA with 0 on this col
                                           "cleaning_fee",
                                           "guests_included",
                                           "extra_people",
                                           "minimum_nights",
                                           "maximum_nights",
                                           "avaliablity_365",
                                           "number_of_review",
                                           "review_scores_rating",
                                           "review_scores_accuracy",
                                           "review_scores_cleanliness",
                                           "review_scores_checkin",
                                           "review_scores_communication",
                                           "review_scores_location",
                                           "review_scores_value",
                                           "requires_license",
                                           "instant_bookable",
                                           "cancellation_policy",
                                           "require_guest_profile_picture",
                                           "require_guest_phone_verification",
                                           "calculated_host_listings_count",
                                           "reviews_per_month"))

selectednew <- which(colnames(Seattle) %in% c("id",
                                              "host_id",
                                              "host_since",
                                              "host_is_superhost",
                                              "neighbourhood_group_cleansed",
                                              "is_location_exact",
                                              "property_type",
                                              "room_type",
                                              "accommodates",
                                              "bathrooms",
                                              "bedrooms",
                                              "beds",
                                              "bed_type",
                                              "amenities",
                                              "price",
                                              "security_deposit",  ##Replace NA with 0 on this col
                                              "cleaning_fee",      ##Replace NA with 0 on this col
                                              "guests_included",
                                              "extra_people",
                                              "minimum_nights",
                                              "maximum_nights",
                                              "avaliablity_365",
                                              "number_of_review",
                                              "review_scores_rating",
                                              "requires_license",
                                              "instant_bookable",
                                              "cancellation_policy",
                                              "require_guest_profile_picture",
                                              "require_guest_phone_verification",
                                              "calculated_host_listings_count",
                                              "reviews_per_month",
                                              "latitude","longitude"
))

QueenAnne <- Seattle[,selectednew]
#dim(QueenAnne)

#Filter data with assigned neighbourhood "Queen Anne"
#colnames(QueenAnne)
#str(QueenAnne)
QueenAnne = dplyr::filter(QueenAnne, neighbourhood_group_cleansed == "Queen Anne")
View(QueenAnne)

#Pie chart property types
Propertytypes <- as.data.frame(table(QueenAnne$property_type))
colnames(Propertytypes) <- c("class", "freq")
pie <- ggplot(Propertytypes, aes(x = "", y=freq, fill = factor(class))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="class", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of Property types")

pie + coord_polar(theta = "y", start=0)