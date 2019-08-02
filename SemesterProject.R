install.packages("naniar")
install.packages("ISLR")
install.packages("forecast")
library(MASS)
library(ISLR)
library(dplyr)
library(GGally)
library(ggplot2)
library(forecast)
library(readr)
library(naniar)
library(GGally)


#Read file
Seattle <- read_csv("seattle.csv")
colnames(Seattle)
View(Seattle)
#Select predictors (These are my personal judgements about which of the colnames can be our predictors. 
#Feel free to edit!)

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
                                           "reviews_per_month"))

QueenAnne <- Seattle[,selectednew]
#dim(QueenAnne)

#Filter data with assigned neighbourhood "Queen Anne"
#colnames(QueenAnne)
#str(QueenAnne)
QueenAnne = dplyr::filter(QueenAnne, neighbourhood_group_cleansed == "Queen Anne")

#Check NA nums
count = 1
while (count <= dim(QueenAnne)[1]){
  num = sum(is.na(QueenAnne[count]))
  cat("\ncolnum_name: ",colnames(QueenAnne)[count],"\nNA: ",num)
  count = count + 1
}

#Headling Character Information
QueenAnne$security_deposit = replace(
  QueenAnne$security_deposit,
  is.na(QueenAnne$security_deposit),
  "$0"
)
QueenAnne$cleaning_fee = replace(
  QueenAnne$cleaning_fee,
  is.na(QueenAnne$cleaning_fee),
  "$0"
)
QueenAnne$reviews_per_month = replace(
  QueenAnne$reviews_per_month,
  is.na(QueenAnne$reviews_per_month),
  "$0"
)
QueenAnne$security_deposit[1]

#Dealing with NA
na_strings = c("NA","N A",""," ","  ","N/A","N / A")
QueenAnne = QueenAnne %>%
  replace_with_na_all(condition = ~.x %in% na_strings)
QueenAnne = na.omit(QueenAnne)

#Summary Statistics
summary(QueenAnne)
QueenAnne$id = as.factor(QueenAnne$id)
QueenAnne$host_id = as.factor(QueenAnne$host_id)
QueenAnne$host_since = as.factor(QueenAnne$host_since)
QueenAnne$host_response_time = as.factor(QueenAnne$host_response_time)
QueenAnne$host_response_rate = as.factor(QueenAnne$host_response_rate)
QueenAnne$host_is_superhost = as.factor(QueenAnne$host_is_superhost)
QueenAnne$neighbourhood_group_cleansed = as.factor(QueenAnne$neighbourhood_group_cleansed)
QueenAnne$is_location_exact = as.factor(QueenAnne$is_location_exact)
QueenAnne$property_type = as.factor(QueenAnne$property_type)
QueenAnne$room_type = as.factor(QueenAnne$room_type)


selected = dplyr::select(QueenAnne, host_response_time, host_is_superhost, neighbourhood_group_cleansed, is_location_exact, property_type, room_type)
ggpairs(selected)


#Visualization




