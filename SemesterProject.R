#install.packages("naniar")
#install.packages("ISLR")
#install.packages("forecast")
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
#View(Seattle)
#Select predictors (These are my personal judgements about which of the colnames can be our predictors. 
#Feel free to edit!)

#Dealing with NA
na_strings = c("NA","N A",""," ","  ","N/A","N / A")
Seattle = Seattle %>%
  replace_with_na_all(condition = ~.x %in% na_strings)

#Select & Filtering
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
QueenAnne = dplyr::filter(QueenAnne, neighbourhood_group_cleansed == "Queen Anne")

#Check NA nums
count = 1
while (count <= dim(QueenAnne)[1]){
  num = sum(is.na(QueenAnne[count]))
  cat("\ncolnum_name: ",colnames(QueenAnne)[count],"\nNA: ",num)
  count = count + 1
}
count = 1

#Headling Character Information
QueenAnne$security_deposit = replace(
  QueenAnne$security_deposit,
  is.na(QueenAnne$security_deposit),
  0.00
)
QueenAnne$cleaning_fee = replace(
  QueenAnne$cleaning_fee,
  is.na(QueenAnne$cleaning_fee),
  0.00
)
QueenAnne$reviews_per_month = replace(
  QueenAnne$reviews_per_month,
  is.na(QueenAnne$reviews_per_month),
  0.00
)

#Predict NA's on review_scores_rating with KNN
QA = na.omit(QueenAnne)

QueenAnne$review_scores_rating = replace(
  QueenAnne$review_scores_rating,
  is.na(QueenAnne$review_scores_rating),
  median(QA$review_scores_rating)
)

#Transfer $xx to numeric
QueenAnne$extra_people<-as.numeric(gsub("\\$","",as.character(QueenAnne$extra_people)))
QueenAnne$cleaning_fee<-as.numeric(gsub("\\$","",as.character(QueenAnne$cleaning_fee)))
QueenAnne$security_deposit<-as.numeric(gsub("\\$","",as.character(QueenAnne$security_deposit)))
QueenAnne$price<-as.numeric(gsub("\\$","",as.character(QueenAnne$price)))

#Transfer Character List Veriable "Amenities" into Dummy
amenityList = c()
for (aData in QueenAnne[,14]){
  string = strsplit(aData,"[,]")
  for (s in string){
    amenityList = c(amenityList,s)
  }
}
amenityList

dummyList = c()
for (anotherData in amenityList){
  dummyList = c(dummyList, gsub("[{}\"]","",anotherData))
}
dummyList = dummyList[!duplicated(dummyList)]
dummyList = dummyList[-c(37)]
dummyList

for (data in QueenAnne[,14]){
  for (dummy in dummyList){
    QueenAnne[dummy] = grepl(dummy, data)
  }
}

QueenAnne = QueenAnne[,-c(14)]

#Change Dummy Variables into 0,1
logical.names = QueenAnne %>% select_if(is.logical) %>% colnames()
QueenAnne[,logical.names] = data.frame(sapply(QueenAnne[,logical.names], as.integer))


#Define function about dummy
change_var_to_dummy = function(x){
  amenityList = c()
  for (aData in QueenAnne[,x]){
    string = strsplit(aData,"[,]")
    for (s in string){
      amenityList = c(amenityList,s)
    }
  }
  amenityList
  
  dummyList = c()
  for (anotherData in amenityList){
    dummyList = c(dummyList, gsub("[{}\"]","",anotherData))
  }
  dummyList = dummyList[!duplicated(dummyList)]
  dummyList
  
  for (data in QueenAnne[,x]){
    for (dummy in dummyList){
      QueenAnne[dummy] = grepl(dummy, data)
    }
  }
  QueenAnne = QueenAnne[,-c(x)]
  return(QueenAnne)
}
#Change Property_type to dummy
QueenAnne = change_var_to_dummy(7)

#Change room_type to Dummy Variables
QueenAnne = change_var_to_dummy(7)

#Change bed_type to Dummy
QueenAnne = change_var_to_dummy(11)

#Change Cancellation Policy to Dummy
amenityList = c()
for (aData in QueenAnne$cancellation_policy){
  string = strsplit(aData,"[,]")
  for (s in string){
    amenityList = c(amenityList,s)
  }
}
amenityList
  
dummyList = c()
for (anotherData in amenityList){
  dummyList = c(dummyList, gsub("[{}\"]","",anotherData))
}
dummyList = dummyList[!duplicated(dummyList)]
dummyList = c("flexible","moderate","strict")
dummyList
  
num = 1
for (data in QueenAnne$cancellation_policy){
  QueenAnne[num,21] = match(data, dummyList)
  num = num + 1
}
QueenAnne$cancellation_policy = as.factor(as.integer(QueenAnne$cancellation_policy))



#Change Dummy Variables into 0,1
logical.names = QueenAnne %>% select_if(is.logical) %>% colnames()
QueenAnne[,logical.names] = data.frame(sapply(QueenAnne[,logical.names], as.integer))

#Get rid of Var Neighbourhood_group_cleansed
QueenAnne = QueenAnne[,-c(5)]

#Headling Character Information
QueenAnne$security_deposit = replace(
  QueenAnne$security_deposit,
  is.na(QueenAnne$security_deposit),
  0.00
)
QueenAnne$cleaning_fee = replace(
  QueenAnne$cleaning_fee,
  is.na(QueenAnne$cleaning_fee),
  0.00
)
QueenAnne$reviews_per_month = replace(
  QueenAnne$reviews_per_month,
  is.na(QueenAnne$reviews_per_month),
  0.00
)


#Transfer as factor
str(QueenAnne)
col.names = colnames(QueenAnne)
num.names = QueenAnne %>% select_if(is.numeric) %>% colnames()
int.names = QueenAnne %>% select_if(is.integer) %>% colnames()
col.names = col.names[!(col.names %in% num.names)]
col.names = c(col.names, int.names,"id", "host_id")

QueenAnne.factor = QueenAnne
QueenAnne.factor[,col.names] = data.frame(sapply(QueenAnne.factor[,col.names], as.factor))
str(QueenAnne.factor)

#Remove space in Column names
colnames(QueenAnne.factor) = gsub('\\s+','',colnames(QueenAnne.factor))

#Export QueenAnne
write.csv(QueenAnne, "~/Desktop/workplace/QueenAnne.csv",row.names = FALSE)
write.csv(QueenAnne.factor, "~/Desktop/workplace/QueenAnne.factor.csv",row.names = FALSE)


#Summary Statistics
summary(QueenAnne)

selected = dplyr::select(QueenAnne, host_response_time, host_is_superhost, neighbourhood_group_cleansed, is_location_exact, property_type, room_type)
ggpairs(selected)


#Visualization




