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
library(scales)


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
colnames(QueenAnne.factor) = gsub('/','',colnames(QueenAnne.factor))


#Export QueenAnne
write.csv(QueenAnne, "~/Desktop/workplace/QueenAnne.csv",row.names = FALSE)
write.csv(QueenAnne.factor, "~/Desktop/workplace/QueenAnne.factor.csv",row.names = FALSE)




#Summary Statistics
#Overall Price Range
summary(QueenAnne.factor$price)

cat("Queen Anne rental price range: from $", range(QueenAnne.factor$price)[1], " to $", range(QueenAnne.factor$price)[2])

#Price range for each property type
QueenAnne.Apartment = filter(QueenAnne.factor, Apartment == 1)
QueenAnne.House = filter(QueenAnne.factor, House == 1)
QueenAnne.Cabin = filter(QueenAnne.factor, Cabin == 1)
QueenAnne.Condominium = filter(QueenAnne.factor, Condominium == 1)
QueenAnne.Camper = filter(QueenAnne.factor, CamperRV == 1)
QueenAnne.Bungalow = filter(QueenAnne.factor, Bungalow == 1)
QueenAnne.Townhouse = filter(QueenAnne.factor, Townhouse == 1)
QueenAnne.Boat = filter(QueenAnne.factor, Boat == 1)
QueenAnne.Loft = filter(QueenAnne.factor, Loft == 1)

#Five nums of each property type
apartment = summary(QueenAnne.Apartment$price)
house = summary(QueenAnne.House$price)
cabin = summary(QueenAnne.Cabin$price)
condominium = summary(QueenAnne.Condominium$price)
camper = summary(QueenAnne.Camper$price)
bungalow = summary(QueenAnne.Bungalow$price)
townhouse = summary(QueenAnne.Townhouse$price)
boat = summary(QueenAnne.Boat$price)
loft = summary(QueenAnne.Loft$price)

property.df = data.frame("Apartment" = c(apartment[1:5]),
                        "House" = c(house[1:5]),
                        "cabin"= c(cabin[1:5]),
                        "condominium" = c(condominium[1:5]),
                        "camper" = c(camper[1:5]),
                        "bungalow" = c(bungalow[1:5]),
                        "townhouse" = c(townhouse[1:5]),
                        "boat"= c(boat[1:5]),
                        "loft" = c(loft[1:5]))


#Distribution of accommodates of different room type in QueenAnne
a.mean = round(mean(QueenAnne.Apartment$accommodates),2)
h.mean = round(mean(QueenAnne.House$accommodates), 2)
c.mean = mean(QueenAnne.Cabin$accommodates)
con.mean = mean(QueenAnne.Condominium$accommodates)
cam.mean = mean(QueenAnne.Camper$accommodates)
bun.mean = mean(QueenAnne.Bungalow$accommodates)
tow.mean = mean(QueenAnne.Townhouse$accommodates)
boa.mean = mean(QueenAnne.Boat$accommodates)
lof.mean = mean(QueenAnne.Loft$accommodates)


#Distribution of review Scores for each type
a.re = round(mean(QueenAnne.Apartment$review_scores_rating),2)
h.re = round(mean(QueenAnne.House$review_scores_rating),2)
c.re = round(mean(QueenAnne.Cabin$review_scores_rating),2)
con.re = round(mean(QueenAnne.Condominium$review_scores_rating),2)
cam.re = round(mean(QueenAnne.Camper$review_scores_rating),2)
bun.re = round(mean(QueenAnne.Bungalow$review_scores_rating),2)
tow.re = round(mean(QueenAnne.Townhouse$review_scores_rating),2)
boa.re = round(mean(QueenAnne.Boat$review_scores_rating),2)
lof.re = round(mean(QueenAnne.Loft$review_scores_rating),2)

#create data frame for property types
accommodates.df = data.frame("PropertyType" = c("Apartment",
                                                "House",
                                                "Cabin",
                                                "Condominium",
                                                "Camper",
                                                "Bungalow",
                                                "Townhouse",
                                                "Boat",
                                                "Loft"),
                             "Accommodates" = c(a.mean, h.mean, c.mean, con.mean, cam.mean, bun.mean, tow.mean, boa.mean, lof.mean),
                             "Review" = c(a.re, h.re,c.re,con.re,cam.re,bun.re,tow.re,boa.re,lof.re))
accommodates.df$Accommodates = as.factor(accommodates.df$Accommodates)

#Accommodates Plot
ggplot(accommodates.df, aes(x=Accommodates, y=PropertyType, label=Accommodates)) + 
  geom_point(stat='identity', aes(col=PropertyType), size=6)  +
  scale_color_manual(name="Accomodates", 
                     labels = c("Apartment", "Boat","Bungalow","Cabin","Camper","Condominium","House","Loft","Townhouse"), 
                     values = c("#CC0000", "#006600", "#669999", "#00CCCC", 
                                "#660099", "#CC0066", "#FF9999", "#FF9900","Black")) + 
  geom_text(color="white", size=2) +
  labs(title="Accommodates Plot for each Property Type") +
  coord_flip()

#Review Plot
ggplot(accommodates.df, aes(x=Review, y=PropertyType, label=Review)) + 
  geom_point(stat='identity', aes(col=PropertyType), size=6)  +
  scale_color_manual(name="Accomodates", 
                     labels = c("Apartment", "Boat","Bungalow","Cabin","Camper","Condominium","House","Loft","Townhouse"), 
                     values = c("#CC0000", "#006600", "#669999", "#00CCCC", 
                                "#660099", "#CC0066", "#FF9999", "#FF9900","Black")) + 
  geom_text(color="white", size=2) +
  labs(title="Review Plot for each Property Type") +
  coord_flip()

#For each property type, what is the percentage that the rooms are pet allowed?
QueenAnne.Apartment.pet = filter(QueenAnne.Apartment, PetsAllowed == 1)
QueenAnne.House.pet = filter(QueenAnne.House, PetsAllowed == 1)
QueenAnne.Cabin.pet = filter(QueenAnne.Cabin, PetsAllowed == 1)
QueenAnne.Condominium.pet = filter(QueenAnne.Condominium, PetsAllowed == 1)
QueenAnne.Camper.pet = filter(QueenAnne.Camper, PetsAllowed == 1)
QueenAnne.Bungalow.pet = filter(QueenAnne.Bungalow, PetsAllowed == 1)
QueenAnne.Townhouse.pet = filter(QueenAnne.Townhouse, PetsAllowed == 1)
QueenAnne.Boat.pet = filter(QueenAnne.Boat, PetsAllowed == 1)
QueenAnne.Loft.pet = filter(QueenAnne.Loft, PetsAllowed == 1)

cat("For Apartment:\n the percentage of a random room being pet allowed is: ",
    percent(dim(QueenAnne.Apartment.pet)[1]/dim(QueenAnne.Apartment)[1]),
    "\nFor House:\n the percentage of a random room being pet allowed is: ",
    percent(dim(QueenAnne.House.pet)[1]/dim(QueenAnne.House)[1]),
    "\nFor Cabin:\n the percentage of a random room being pet allowed is: ",
    percent(dim(QueenAnne.Cabin.pet)[1]/dim(QueenAnne.Cabin)[1]),
    "\nFor Condominium:\n the percentage of a random room being pet allowed is: ",
    percent(dim(QueenAnne.Condominium.pet)[1]/dim(QueenAnne.Condominium)[1]),
    "\nFor Camper:\n the percentage of a random room being pet allowed is: ",
    percent(dim(QueenAnne.Camper.pet)[1]/dim(QueenAnne.Camper)[1]),
    "\nFor Bungalow:\n the percentage of a random room being pet allowed is: ",
    percent(dim(QueenAnne.Bungalow.pet)[1]/dim(QueenAnne.Bungalow)[1]),
    "\nFor Townhouse:\n the percentage of a random room being pet allowed is: ",
    percent(dim(QueenAnne.Townhouse.pet)[1]/dim(QueenAnne.Townhouse)[1]),
    "\nFor Boat:\n the percentage of a random room being pet allowed is: ",
    percent(dim(QueenAnne.Boat.pet)[1]/dim(QueenAnne.Boat)[1]),
    "\nFor Loft:\n the percentage of a random room being pet allowed is: ",
    percent(dim(QueenAnne.Loft.pet)[1]/dim(QueenAnne.Loft)[1]))



#Visualization
boxplot(QueenAnne.factor$price, xlab = "Queen Anne", ylab = "Price")



