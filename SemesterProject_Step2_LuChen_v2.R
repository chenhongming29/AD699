##Step II: Predition (Lu Chen)
QueenAnne <- read.csv('QueenAnne.CSV')
str(QueenAnne)

#(Logi to Factor)
library(dplyr)
col.names = colnames(QueenAnne)
num.names = QueenAnne %>% select_if(is.numeric) %>% colnames()
col.names = col.names[!(col.names %in% num.names)]
col.names = c(col.names, "id", "host_id")

QueenAnne.factor = QueenAnne
QueenAnne.factor[,col.names] = data.frame(sapply(QueenAnne.factor[,col.names], as.factor))
str(QueenAnne.factor)

#(Remove space in colname)
colnames(QueenAnne.factor) = gsub('\\s+','',colnames(QueenAnne.factor))

#(Partition)
QueenAnne.df <- QueenAnne.factor

set.seed(125)
train.index <- sample(row.names(QueenAnne.df), 0.6*dim(QueenAnne.df)[1])  
valid.index <- setdiff(row.names(QueenAnne.df), train.index)  
train <- QueenAnne.df[train.index, ]
valid <- QueenAnne.df[valid.index, ]

#(Multicollinearity Check)
library(dplyr)
library(GGally)
library(MASS)
str(train)
testIndep <- dplyr::select(train,
                           accommodates,
                           bathrooms,
                           bedrooms,
                           beds,
                           security_deposit,
                           cleaning_fee,
                           guests_included,
                           extra_people,
                           minimum_nights,
                           maximum_nights,
                           review_scores_rating,
                           calculated_host_listings_count,
                           reviews_per_month
                           )##Need to discuss which one is dropped
ggpairs(testIndep)

#(Drop Variables)
#I only dropped acoomodates, but we can discuss about it.
keptIndep <- dplyr::select(train,
                           bathrooms,
                           bedrooms,
                           beds,
                           security_deposit,
                           cleaning_fee,
                           guests_included,
                           extra_people,
                           minimum_nights,
                           maximum_nights,
                           review_scores_rating,
                           calculated_host_listings_count,
                           reviews_per_month
                           )##!!!!!
ggpairs(keptIndep)

#(Initial Regression)
#Factor variables with only 1 level is removed.
#Factor variables with tooooo many levels (host_id) is removed
options(scipen = 999)

train.lm.origin <- lm(price
                      ~bathrooms##Numeriacal Variables below
                      +bedrooms
                      +beds
                      +security_deposit
                      +cleaning_fee
                      +guests_included
                      +extra_people
                      +minimum_nights
                      +maximum_nights
                      +review_scores_rating
                      +calculated_host_listings_count
                      +reviews_per_month
                      +host_is_superhost##Factor Variables below
                      +is_location_exact
                      +property_type
                      +room_type
                      +bed_type
                      +instant_bookable
                      +cancellation_policy
                      +require_guest_profile_picture
                      +require_guest_phone_verification
                      +TV
                      +Cable.TV
                      +Internet
                      +Wireless.Internet
                      +Air.Conditioning
                      +Kitchen
                      +Heating
                      +Family.Kid.Friendly
                      +Washer
                      +Dryer
                      +Free.Parking.on.Premises
                      +Buzzer.Wireless.Intercom
                      +Smoke.Detector
                      +Carbon.Monoxide.Detector
                      +First.Aid.Kit
                      +Safety.Card,
                      data=train)##!!!!!
summary(train.lm.origin)

#(Backward Elimination)
train.lm.step <- step(train.lm.origin, direction = 'backward')
summary(train.lm.step)

#(Final Model)
#Four of the variables are deleted from the backward result:
  #1. guests_included
  #2. host_is_superhostTRUE
  #3. require_guest_profile_pictureTRUE
  #4. Family.KidFriendlyTRUE
  #5. DryerTRUE

train.lm.final <- lm(price
                     ~bathrooms
                     +bedrooms
                     +cleaning_fee
                     +is_location_exact
                     +room_type
                     +instant_bookable
                     +cancellation_policy
                     +Air.Conditioning, 
                     data=train)##!!!!!
summary(train.lm.final)

#(Accuracy)
library(forecast)
train.lm.pred <- predict(train.lm.final, train)
valid.lm.pred <- predict(train.lm.final, valid)

accuracy(train.lm.pred, train$price)
accuracy(valid.lm.pred, valid$price)

