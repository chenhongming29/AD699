##Step II: Predition (Lu Chen)
QueenAnne <- read.csv('QueenAnne.CSV')
str(QueenAnne)

#(Logi to Factor)
library(dplyr)
col.names = colnames(QueenAnne)
num.names = QueenAnne %>% select_if(is.numeric) %>% colnames()
col.names = col.names[!(col.names %in% num.names)]
col.names = c(col.names, "id", "host_id")

QueenAnne.reg = QueenAnne
QueenAnne.reg[,col.names] = data.frame(sapply(QueenAnne.reg[,col.names], as.factor))

#(Remove space in colname)
colnames(QueenAnne.reg) = gsub('\\s+','',colnames(QueenAnne.reg))

#(Partition)
QueenAnne.df <- QueenAnne.reg

set.seed(125)
train.index <- sample(row.names(QueenAnne.df), 0.6*dim(QueenAnne.df)[1])  
valid.index <- setdiff(row.names(QueenAnne.df), train.index)  
train <- QueenAnne.df[train.index, ]
valid <- QueenAnne.df[valid.index, ]

#(Multicollinearity Check)
library(dplyr)
library(GGally)
library(MASS)
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
                           )
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
                      data=train)
summary(train.lm.origin)

#(Backward Elimination)
train.lm.step <- step(train.lm.origin, direction = 'backward')
summary(train.lm.step)

#(DontWannaBeleive Model)
#Four of the variables are deleted from the backward result:
  #1. guests_included
  #2. host_is_superhostTRUE
  #3. require_guest_profile_pictureTRUE
  #4. Family.KidFriendlyTRUE
  #5. DryerTRUE
  #6. is_location_exact
  #7. cancellation_policy
#Add some Variables:
  #1. review_scores_rating
train.lm.question <- lm(price
                     ~bathrooms
                     +bedrooms
                     +review_scores_rating
                     +cleaning_fee
                     +room_type
                     +instant_bookable
                     +Air.Conditioning, 
                     data=train)
summary(train.lm.question)

#(Final Model)
#Four of the variables are deleted from the backward result:
  #1. guests_included
  #2. host_is_superhostTRUE
  #3. require_guest_profile_pictureTRUE
  #4. Family.KidFriendlyTRUE
  #5. DryerTRUE
  #6. is_location_exact
  #7. cancellation_policy
#The Reviews really does not matter.
train.lm.final <- lm(price
                     ~bathrooms
                     +bedrooms
                     +cleaning_fee
                     +room_type
                     +instant_bookable
                     +Air.Conditioning, 
                     data=train)
summary(train.lm.final)

#(Accuracy)
library(forecast)
train.lm.pred <- predict(train.lm.final, train)
valid.lm.pred <- predict(train.lm.final, valid)

accuracy(train.lm.pred, train$price)
accuracy(valid.lm.pred, valid$price)

