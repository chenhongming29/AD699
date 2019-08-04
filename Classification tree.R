library(MASS)
library(ISLR)
library(dplyr)
library(GGally)
library(ggplot2)
library(forecast)
library(readr)
library(naniar)
library(rpart)
library(rpart.plot)
library(dplyr)
library(caret)

#Read csv
QueenAnne <- read_csv("QueenAnne.factor.csv")
colnames(QueenAnne)
str(QueenAnne)
View(QueenAnne)

#Remove $0 cleaning fee
QueenAnneclean <- QueenAnne %>% filter(cleaning_fee != 0)
View(QueenAnneclean)

#Data Partitioning
set.seed(125)
QueenAnnedf1 <- sample_n(QueenAnneclean, 218)
View(QueenAnnedf1)
train <- slice(QueenAnnedf1, 1:130)
valid <- slice(QueenAnnedf1, 131:218)

#Classification tree trial
colnames(QueenAnneclean)
QueenAnnemodel <- rpart(cleaning_fee~property_type+room_type+accommodates+bathrooms+
                          bedrooms+beds+bed_type+price+guests_included+minimum_nights+
                          maximum_nights, data=train, method="class")
rpart.plot(QueenAnnemodel, type = 4, extra = 3, branch.lty = 1, box.palette = "RdYlGn",
           legend.x=NA,tweak=1)

#Optimal complexity parameter
QueenAnnemodel2 <- rpart(cleaning_fee~property_type+room_type+accommodates+bathrooms+
                         bedrooms+beds+bed_type+price+guests_included+minimum_nights+
                         maximum_nights, data=train, method="class", cp=0.00, xval=5)
rpart.plot(QueenAnnemodel2, type = 4, extra = 3, branch.lty = 1, box.palette = "RdYlGn",
           legend.x=NA,tweak = 1)

options(scipen=999)
Optimalcp <- printcp(QueenAnnemodel2)
class(Optimalcp)
Optimalcp <- data.frame(Optimalcp)
which.min(Optimalcp$xerror) ##misleading cp, review needed
plotcp(QueenAnnemodel2)

# New tree model
QueenAnnemodel3 <- rpart(cleaning_fee~property_type+room_type+accommodates+bathrooms+
                         bedrooms+beds+bed_type+price+guests_included+minimum_nights+
                         maximum_nights, data=train, method="class", xval=5, cp=0.018519)
rpart.plot(QueenAnnemodel3, type = 4, extra = 3, branch.lty = 1, box.palette = "RdYlGn",
           legend.x=NA,tweak = 1)

# Confusion matrices
#QueenAnnemodel.pred <- predict(QueenAnnemodel3, train, type="class")
#confusionMatrix(table(QueenAnnemodel.pred, train$cleaning_fee))

#QueenAnnemodel.pred <- predict(QueenAnnemodel3, valid, type="class")
#confusionMatrix(table(QueenAnnemodel.pred, valid$cleaning_fee))

