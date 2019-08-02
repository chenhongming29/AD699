
##Step II: Predition (Lu Chen)
#***I left the independent variables as A+B+C... for us to decide together. ***
#***"!!!!" is tip of requiement of above replacement. ***
#***This script has not been actually run yet. So there might be errors. ***

#(Dummy Variables)
#The dummy variable transformation is done before the partiotion for convenient.
#I can change this order if latter steps fund it bothered.
library(caret)
dummy <- dummyVars('~A+B+...', data=QueenAnne, fullRank = TRUE) ##!!!!!
trsf <- data.frame(predict(dummy, newdata = QueenAnne))

QueenAnne.dmy <- cbind(QueenAnne,trsf)
QueenAnne.dmy <- QueenAnne.dmy[,-c(1,2)] ##!!!!!

#(Partition)
QueenAnne.df <- QueenAnne.dmy

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
testIndep <- dplyr::select(train, A,B,C)##!!!!!
ggpairs(testIndep)

#(Drop Variables)
keptIndep <- dplyr::select(train, A,C)##!!!!!
ggpairs(keptIndep)

#(Backward Elimination)
options(scipen = 999)

train.lm.origin <- lm(price~A+B+C, data=train)##!!!!!
train.lm.step <- step(train.lm, direction = 'backward')
summary(train.lm.step)

#(Final Model)
train.lm.final <- lm(price~A+B+C, data=train)##!!!!!
summary(train.lm.final)

#(Accuracy)
library(forecast)
train.lm.pred <- predict(train.lm.final, train)
valid.lm.pred <- predict(train.lm.final, valid)

accuracy(train.lm.pred, train$price)
accuracy(valid.lm.pred, valid$price)

