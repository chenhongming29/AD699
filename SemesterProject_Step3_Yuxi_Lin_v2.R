# Step III

library(caret)
library(lattice)
library(dplyr)
library(FNN)

seattle <- read.csv('seattle.csv')
View(seattle)

QueenAnne <- read.csv('QueenAnne.csv')
QueenAnne.factor <- read.csv('QueenAnne.factor.csv')


# Part I: k-nn
QA <- QueenAnne.factor[,-c(1:6,9,13:18,21:74)]
QA <- QA[c(1:6,8:13,7)]


str(QA)
QA$cancellation_policy <- as.factor(QA$cancellation_policy)


# partition 
set.seed(699)
train.index <- sample(row.names(QA),dim(QA)[1]*0.6) 
valid.index<-setdiff(row.names(QA),train.index)
train.knn <- QA[train.index, ] 
valid.knn <- QA[valid.index, ]

# Knn: new data.frame
knn.new.data <- data.frame(bathrooms=1, bedrooms=3, price=130, security_deposit=200, cleaning_fee=0, instant_bookable=0,
                           Privateroom=1, Sharedroom=0, RealBed=1, Futon=0, "Pull-outSofa"=0, Couch=0)
names(knn.new.data)[11]<-"Pull-outSofa"
str(knn.new.data)

# Knn: normalization
norm.values <- preProcess(train.knn[,1:12], method =c("center","scale"))
train.knn[,1:12] <- predict(norm.values, train.knn[,1:12])
valid.knn[,1:12] <- predict(norm.values, valid.knn[,1:12])
new.norm <- predict(norm.values, knn.new.data)

295^0.5 ~ 17
accuracy.df <- data.frame(k = seq(1, 17, 1), accuracy = rep(0, 17))
for(i in 1:17) {
  knn.pred <- knn(train.knn[,1:12], valid.knn[,1:12], cl = train.knn[,13], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid.knn[, 13])$overall[1] 
}
accuracy.df
accuracy.df %>% filter(accuracy == max(accuracy))

# rerun the knn 
nn <-knn(train = train.knn[,1:12], test = new.norm, cl = train.knn[, 13], k = 14)
row.names(train.knn)[attr(nn, "nn.index")] 
nn




# Part II: Naive Bayes
library(dplyr)
library(e1071)
library(caret)



QA <- QueenAnne.factor[,-c(1:6,9,13:18,21:74)]
QA <- QA[c(1:6,8:13,7)]

set.seed(699)
QA$instant_bookable<-as.factor(QA$instant_bookable)
QAsample<-sample_n(QA, dim(QA)[1])
train <- slice(QAsample, 1:dim(QA)[1]*0.6)
valid <- slice(QAsample, dim(QA)[1]*0.6+1:dim(QA)[1])

# Build the model
QA.nb <- naiveBayes(train$instant_bookable~., data = train)

# Performance against the trainning set
pre.nb <- predict(QA.nb, newdata = train)
confusionMatrix(pre.nb, train$instant_bookable)

# Performance against the validation set
pre2.nb<- predict(QA.nb, newdata= valid)
confusionMatrix(pre2.nb, valid$instant_bookable)

# Predict on the new fictional
fictional <- data.frame(price=130, security_deposit=200, cleaning_fee=0,
                        Privateroom=1, Sharedroom=0, RealBed=1, Futon=0, "Pull-outSofa"=0, Couch=0, cancellation_policy=1)
names(knn.new.data)[11]<-"Pull-outSofa"

predict(QA.nb, newdata = fictional)


