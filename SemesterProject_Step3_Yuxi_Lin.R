seattle <- read.csv('seattle.csv')
View(seattle)

library()
seattle <- read_csv('seattle.csv')

QueenAnne.3 <- read_csv('QueenAnne.csv')
View(QueenAnne.3)

#k-nn
QA <- QueenAnne.3[,-c(1,2,14)]
QA <- QA[-c(3,20)]


# Knn
library(stringr)
# Transform date value to numerical value
QA <- QA %>%
  mutate(year=as.numeric(str_sub(QA$host_since,start=1,end=4))
         ,mon=as.numeric(str_sub(QA$host_since,start=6,end=7)),
         host_since=year*12+mon-2009*12)

# Need dummy variable(Need original data frame to be modified)
dmy <- dummyVars("~host_is_superhost",QA,fullRank=TRUE)
trqa <- data.frame(predict(dmy,newdata = QA))
QA <- cbind(QA,trqa)

# partition 
set.seed(699)
train.index <- sample(row.names(QA),dim(QA)[1]*0.6) 
valid.index<-setdiff(row.names(QA),train.index)
train.knn <- QA[train.index, ] 
valid.knn <- QA[valid.index, ]
# Knn: new data.frame
new.data <- data.frame()
# Knn: normalization ( need insert column numbers!!!!!!!!!)
norm.values <- preProcess(train.knn[,], method =c("center","scale"))
train.knn[,] <- predict(norm.values, train.knn[,])
valid.knn[,] <- predict(norm.values, valid.knn[,]) 
new.norm <- predict(norm.values, new.data)
# Determine the best k value
295^0.5 ~ 17

accuracy.df <- data.frame(k = seq(1, 17, 1), accuracy = rep(0, 17)) 
for(i in 1:17) {
  knn.pred <- knn(train.knn[, ], valid.knn[, ], cl = train.knn[, 1], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid.knn[, 1])$overall[1] 
}
accuracy.df
accuracy.df %>% filter(accuracy == max(accuracy))
# rerun the knn 
nn <-knn(train = train.knn[,], test = new.norm, cl = train.knn[, 1], k = 1)# the best k value
row.names(train.knn)[attr(nn, "nn.index")] 
nn




# Naive Bayes
library(dplyr)
library(e1071)
library(caret)
set.seed(699)
QA$instant_bookable<-as.factor(QA$instant_bookable)
QAsample<-sample_n(QA,dim(QA)[1])
train <- slice(QAsample,1:dim(QA)[1]*0.6)
valid <- slice(QAsample,dim(QA)[1]*0.6+1:dim(QA)[1])
# Build the model
QA.nb <- naiveBayes(train$instant_bookable ~. , data = train)
# Performance against the trainning set
pre.nb <- predict(QA.nb,newdata = train)
confusionMatrix(pre.nb,train$instant_bookable)
# Performance against the validation set
pre2.nb<- predict(QA.nb,newdata= valid)
confusionMatrix(pre2.nb,valid$instant_bookable)
# Predict on the new fictional
fictional <- data.frame()#!!!!!!!!Need data
predict(air.nb,newdata = fictional)






