##Clustering
QueenAnne.clu <- read.csv('QueenAnne.factor.clu.csv')
any_na(QueenAnne.clu)

#Transfer as factor
str(QueenAnne.clu)
col.names = colnames(QueenAnne.clu)
num.names = QueenAnne.clu %>% select_if(is.numeric) %>% colnames()
col.names = col.names[!(col.names %in% num.names)]
col.names = c(col.names, "id", "host_id")

QueenAnne.factor = QueenAnne.clu
QueenAnne.factor[,col.names] = data.frame(sapply(QueenAnne.factor[,col.names], as.factor))
str(QueenAnne.factor)
# convert to dummy variable
library(caret)
dummy <- dummyVars('~host_is_superhost
                   +is_location_exact
                   +property_type
                   +room_type
                   +bed_type
                   +requires_license
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
                   +Family/Kid.Friendly
                   +Washer
                   +Dryer
                   +Free.Parking.on.Premises
                   +Buzzer/Wireless.Intercom
                   +Smoke.Detector
                   +Carbon.Monoxide.Detector
                   +First.Aid.Kit
                   +Safety.Card
                   +Fire.Extinguisher
                   +Essentials
                   +Pets.Allowed
                   +Pets.live.on.this.property
                   +Dog(s)
                   +Cat(s)
                   +Hot.Tub
                   +Indoor.Fireplace
                   +Shampoo
                   +Breakfast
                   +24-Hour.Check.in
                   +Hangers
                   +Hair.Dryer
                   +Iron
                   +Laptop.Friendly.Workspace
                   +Suitable.for.Events
                   +Elevator.in.Building
                   +Lock.on.Bedroom.Door
                   +Wheelchair.Accessible
                   +Gym
                   +Pool
                   +Doorman
                   +Other.pet(s)
                   ', data=QueenAnne.clu, fullRank = TRUE) 

trsf <- data.frame(predict(dummy, newdata = QueenAnne.clu))
QueenAnne.dmy <- cbind(QueenAnne.clu,trsf)
# convert to numeric
QueenAnne.dmy$cancellation_policy<- as.numeric(QueenAnne.dmy$cancellation_policy) # 1 as flexiable, 2 as med, 3 as strict
QueenAnne.dmy$room_type<- as.numeric(QueenAnne.dmy$room_type) # 1 as entire home/apt, 2 as private room, 3 as shared room

str(QueenAnne.dmy)
#select varialbes for clustering
QueenAnne.dmy1 <- QueenAnne.dmy[,c('reviews_per_month','security_deposit','host_is_superhost','price','review_scores_rating','instant_bookable','accommodates',
                                   'Breakfast','Gym','Kitchen','cleaning_fee')]


row.names(QueenAnne.dmy1) <- QueenAnne.clu[,1] 
#nomolization
QAcluster.norm <- sapply(QueenAnne.dmy1, scale)

####feature engineering, combine cleaning fee, security deposit and price to total.price
#total.price <-QAcluster.norm[,2] +QAcluster.norm[,4] +QAcluster.norm[,11] 
#QAcluster.norm <- cbind(QAcluster.norm, total.price)
#QAcluster.norm <- QAcluster.norm[, -c(2,4,11)]

#elbow chart
k.max <- 15
data <- QAcluster.norm
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart = 50, iter.max = 15)$tot.withinss})
wss
plot(1:k.max, wss, 
     type = 'b', pch =19, frame = FALSE,
     xlab = 'numbers of clusters K',
     ylab = 'total within-cluster sum of squares')


#kmeans
set.seed(125)
clusters <- kmeans(QAcluster.norm,9)
clusters$cluster
clusters$centers

QueenAnne.dmy1 <- cbind(QueenAnne.dmy1, clusters$cluster)

colnames(QueenAnne.dmy1)[colnames(QueenAnne.dmy1) == 'clusters$cluster'] <- 'cluster'

class(QueenAnne.dmy1)

QueenAnne.dmy1$price<-as.numeric(gsub("\\$","",as.character(QueenAnne.dmy1$price)))
QueenAnne.dmy1$cluster<-as.character(QueenAnne.dmy1$cluster)

#sepreate the clusters and check summary
clu1 <- filter(QueenAnne.dmy1, cluster == '1')  
clu2 <- filter(QueenAnne.dmy1, cluster == '2')
clu3 <- filter(QueenAnne.dmy1, cluster == '3')
clu4 <- filter(QueenAnne.dmy1, cluster == '4')
clu5 <- filter(QueenAnne.dmy1, cluster == '5')
clu6 <- filter(QueenAnne.dmy1, cluster == '6')
clu7 <- filter(QueenAnne.dmy1, cluster == '7')
clu8 <- filter(QueenAnne.dmy1, cluster == '8')
clu9 <- filter(QueenAnne.dmy1, cluster == '9')

summary(clu1)
summary(clu2)
summary(clu3)
summary(clu4)
summary(clu5)
summary(clu6)
summary(clu7)
summary(clu8)
summary(clu9)
#name the clusters
QueenAnne.dmy1$cluster[QueenAnne.dmy1$cluster== "1"]<- "morning people"
QueenAnne.dmy1$cluster[QueenAnne.dmy1$cluster== "2"]<- "family choice"
QueenAnne.dmy1$cluster[QueenAnne.dmy1$cluster== "3"]<- "party choice"
QueenAnne.dmy1$cluster[QueenAnne.dmy1$cluster== "4"]<- "gym people"
QueenAnne.dmy1$cluster[QueenAnne.dmy1$cluster== "5"]<- "popular choice"
QueenAnne.dmy1$cluster[QueenAnne.dmy1$cluster== "6"]<- "cheapest choice"
QueenAnne.dmy1$cluster[QueenAnne.dmy1$cluster== "7"]<- "average choice"
QueenAnne.dmy1$cluster[QueenAnne.dmy1$cluster== "8"]<- "think twice"
QueenAnne.dmy1$cluster[QueenAnne.dmy1$cluster== "9"]<- "luxury choice"
