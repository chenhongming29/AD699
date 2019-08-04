QueenAnne <- read.csv('QueenAnne.factor.csv')
any_na(QueenAnne)

#Transfer as factor
str(QueenAnne)
col.names = colnames(QueenAnne)
num.names = QueenAnne %>% select_if(is.numeric) %>% colnames()
col.names = col.names[!(col.names %in% num.names)]
col.names = c(col.names, "id", "host_id")

QueenAnne.factor = QueenAnne
QueenAnne.factor[,col.names] = data.frame(sapply(QueenAnne.factor[,col.names], as.factor))
str(QueenAnne.factor)
# convert to dummy vari
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
                   ', data=QueenAnne, fullRank = TRUE) 

trsf <- data.frame(predict(dummy, newdata = QueenAnne))
QueenAnne.dmy <- cbind(QueenAnne,trsf)

QueenAnne.dmy$cancellation_policy<- as.numeric(QueenAnne.dmy$cancellation_policy) # 1 as flexiable, 2 as med, 3 as strict
QueenAnne.dmy$room_type<- as.numeric(QueenAnne.dmy$room_type) # 1 as entire home/apt, 2 as private room, 3 as shared room

str(QueenAnne.dmy)
#QueenAnne.dmy2 <- QueenAnne.dmy[ ,-c(1,2,3,5,6,7,10:13,15:20,22,23,25:28,67,68)] ## with amentites
#amentities <- sum=colSums(QAcluster.norm[,7:44]) # big problem!!!!!!

QueenAnne.dmy1 <- QueenAnne.dmy[,c('host_is_superhost','price','review_scores_rating','cancellation_policy','instant_bookable','room_type','accommodates')]
                              
row.names(QueenAnne.dmy1) <- QueenAnne[,1] 
#nomolization
QAcluster.norm <- sapply(QueenAnne.dmy1, scale)


#elbow chart
set.seed(111)
k.max <- 15
data <- QAcluster.norm
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart = 50, iter.max = 15)$tot.withinss})
wss
plot(1:k.max, wss, 
     type = 'b', pch =19, frame = FALSE,
     xlab = 'numbers of clusters K',
     ylab = 'total within-cluster sum of squares')


clusters <- kmeans(QAcluster.norm,5)
clusters$cluster
clusters$centers
