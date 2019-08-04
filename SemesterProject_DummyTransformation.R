#(Dummy Variables)#(Lu Chen)
#All factor variables are included below
#Not needed in regression 
#Need to add column numbers to delete at the bottom
library(caret)
dummy <- dummyVars('~host_id
                   +host_is_superhost
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
                   +Cable TV
                   +Internet
                   +Wireless Internet
                   +Air Conditioning
                   +Kitchen
                   +Heating
                   +Family/Kid Friendly
                   +Washer
                   +Dryer
                   +Free Parking on Premises
                   +Buzzer/Wireless Intercom
                   +Smoke Detector
                   +Carbon Monoxide Detector
                   +First Aid Kit
                   +Safety Card
                   +Fire Extinguisher
                   +Essentials
                   +Pets Allowed
                   +Pets live on this property
                   +Dog(s)
                   +Cat(s)
                   +Hot Tub
                   +Indoor Fireplace
                   +Shampoo
                   +Breakfast
                   +24-Hour Check-in
                   +Hangers
                   +Hair Dryer
                   +Iron
                   +Laptop Friendly Workspace
                   +Suitable for Events
                   +Elevator in Building
                   +Lock on Bedroom Door
                   +Wheelchair Accessible
                   +Gym
                   +Pool
                   +Doorman
                   +Other pet(s)
                   ', data=QueenAnne, fullRank = TRUE) 

trsf <- data.frame(predict(dummy, newdata = QueenAnne))

QueenAnne.dmy <- cbind(QueenAnne,trsf)
QueenAnne.dmy <- QueenAnne.dmy[,-c(1,2)] ##!!!!!