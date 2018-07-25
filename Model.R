library(car)
library(lme4)
library(lattice)
library(nnet)
library(caret)
library(e1071)
require(reshape2)
#library(ggforce)


#old.par <- par(mar = c(0, 0, 0, 0))
#par(old.par)
#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
#multinomial model
#quantile(bookings.made.v.grouped$Utilisation, probs = c(0, 0.25, 0.35, 0.5, 0.7, 0.8, 0.9, 0.95, 0.98,1))
#c(0.014,  1.004,  1.574,  2.825,  5.890,  8.611, 13.660, 20.000, 27.568, 58.889 )
bookings.made.v.grouped$Booking.Month <- as.factor(bookings.made.v.grouped$Booking.Month)
bookings.made.v.grouped$Utilisation.Category <-  cut(bookings.made.v.grouped$Utilisation, breaks = c(0.013, 0.781,  1.923,  4.1, 8.61, 58.9), 
                                                     include.lowest = TRUE,
                                                     labels= c( "-2","-1", "+0", "+1", "+2"))
bookings.made.v.grouped$Log.Utilisation.Category <-  cut(log(bookings.made.v.grouped$Utilisation), breaks = c(-4.2, -0.246,  0.65,  1.41, 2.15, 4.1), 
                                                     include.lowest = TRUE,
                                                     labels= c( "-2","-1", "+0", "+1", "+2"))

bookings.made.v.grouped$Total.Bookings.Category <-  cut(log(bookings.made.v.grouped$Total.Bookings),
                                                        breaks = c(0, 0.138, 2.19, 2.94, 4, 5.02), 
                                                        include.lowest = TRUE, labels= c( "-2","-1", "+0", "+1", "+2"))
#View(bookings.made.v.grouped[1:10,c("Utilisation", "Utilisation.Category")])


bookings.made.v.grouped$Utilisation.Category  <- relevel(bookings.made.v.grouped$Utilisation.Category , ref = "-2")
bookings.made.v.grouped$Total.Bookings.Category  <- relevel(bookings.made.v.grouped$Total.Bookings.Category , ref = "-2")
bookings.made.v.grouped$State <- factor(bookings.made.v.grouped$State)
bookings.made.v.grouped$Organisation.Type <- factor(bookings.made.v.grouped$Organisation.Type)
bookings.made.v.grouped$Organisation.Status <- factor(bookings.made.v.grouped$Organisation.Status)
bookings.made.v.grouped$Facility.Type <- factor(bookings.made.v.grouped$Facility.Type)
bookings.made.v.grouped$Year.OfBooking <- factor(bookings.made.v.grouped$Year.OfBooking)


#bookings.members.by.court <- members.by.court.final[members.by.court.final$nbr.courts>0 , c("Club.Facility.Name", "Facility.Name", "Club.Name", "MembersByCourt")]
#bookings.members.by.court$Facility.Name <- proper(bookings.members.by.court$Facility.Name)

##bookings.made.v.grouped$Facility.Name <- proper(bookings.made.v.grouped$Facility.Name)

data.model <- bookings.made.v.grouped

bookings.members.by.court.X <- bmc#[!duplicated(bookings.members.by.court$Club.Facility.Name2),]
bookings.members.by.court.2 <- bookings.members.by.court.X
bookings.members.by.court.2$Club.Facility.Name2 <- bookings.members.by.court.2$Facility.Name

bookings.members.by.court.X <- rbind(bookings.members.by.court.X, bookings.members.by.court.2)
bookings.members.by.court.X$Club.Facility.Name2 <- standardise.club.names(bookings.members.by.court.X$Club.Facility.Name2)
bookings.members.by.court.X <-bookings.members.by.court.X[!duplicated(bookings.members.by.court.X$Club.Facility.Name2),]

data.model <- merge(data.model, bookings.members.by.court.X, by.x="Venue.Name", by.y = "Club.Facility.Name2", all.x=T) # 

#data.model.2 <- merge(data.model, bookings.members.by.court, by.x="Club.Facility.Name2", by.y = "Club.Facility.Name2", all.x=T) # 
#summary(is.na(data.model$MembersByCourt))
#####!!!!!!!!!!!!!!!!!!!!!!
#unique(data.model[is.na(data.model$MembersByCourt), c("Venue.Name")])

## <<<< ADD AVAILABILITY AS A FUNCTION OF #COURTS AND MEMBERSBYCOURT >>> 
data.model$AV <- data.model$nbr.courts/data.model$MembersByCourt
#summary(data.model$AV)

members.by.nbr.of.courts <- members.by.court.final[members.by.court.final$nbr.courts>0 , c("nbr.courts", "MembersByCourt")]
members.by.nbr.of.courts<- members.by.nbr.of.courts %>% group_by(nbr.courts) %>% summarise(MembersByCourt = median(MembersByCourt)) 
data.model[is.na(data.model$MembersByCourt), "MembersByCourt"] <- 
 c(members.by.nbr.of.courts[data.model[is.na(data.model$MembersByCourt), "nbr.courts"] , "MembersByCourt"])


members.gender.ratio <- members.gender.ratio[!duplicated(members.gender.ratio$Facility.Name),]
data.model <- merge(data.model, members.gender.ratio[,c("Facility.Name", "male.member.ratio", "members")], by.x="Facility.Name", by.y = "Facility.Name", all.x=T) # 

data.model[is.na(data.model$male.member.ratio), "male.member.ratio"] <- mean(members.gender.ratio$male.member.ratio)
#summary(data.model$male.member.ratio)

members.ages <- members.ages.by.club[!duplicated(members.ages.by.club$Facility.Name),]
data.model <- merge(data.model, members.ages[,c("Facility.Name", "Mean.Age")], by.x="Facility.Name", by.y = "Facility.Name", all.x=T) # 
data.model[is.na(data.model$Mean.Age), "Mean.Age"] <- mean(members.ages$Mean.Age)


data.model$Booking.and.Utilisation <- normBetweenMinMax(data.model$Total.Bookings, 0,50) + 
  normBetweenMinMax(data.model$Utilisation, 0,50)

data.model$Booking.and.Utilisation.Category <- cut(data.model$Booking.and.Utilisation, 
                                                                breaks = quantile(data.model$Booking.and.Utilisation, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)), 
                                                       include.lowest = TRUE,
                                                       labels= c( "-2","-1", "+0", "+1", "+2"))
#View(head(data.model))

data.model$Utilisation.Weighted.By.NbrCourts <- data.model$Utilisation*sqrt(data.model$nbr.courts)

data.model$Utilisation.descripencies <- data.model$Utilisation-data.model$Utilisation.Weighted.By.NbrCourts

data.model[is.na(data.model$SpeaksaLanguageOtherThanEnglishatHome.Proportionoftotalpopulation.Perc), 
           "SpeaksaLanguageOtherThanEnglishatHome.Proportionoftotalpopulation.Perc"] <- 
  mean(data.model$SpeaksaLanguageOtherThanEnglishatHome.Proportionoftotalpopulation.Perc, na.rm=T)
  

data.model$DistanceToCBD.km.truncated <- data.model$DistanceToCBD.km
data.model[data.model$DistanceToCBD.km.truncated > 100, "DistanceToCBD.km.truncated"] <- 100
data.model$DistanceToCBD.km.truncated <- as.numeric(data.model$DistanceToCBD.km.truncated)

# data.model[data.model$RegionLocation== "Inner City", "RegionLocation"] <- "City"
# data.model[data.model$RegionLocation== "City Border", "RegionLocation"] <- "City"
# data.model[data.model$RegionLocation== "Outer Suburb", "RegionLocation"] <- "Suburb"
# data.model$RegionLocation <- factor(data.model$RegionLocation)
#View(data.model[,c("Club.Name", "nbr.courts", "Utilisation", "Utilisation.Weighted.By.NbrCourts", "Utilisation.descripencies")])
#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
set.seed(1987) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(data.model), size = floor(.8*nrow(data.model)), replace = F)
train <- data.model[sample, ]
test  <- data.model[-sample, ]

#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
model <- lm(log(Utilisation)~#log(Utilisation) Utilisation.Weighted.By.NbrCourts
              State + 
              DistanceToCBD.km.truncated +  RegionLocation+
              log(Populationdensity.ERPat30June.persons.km2)+ 
              Totalfamilies.no. +
              #RegionLocation+
              MaleFemalePerc +
              male.member.ratio+
              AV + 
              #(ClubsInSuburb)+
              recently.advertised +
              #(Medianequivalisedtotalhouseholdincome.weekly.AUD) +
              #SpeaksaLanguageOtherThanEnglishatHome.Proportionoftotalpopulation.Perc + 
              (MedianAge.Persons.years)+ 
              Mean.Age +
              (WorkingAgePopulation.aged15.64years)+
              Australiancitizen.Perc+
              SecularBeliefs.Perc+
              Booking.Month + 
              Year.OfBooking+
              Booking.Day +
              Facility.Type +  
              #Organisation.Type +
              #Organisation.Status+
              log(MembersByCourt)+ 
              Total.Opening.Hours*nbr.courts +
              nbr.courts*court.options +
              #has.hard*has.clay*has.grass +
              has.Lights +
              has.hot.shot+ 
              Amenities.options,
            #offset= log(50/nbr.courts),
            data = train)

summary(model)
qqnorm(model$residuals)
qqline(model$residuals)
car::vif(model)
#library(arm)
coefplot(model, mar=c(12,2.5,2,2), vertical=F, offset=0.1, CI=1) + grid(length(coefficients(model)), 3) # mar=c(12,2.5,2,2),

test.potential.venues <- potential.courts[ ,  c("State" , "Populationdensity.ERPat30June.persons.km2", "RegionLocation" , "ClubsInSuburb" , 
                                                "recently.advertised" , "Medianequivalisedtotalhouseholdincome.weekly.AUD" , "MedianAge.Persons.years", 
                                                "WorkingAgePopulation.aged15.64years", "MaleFemalePerc", "Australiancitizen.Perc", "Booking.Month" , 
                                                "Year.OfBooking" , "Booking.Day","Total.Opening.Hours", "Facility.Type" , "Organisation.Type" ,
                                                "nbr.courts",  "Lighted.Full.Count.All","has.Lights", "has.indoor", "has.outdoor", "has.hard", 
                                                "has.clay", "has.grass", "has.hot.shot", "MembersByCourt", "court.options", "Totalfamilies.no.", "SecularBeliefs.Perc",
                                                "Club.Facility.Name", "Club.Name", "Amenities.options", "Suburb", "DistanceToCBD.km")]

#summary(standardise.club.names(test.potential.venues$Club.Facility.Name) %in% standardise.club.names(members.gender.ratio$Club.Facility.Name))
test.potential.venues <- merge(test.potential.venues, members.gender.ratio[,c("Club.Facility.Name", "male.member.ratio")], by.x="Club.Facility.Name", by.y= "Club.Facility.Name", all.x=T)
test.potential.venues[is.na(test.potential.venues$male.member.ratio), "male.member.ratio"] <- round(median(data.model$male.member.ratio))


test.potential.venues[test.potential.venues$Facility.Type %in% c("LGA", "School"), "Facility.Type"] <- "Private"


test.potential.venues <- merge(test.potential.venues, members.ages[,c("Club.Facility.Name2", "Mean.Age")], by.x="Club.Facility.Name", by.y= "Club.Facility.Name2", all.x=T)
test.potential.venues[is.na(test.potential.venues$Mean.Age), "Mean.Age"] <- round(median(data.model$Mean.Age))
test.potential.venues[is.na(test.potential.venues$MembersByCourt), "MembersByCourt"] <- median(test.potential.venues$MembersByCourt, na.rm=T)

test.potential.venues$AV <- test.potential.venues$nbr.courts/test.potential.venues$MembersByCourt
test.potential.venues[is.na(test.potential.venues$Total.Opening.Hours), "Total.Opening.Hours"] <- median(data.model$Total.Opening.Hours, na.rm=T)

dtcbd <- unique(test.potential.venues[,c("State", "Suburb", "RegionLocation")])
dtcbd <- dtcbd[!is.na(dtcbd$RegionLocation),]
dtcbd$Suburb.RegionalLocation <-dtcbd$RegionLocation

test.potential.venues <- merge(test.potential.venues, dtcbd, all.x=T)
#test.potential.venues$RegionLocation[is.na(test.potential.venues$RegionLocation)] <- test.potential.venues$Suburb.RegionalLocation[is.na(test.potential.venues$RegionLocation)]

test.potential.venues[is.na(test.potential.venues$RegionLocation) & test.potential.venues$Suburb== "Burpengary", c("RegionLocation", "DistanceToCBD.km")] <- c("Suburb", 30)
test.potential.venues[is.na(test.potential.venues$RegionLocation) & test.potential.venues$Suburb== "Nerang", c("RegionLocation", "DistanceToCBD.km")] <- c("Outer Suburb", 30)
test.potential.venues[is.na(test.potential.venues$RegionLocation) & test.potential.venues$Suburb== "North Lakes", c("RegionLocation", "DistanceToCBD.km")] <- c("City Border", 20)
test.potential.venues[is.na(test.potential.venues$RegionLocation) & test.potential.venues$Suburb== "St Lucia", c("RegionLocation", "DistanceToCBD.km")] <- c("City", 10)
test.potential.venues[is.na(test.potential.venues$RegionLocation) & test.potential.venues$Suburb== "Mitchell Park", c("RegionLocation", "DistanceToCBD.km")] <- c("City", 10)
test.potential.venues[is.na(test.potential.venues$RegionLocation) & test.potential.venues$Suburb== "Blackburn South", c("RegionLocation", "DistanceToCBD.km")] <- c("City Border", 20)
test.potential.venues[is.na(test.potential.venues$RegionLocation) & test.potential.venues$Suburb== "Hawthorn", c("RegionLocation", "DistanceToCBD.km")] <- c("City", 10)
test.potential.venues[is.na(test.potential.venues$RegionLocation) & test.potential.venues$Suburb== "Mitcham", c("RegionLocation", "DistanceToCBD.km")] <- c("Suburb", 30)
test.potential.venues[is.na(test.potential.venues$RegionLocation) & test.potential.venues$Suburb== "Point Cook", c("RegionLocation", "DistanceToCBD.km")] <- c("Suburb", 30)
test.potential.venues[is.na(test.potential.venues$RegionLocation) & test.potential.venues$Suburb== "Ringwood", c("RegionLocation", "DistanceToCBD.km")] <- c("Suburb", 30)
test.potential.venues[is.na(test.potential.venues$RegionLocation) & test.potential.venues$Suburb== "St Kilda", c("RegionLocation", "DistanceToCBD.km")] <- c("City", 10)
test.potential.venues[is.na(test.potential.venues$RegionLocation) & test.potential.venues$Suburb== "The Patch", c("RegionLocation", "DistanceToCBD.km")] <- c("Outer Suburb", 40)
test.potential.venues[is.na(test.potential.venues$RegionLocation) & test.potential.venues$Suburb== "Yering", c("RegionLocation", "DistanceToCBD.km")] <- c("Outer Suburb", 40)
test.potential.venues[is.na(test.potential.venues$RegionLocation) & test.potential.venues$Suburb== "Como", c("RegionLocation", "DistanceToCBD.km")] <-c("Outer Suburb", 40)
test.potential.venues[is.na(test.potential.venues$RegionLocation) & test.potential.venues$Suburb== "Berriedale", c("RegionLocation", "DistanceToCBD.km")] <-c("City Border", 11)
test.potential.venues[is.na(test.potential.venues$RegionLocation) & test.potential.venues$Suburb== "Metcalfe", c("RegionLocation", "DistanceToCBD.km")] <-c("Outer Suburb", 40)
test.potential.venues[is.na(test.potential.venues$RegionLocation) & test.potential.venues$Suburb== "Yannathan", c("RegionLocation", "DistanceToCBD.km")] <-c("Outer Suburb", 75)
test.potential.venues$RegionLocation[is.na(test.potential.venues$RegionLocation)] <- "Remote area"
test.potential.venues$DistanceToCBD.km[is.na(test.potential.venues$DistanceToCBD.km)] <-  100

test.potential.venues$DistanceToCBD.km.truncated <- test.potential.venues$DistanceToCBD.km
test.potential.venues[test.potential.venues$DistanceToCBD.km.truncated > 100, "DistanceToCBD.km.truncated"] <- 100
test.potential.venues$DistanceToCBD.km.truncated <- as.numeric(test.potential.venues$DistanceToCBD.km.truncated)


impute.mean <- function(x) replace(x, is.na(x), mean(as.numeric(x), na.rm = TRUE))

test.potential.venues <- test.potential.venues %>% group_by(State, RegionLocation) %>% mutate(
  Populationdensity.ERPat30June.persons.km2 = impute.mean(Populationdensity.ERPat30June.persons.km2),
  Medianequivalisedtotalhouseholdincome.weekly.AUD = impute.mean(Medianequivalisedtotalhouseholdincome.weekly.AUD ),
  MedianAge.Persons.years = impute.mean(MedianAge.Persons.years ),
  WorkingAgePopulation.aged15.64years = impute.mean(WorkingAgePopulation.aged15.64years),
  MaleFemalePerc = impute.mean(MaleFemalePerc ),
  Australiancitizen.Perc = impute.mean(Australiancitizen.Perc),
  Totalfamilies.no. = impute.mean(Totalfamilies.no. ),
  SecularBeliefs.Perc = impute.mean(SecularBeliefs.Perc)
)


# test.potential.venues[test.potential.venues$RegionLocation== "Inner City", "RegionLocation"] <- "City"
# test.potential.venues[test.potential.venues$RegionLocation== "City Border", "RegionLocation"] <- "City"
# test.potential.venues[test.potential.venues$RegionLocation== "Outer Suburb", "RegionLocation"] <- "Suburb"
# test.potential.venues$RegionLocation <- factor(test.potential.venues$RegionLocation)

test.potential.venues$Year.OfBooking <- as.factor(test.potential.venues$Year.OfBooking)
preds.potential.clubs <- predict(model, newdata = test.potential.venues)
test.potential.venues$Rating <- preds.potential.clubs

ggplot(test.potential.venues[test.potential.venues$State=="ACT", ], aes(x=Rating, y=Club.Facility.Name, size=nbr.courts, col=discretize(MembersByCourt, breaks = 5))) + 
  geom_point()  + ylab("") + xlab("log(Utilisation)")#+ facet_wrap_paginate(~test.potential.venues$State)



# club.distance.to.cbd[club.distance.to.cbd$DistanceToCBD.km<= 5 , "RegionLocation"] <- "Inner City"
# club.distance.to.cbd[club.distance.to.cbd$DistanceToCBD.km>= 6  & club.distance.to.cbd$DistanceToCBD.km< 11 , "RegionLocation"] <- "City"
# club.distance.to.cbd[club.distance.to.cbd$DistanceToCBD.km>= 11 & club.distance.to.cbd$DistanceToCBD.km< 21 , "RegionLocation"] <- "City Border"
# club.distance.to.cbd[club.distance.to.cbd$DistanceToCBD.km>= 21 & club.distance.to.cbd$DistanceToCBD.km< 40 , "RegionLocation"] <- "Suburb"
# club.distance.to.cbd[club.distance.to.cbd$DistanceToCBD.km>= 41 & club.distance.to.cbd$DistanceToCBD.km< 80 , "RegionLocation"] <- "Outer Suburb"
#

View(test.potential.venues[is.na(test.potential.venues$DistanceToCBD.km.truncated),])
nrow(test.potential.venues[is.na(test.potential.venues$DistanceToCBD.km.truncated),])

#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------

model <- multinom(Utilisation.Category ~#Utilisation.Category Booking.and.Utilisation.Category
                    State + 
                    log(Populationdensity.ERPat30June.persons.km2)+ 
                    Totalfamilies.no. +
                    RegionLocation+
                    MaleFemalePerc +
                    male.member.ratio+
                    AV + 
                    #(ClubsInSuburb)+
                    recently.advertised +
                    #(Medianequivalisedtotalhouseholdincome.weekly.AUD) +
                    #SpeaksaLanguageOtherThanEnglishatHome.Proportionoftotalpopulation.Perc + 
                    (MedianAge.Persons.years)+ 
                    Mean.Age +
                    (WorkingAgePopulation.aged15.64years)+
                    Australiancitizen.Perc+
                    SecularBeliefs.Perc+
                    Booking.Month + 
                    Year.OfBooking+
                    Booking.Day +
                    Facility.Type +  
                    #Organisation.Type +
                    #Organisation.Status+
                    log(MembersByCourt)+ 
                    Total.Opening.Hours*nbr.courts +
                    nbr.courts*court.options +
                    #has.hard*has.clay*has.grass +
                    has.Lights +
                    has.hot.shot+ 
                    Amenities.options,
            data = train, maxit=1000)
#summary(model)

preds <- predict(model, newdata = test)
preds.table.2 <- as.matrix(table(Actual = preds, Predicted = test$Utilisation.Category))
preds.table <- table(preds, test$Utilisation.Category)
#confusionMatrix(preds, test$Utilisation.Category)
accuracy = sum(diag(preds.table))/sum(preds.table)
accuracy

#sum(diag(preds.table.2))/sum(preds.table.2)


weak.accuracy = (sum(diag(preds.table)) + 
                      (preds.table[1,2] + preds.table[2,1] +
                      preds.table[2,3] + preds.table[3,2] +
                      preds.table[3,4] + preds.table[4,3] +
                      preds.table[4,5] + preds.table[5,4]))/sum(preds.table)
weak.accuracy
#summary(model)

#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------

test.potential.venues <- potential.courts[ ,  c("State" , "Populationdensity.ERPat30June.persons.km2", "RegionLocation" , "ClubsInSuburb" , 
                                                "recently.advertised" , "Medianequivalisedtotalhouseholdincome.weekly.AUD" , "MedianAge.Persons.years", 
                                                "WorkingAgePopulation.aged15.64years", "MaleFemalePerc", "Australiancitizen.Perc", "Booking.Month" , 
                                                "Year.OfBooking" , "Booking.Day","Total.Opening.Hours", "Facility.Type" , "Organisation.Type" ,
                                                "nbr.courts",  "Lighted.Full.Count.All","has.Lights", "has.indoor", "has.outdoor", "has.hard", 
                                                "has.clay", "has.grass", "has.hot.shot", "MembersByCourt", "court.options", "Totalfamilies.no.")]

test.potential.venues$Year.OfBooking <- as.factor(test.potential.venues$Year.OfBooking)
preds.potential.clubs <- predict(model, newdata = test.potential.venues)
potential.courts$Rating <- preds.potential.clubs
#View(potential.courts)

ggplot(potential.courts, aes(x=Rating, y=Club.Facility.Name, size=nbr.courts, col=discretize(MembersByCourt, breaks = 5))) + geom_point() + ylab("")

#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
# plot predicted probabilities to help you understand the model
head(fitted(model))
dwrite <- data.frame(State = rep(c("VIC", "NSW", "ACT", "WA", "SA"), each = 22), Populationdensity.ERPat30June.persons.km2 = rep(seq(100,11000,by=500), 5),
                     #State= "VIC", 
                     RegionLocation = "City", ClubsInSuburb = 1, recently.advertised= FALSE, Medianequivalisedtotalhouseholdincome.weekly.AUD = mean(bookings.made.v.grouped$Medianequivalisedtotalhouseholdincome.weekly.AUD),
                     MedianAge.Persons.years = mean(bookings.made.v.grouped$MedianAge.Persons.years), 
                     WorkingAgePopulation.aged15.64years = mean(bookings.made.v.grouped$WorkingAgePopulation.aged15.64years), 
                     MaleFemalePerc = mean(bookings.made.v.grouped$MaleFemalePerc), 
                     Australiancitizen.Perc= mean(bookings.made.v.grouped$Australiancitizen.Perc), Booking.Month = "February",
                     Year.OfBooking = 2018,
                     Booking.Day = "Saturday", 
                     Total.Opening.Hours = 15,
                     Facility.Type= "Club",
                     Organisation.Type= "Club",
                     nbr.courts = 6, has.Lights= TRUE,  has.hard= TRUE,
                     has.clay=TRUE, has.grass= FALSE, has.hot.shot= TRUE)
## store the predicted probabilities for each value of ses and write
pp.write <- cbind(dwrite, predict(model, newdata = dwrite, type = "probs", se = TRUE))
## calculate the mean probabilities within each level of ses
by(pp.write[, 24:27], pp.write$State, colMeans)

## melt data set to long for ggplot2
lpp <- melt(pp.write, id.vars = c("State", "Populationdensity.ERPat30June.persons.km2"), value.name = "probability")
head(lpp)  # view first few rows

lpp.2 <- lpp[lpp$variable == "Medianequivalisedtotalhouseholdincome.weekly.AUD", ]
## plot predicted probabilities across write values for each level of ses
## facetted by program type
ggplot(lpp.2, aes(x = Populationdensity.ERPat30June.persons.km2, y = probability, colour = State)) + geom_line() + facet_grid(variable ~ ., scales = "free") #+
 # facet_wrap(~variable)


#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
#linear model
model <- lm(log(Utilisation) ~ #Utilisation   log(Booking.Duration) ~ #
            State + 
            log(Populationdensity.ERPat30June.persons.km2)+ 
            #log(Total.Availablility) +
            #Geographical.classification +
            RegionLocation +
            #ClubsInSuburb+
            recently.advertised +
            #LandArea.Km2 + 
            #Persons.Total.no. +
            #Totalfamilies.no. + 
            Christianity.Perc + 
            #Buddhism.Perc + 
            #Islam.Perc + 
            #Hinduism.Perc +  #Judaism.Perc +  #OtherReligions.Perc +  
            SecularBeliefs.Perc + 
            #Totalnumberofbusinesses.no. + 
            #Totalbornoverseas.Perc + 
            Medianequivalisedtotalhouseholdincome.weekly.AUD + 
            #WithGraduateDiploma.GraduateCertificate.Perc + 
            #SpeaksaLanguageOtherThanEnglishatHome.Proportionoftotalpopulation.Perc +
            #WithPostgraduateDegree.Perc +
            #WithPostSchoolQualifications.Perc +
            MedianAge.Persons.years+ 
            WorkingAgePopulation.aged15.64years+
            MaleFemalePerc+
            #BorninAmericas.Perc+ 
            #BorninNorthAfricaandtheMiddleEast.Perc+
            #BorninNorth.WestEurope.Perc+
            #BorninOceaniaandAntarctica.excludingAustralia.Perc+
            #BorninSouth.EastAsia.Perc+
            #BorninNorth.EastAsia.Perc+ 
            #BorninSouthernandCentralAsia.Perc+
            #BorninSouthernandEasternEurope.Perc+ 
            #BorninSub.SaharanAfrica.Perc +
            Australiancitizen.Perc+
            Booking.Month + 
            as.factor(Year.OfBooking)+
            #Booking.Week.Number +
            Booking.Day +
            Total.Opening.Hours+
            #Total.Availablility +
            Facility.Type +  
            Organisation.Type +
            #Organisation.Status+ 
            #court.options+
            nbr.courts +
            has.Lights + has.hard*has.clay*has.grass + has.hot.shot,
            #Total.Full.Count.Grass + 
            #Total.Full.Count.Synthetic.Grass + 
            #Total.Full.Count.Clay  +
            #Total.Full.Count.Synthetic.Clay + 
            #Total.Full.Count.Non.Cushioned.Hard.Court + 
            #Total.Full.Count.Cushioned.Hard.Court + 
            #Total.Full.Count.Other +      
            #Hot.Shots.Red.count.All,      
            #Hot.Shots.Orange.Count.All + 
            #Total.Full.Count.All +      
            #Other.Count.All + 
            #Outdoor.Full.Count.All +      
            #Indoor.Full.Count.All + 
            #Lighted.Full.Count.All,
            #Total.Unavailablility,
            #unavailability,
            #offset = Total.Availablility,
            data = bookings.made.v.grouped)
summary(model)
vif(model)



View(which(coefficients(model)>0))
levels(train$RegionLocation)
#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
#linear model


model <- lm(Utilisation^lambda ~ #Utilisation  
              Venue.Name + 
              #Booking.Day +
              Booking.Month + 
              as.factor(Year.OfBooking), 
              #offset = log(Populationdensity.ERPat30June.persons.km2),
              data = train)
summary(model)
vif(model)
qqnorm(model$residuals)
qqline(model$residuals)



model <- lm(Booking.Duration^lambda ~ #Utilisation  
              Venue.Name + 
              #Booking.Day +
              Booking.Month + 
              as.factor(Year.OfBooking), 
            #offset = log(Populationdensity.ERPat30June.persons.km2),
            data = bookings.made.v.grouped)
summary(model)
vif(model)
qqnorm(model$residuals)
qqline(model$residuals)

#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
#Mixed-model

bc<- boxCox(model)
lambda<- bc$x[which.max(bc$y)]
lambda

model <- lmer(log(Utilisation)  ~ #Utilisation  
              (1|Venue.Name) + 
              #Booking.Day +
              Booking.Month + 
              as.factor(Year.OfBooking) + 
              normBetweenZeroOne(Total.Availablility) +
                nbr.courts,
            data = data.model)
#summary(model)
dotplot(ranef(model, condVar = T))

model <- lmer(log(Utilisation.Weighted.By.NbrCourts) ~ #Utilisation  
                (1|Venue.Name) + 
                #Booking.Day +
                Booking.Month + 
                as.factor(Year.OfBooking) + 
                Total.Availablility,
              data = data.model)
#summary(model)
dotplot(ranef(model, condVar = T))

model <- lmer(Booking.Duration^lambda  ~ #Utilisation  
                (1|Venue.Name) + 
                #Booking.Day +
                Booking.Month + 
                as.factor(Year.OfBooking)+ offset(log(Total.Availablility)),
              data = bookings.made.v.grouped)
summary(model)
dotplot(ranef(model, condVar = T)) 


#library(DAAG)
cv.lm(data = data.model, form.lm = formula(log(Utilisation.Weighted.By.NbrCourts)~#log(Utilisation) Utilisation.Weighted.By.NbrCourts
                                             State + 
                                             log(Populationdensity.ERPat30June.persons.km2)+ 
                                             Totalfamilies.no. +
                                             RegionLocation+
                                             MaleFemalePerc +
                                             male.member.ratio+
                                             #(ClubsInSuburb)+
                                             recently.advertised +
                                             #(Medianequivalisedtotalhouseholdincome.weekly.AUD) +
                                             #SpeaksaLanguageOtherThanEnglishatHome.Proportionoftotalpopulation.Perc + 
                                             (MedianAge.Persons.years)+ 
                                             Mean.Age +
                                             (WorkingAgePopulation.aged15.64years)+
                                             Australiancitizen.Perc+
                                             SecularBeliefs.Perc+
                                             Booking.Month + 
                                             Year.OfBooking+
                                             Booking.Day +
                                             has.Lights+
                                             Facility.Type +  
                                             #Organisation.Type +
                                             #nbr.courts,
                                             log(MembersByCourt)+ 
                                             Total.Opening.Hours*nbr.courts +
                                             nbr.courts*court.options +
                                             has.hard*has.clay*has.grass +
                                             has.hot.shot), m=3, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)

