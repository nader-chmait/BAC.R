library(car)
library(lme4)
library(lattice)
#library(DAAG)
library(nnet)
library(caret)
library(e1071)
require(reshape2)



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

# cv.lm(data = bookings.made.v.grouped, form.lm = formula(log(Utilisation) ~ State + Populationdensity.ERPat30June.persons.km2 +
#                                                           RegionLocation + ClubsInSuburb + recently.advertised + Medianequivalisedtotalhouseholdincome.weekly.AUD +
#                                                           MedianAge.Persons.years + WorkingAgePopulation.aged15.64years + MaleFemalePerc + Australiancitizen.Perc + 
#                                                           Booking.Month + as.factor(Year.OfBooking) + Booking.Day +
#                                                           Total.Opening.Hours+ Facility.Type + Organisation.Type + nbr.courts + has.Lights + 
#                                                           has.indoor + has.hard + has.clay + has.grass + has.hot.shot), m=3, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)

bookings.made.v.grouped$Utilisation.Category  <- relevel(bookings.made.v.grouped$Utilisation.Category , ref = "-2")
bookings.made.v.grouped$Total.Bookings.Category  <- relevel(bookings.made.v.grouped$Total.Bookings.Category , ref = "-2")
bookings.made.v.grouped$State <- factor(bookings.made.v.grouped$State)
bookings.made.v.grouped$Organisation.Type <- factor(bookings.made.v.grouped$Organisation.Type)
bookings.made.v.grouped$Organisation.Status <- factor(bookings.made.v.grouped$Organisation.Status)
bookings.made.v.grouped$Facility.Type <- factor(bookings.made.v.grouped$Facility.Type)
bookings.made.v.grouped$Year.OfBooking <- factor(bookings.made.v.grouped$Year.OfBooking)


#bookings.members.by.court <- members.by.court.final[members.by.court.final$nbr.courts>0 , c("Club.Facility.Name", "Facility.Name", "Club.Name", "MembersByCourt")]
#bookings.members.by.court$Facility.Name <- proper(bookings.members.by.court$Facility.Name)
bookings.made.v.grouped$Facility.Name <- proper(bookings.made.v.grouped$Facility.Name)

data.model <- bookings.made.v.grouped

bookings.members.by.court <- bookings.members.by.court[!duplicated(bookings.members.by.court$Facility.Name),]
data.model <- merge(data.model, bookings.members.by.court, by.x="Facility.Name", by.y = "Facility.Name", all.x=T) # 


members.by.nbr.of.courts <- members.by.court.final[members.by.court.final$nbr.courts>0 , c("nbr.courts", "MembersByCourt")]
members.by.nbr.of.courts<- members.by.nbr.of.courts %>% group_by(nbr.courts) %>% summarise(MembersByCourt = median(MembersByCourt)) 
data.model[is.na(data.model$MembersByCourt), "MembersByCourt"] <- 
 c(members.by.nbr.of.courts[data.model[is.na(data.model$MembersByCourt), "nbr.courts"] , "MembersByCourt"])


members.gender.ratio <- members.gender.ratio[!duplicated(members.gender.ratio$Facility.Name),]
data.model <- merge(data.model, members.gender.ratio[,c("Facility.Name", "male.member.ratio")], by.x="Facility.Name", by.y = "Facility.Name", all.x=T) # 

data.model[is.na(data.model$male.member.ratio), "male.member.ratio"] <- mean(members.gender.ratio$male.member.ratio)
#summary(data.model$male.member.ratio)

data.model$Booking.and.Utilisation <- normBetweenMinMax(data.model$Total.Bookings, 0,50) + 
  normBetweenMinMax(data.model$Utilisation, 0,50)

data.model$Booking.and.Utilisation.Category <- cut(data.model$Booking.and.Utilisation, 
                                                                breaks = quantile(data.model$Booking.and.Utilisation, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)), 
                                                       include.lowest = TRUE,
                                                       labels= c( "-2","-1", "+0", "+1", "+2"))
#View(head(data.model))
#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
set.seed(1987) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(data.model), size = floor(.8*nrow(data.model)), replace = F)
train <- data.model[sample, ]
test  <- data.model[-sample, ]
#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------

model <- multinom(Utilisation.Category ~#Utilisation.Category Booking.and.Utilisation.Category
                    State + 
                    (Populationdensity.ERPat30June.persons.km2)+ 
                    log(Totalfamilies.no.) +
                    RegionLocation+
                    #MaleFemalePerc +
                    (ClubsInSuburb)+
                    recently.advertised +
                    log(Medianequivalisedtotalhouseholdincome.weekly.AUD) + 
                    (MedianAge.Persons.years)+ 
                    (WorkingAgePopulation.aged15.64years)+
                    (Australiancitizen.Perc)+
                    Booking.Month + 
                    Year.OfBooking+
                    Booking.Day +
                    #has.Lights+
                    Facility.Type +  
                    Organisation.Type +
                    #nbr.courts,
                    MembersByCourt +
                    #has.hard*has.clay*has.grass + has.hot.shot,
                    Total.Opening.Hours*nbr.courts+
                    nbr.courts*court.options,
                  offset= log(1/MembersByCourt),
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
model <- lm(log(Utilisation)~#Utilisation.Category
              State + 
              Populationdensity.ERPat30June.persons.km2+ 
              log(Totalfamilies.no.) +
              RegionLocation+
              #MaleFemalePerc +
              male.member.ratio+
              (ClubsInSuburb)+
              recently.advertised +
              log(Medianequivalisedtotalhouseholdincome.weekly.AUD) + 
              (MedianAge.Persons.years)+ 
              (WorkingAgePopulation.aged15.64years)+
              Australiancitizen.Perc+
              SecularBeliefs.Perc+
              Booking.Month + 
              Year.OfBooking+
              Booking.Day +
              #has.Lights+
              Facility.Type +  
              Organisation.Type +
              #nbr.courts,
              MembersByCourt+ 
              Total.Opening.Hours*nbr.courts +
              #has.hard*has.clay*has.grass + has.hot.shot,
              nbr.courts*court.options,
            #offset= (nbr.courts),
            data = train)
summary(model)
qqnorm(model$residuals)
qqline(model$residuals)
car::vif(model)
library(arm)
coefplot(model, vertical=F, offset=0.1, CI=1) + grid(length(coefficients(model)), 3) # mar=c(12,2.5,2,2),

test.potential.venues <- potential.courts[ ,  c("State" , "Populationdensity.ERPat30June.persons.km2", "RegionLocation" , "ClubsInSuburb" , 
                                                "recently.advertised" , "Medianequivalisedtotalhouseholdincome.weekly.AUD" , "MedianAge.Persons.years", 
                                                "WorkingAgePopulation.aged15.64years", "MaleFemalePerc", "Australiancitizen.Perc", "Booking.Month" , 
                                                "Year.OfBooking" , "Booking.Day","Total.Opening.Hours", "Facility.Type" , "Organisation.Type" ,
                                                "nbr.courts",  "Lighted.Full.Count.All","has.Lights", "has.indoor", "has.outdoor", "has.hard", 
                                                "has.clay", "has.grass", "has.hot.shot", "MembersByCourt", "court.options", "Totalfamilies.no.", "SecularBeliefs.Perc",
                                                "Club.Facility.Name", "Club.Name")]

test.potential.venues$Year.OfBooking <- as.factor(test.potential.venues$Year.OfBooking)
preds.potential.clubs <- predict(model, newdata = test.potential.venues)
#potential.courts$Rating <- preds.potential.clubs
test.potential.venues$Rating <- preds.potential.clubs
#View(potential.courts)

ggplot(test.potential.venues, aes(x=Rating, y=Club.Facility.Name, size=nbr.courts, col=discretize(MembersByCourt, breaks = 5))) + 
  geom_point() + ylab("") + xlab("log(Utilisation)")

#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
#linear model
model <- lm(log(Utilisation) ~ #Utilisation   log(Booking.Duration) ~ #
            State + 
            log(Populationdensity.ERPat30June.persons.km2)+ 
            #log(Total.Availablility) +
            #Geographical.classification +
            RegionLocation +
            ClubsInSuburb+
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


bc<- boxCox(model)
lambda<- bc$x[which.max(bc$y)]
lambda



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

model <- lmer(Utilisation^lambda  ~ #Utilisation  
              (1|Venue.Name) + 
              #Booking.Day +
              Booking.Month + 
              as.factor(Year.OfBooking),
            data = bookings.made.v.grouped)
summary(model)
dotplot(ranef(model, postVar = T))

model <- lmer(Booking.Duration^lambda  ~ #Utilisation  
                (1|Venue.Name) + 
                #Booking.Day +
                Booking.Month + 
                as.factor(Year.OfBooking)+ offset(log(Total.Availablility)),
              data = bookings.made.v.grouped)
summary(model)
dotplot(ranef(model, postVar = T))
