library(car)
library(lme4)
library(lattice)
library(DAAG)
library(nnet)
library(caret)
library(e1071)

#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
#multinomial model
#quantile(bookings.made.v.grouped$Utilisation, probs = c(0, 0.25, 0.35, 0.5, 0.7, 0.8, 0.9, 0.95, 0.98,1))
#c(0.014,  1.004,  1.574,  2.825,  5.890,  8.611, 13.660, 20.000, 27.568, 58.889 )
bookings.made.v.grouped$Booking.Month <- as.factor(bookings.made.v.grouped$Booking.Month)
bookings.made.v.grouped$Utilisation.Category <-  cut(bookings.made.v.grouped$Utilisation, breaks = c(0.014, 1.004,  2.825,  7.158, 27.568, 58.889 ), 
                                                     include.lowest = TRUE,
                                                     labels= c( "-2","-1", "0", "+1", "+2"))
#View(bookings.made.v.grouped[1:10,c("Utilisation", "Utilisation.Category")])

cv.lm(data = bookings.made.v.grouped, form.lm = formula(log(Utilisation) ~ State + Populationdensity.ERPat30June.persons.km2 +
                                                          RegionLocation + ClubsInSuburb + recently.advertised + Medianequivalisedtotalhouseholdincome.weekly.AUD +
                                                          MedianAge.Persons.years + WorkingAgePopulation.aged15.64years + MaleFemalePerc + Australiancitizen.Perc + 
                                                          Booking.Month + as.factor(Year.OfBooking) + Booking.Day +
                                                          Total.Opening.Hours+ Facility.Type + Organisation.Type + court.options), m=3, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)

bookings.made.v.grouped$Utilisation.Category  <- relevel(bookings.made.v.grouped$Utilisation.Category , ref = "0")
set.seed(1987) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(bookings.made.v.grouped), size = floor(.8*nrow(bookings.made.v.grouped)), replace = F)
train <- bookings.made.v.grouped[sample, ]
test  <- bookings.made.v.grouped[-sample, ]


model <- multinom(Utilisation.Category~
              State + 
              Populationdensity.ERPat30June.persons.km2+ 
              RegionLocation +
              ClubsInSuburb+
              recently.advertised +
              Medianequivalisedtotalhouseholdincome.weekly.AUD + 
              MedianAge.Persons.years+ 
              WorkingAgePopulation.aged15.64years+
              MaleFemalePerc+
              Australiancitizen.Perc+
              Booking.Month + 
              as.factor(Year.OfBooking)+
              Booking.Day +
              Total.Opening.Hours+
              Facility.Type +  
              Organisation.Type +
              court.options,
            data = train, maxit=1000)
#summary(model)

preds <- predict(model, newdata = test)
preds.table <- table(preds, test$Utilisation.Category)
#confusionMatrix(preds, test$Utilisation.Category)
accuracy = sum(diag(preds.table))/sum(preds.table)
accuracy


test.potential.venues <- potential.courts[ ,  c("State" , "Populationdensity.ERPat30June.persons.km2", "RegionLocation" , "ClubsInSuburb" , 
                                                "recently.advertised" , "Medianequivalisedtotalhouseholdincome.weekly.AUD" , "MedianAge.Persons.years", 
                                                "WorkingAgePopulation.aged15.64years", "MaleFemalePerc", "Australiancitizen.Perc", "Booking.Month" , 
                                                "Year.OfBooking" , "Booking.Day","Total.Opening.Hours", "Facility.Type" , "Organisation.Type" , "court.options")]

preds.potential.clubs <- predict(model, newdata = test.potential.venues)
potential.courts$Rating <- preds.potential.clubs
View(potential.courts)
#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
#linear model
model <- lm(log(Utilisation) ~ #Utilisation  
            State + 
            Populationdensity.ERPat30June.persons.km2+ 
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
            court.options,
            #nbr.courts,
            #has.Lights +
            #has.indoor +
            #has.outdoor +
            #has.hard +
            #has.clay +
            #has.grass +
            #has.hot.shot,
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
qqnorm(model$residuals)
qqline(model$residuals)

bc<- boxCox(model)
lambda<- bc$x[which.max(bc$y)]
lambda

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
