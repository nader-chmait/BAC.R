library(ggplot2)
library(car)
normBetweenZeroOne <-
  function(x) {
    x <- (x - min(x)) / (max(x) - min(x))
    return(x)
  }
normBetweenMinMax <-
  function(x, min.x, max.x) {
    x <- (x - min.x) / (max.x - min.x)
    return(x)
  }


bookings.closed <-  bookings.made.v%>% group_by(Venue.ID, Venue.Name, Year.OfBooking, Booking.Month, Booking.Day, Booking.Date, Total.Closing.Mins) %>%
  summarise(totaldays = 1)
bookings.closed <-  bookings.closed%>%  group_by(Venue.ID, Venue.Name, Year.OfBooking, Booking.Month, Booking.Day) %>%
  summarise(totaldays = sum(totaldays), Total.Closing.Mins.All.Days = sum(Total.Closing.Mins))


bookings.made.v$individual.booking <- 1
bookings.made.v.grouped <-  bookings.made.v%>% group_by(Venue.Name, Suburb, Post.Code, State, Year.OfBooking, Booking.Month, Booking.Day, Booking.Weekend, Total.Closing.Mins,
                                                        Total.Opening.Hours, SpeaksaLanguageOtherThanEnglishatHome.Proportionoftotalpopulation.Perc, Australiancitizen.Perc,
                                                        ClubsInSuburb, recently.advertised,
                                                        REGIONTYPE, Facility.Type, Organisation.Type, Organisation.Status, Populationdensity.ERPat30June.persons.km2,
                                                        Booking.Type, Density, MaleFemalePerc, Geographical.classification, 
                                                        Total.Full.Count.Grass                          ,Total.Full.Count.Non.Cushioned.Hard.Court ,      
                                                        Total.Full.Count.Synthetic.Grass                ,Total.Full.Count.Clay                     ,      
                                                        Total.Full.Count.Cushioned.Hard.Court           ,Total.Full.Count.Other                    ,      
                                                        Total.Full.Count.Synthetic.Clay                 ,Hot.Shots.Red.count.All                   ,      
                                                        Hot.Shots.Orange.Count.All                      ,Total.Full.Count.All                      ,      
                                                        Other.Count.All                                 ,Outdoor.Full.Count.All                    ,      
                                                        Indoor.Full.Count.All                           ,Lighted.Full.Count.All                    ,      
                                                        has.Lights                                      ,has.indoor                                ,      
                                                        has.outdoor                                     ,has.grass                                 ,      
                                                        has.clay                                        ,has.hard                                  ,      
                                                        has.hot.shot                                    ,court.options                             ,      
                                                        nbr.courts                                      ,Aboriginal.Perc                           ,      
                                                        Buddhism.Perc                                   ,Christianity.Perc                         ,      
                                                        CompletedYear12orequivalent.Perc                ,Females.Total.no.                         ,      
                                                        Hinduism.Perc                                   ,Islam.Perc                                ,      
                                                        Judaism.Perc                                    ,LandArea.Ha                               ,      
                                                        Males.Total.no.                                 ,MedianAge.Females.years                   ,      
                                                        MedianAge.Males.years                           ,MedianAge.Persons.years                   ,      
                                                        Medianequivalisedtotalhouseholdincome.weekly.AUD,NotanAustraliancitizen.Perc               ,      
                                                        OtherReligions.Perc                             ,Persons.Total.no.                         ,      
                                                        SecularBeliefs.Perc                             ,Establishmentswith15ormorerooms.no.        ,      
                                                        Totalbornoverseas.Perc                          ,Totalnumberofbusinesses.no.               ,      
                                                        WithGraduateDiploma.GraduateCertificate.Perc    ,WorkingAgePopulation.aged15.64years       ,
                                                        WithPostgraduateDegree.Perc, WithPostSchoolQualifications.Perc, 
                                                        BorninAmericas.Perc, BorninNorthAfricaandtheMiddleEast.Perc,
                                                        BorninNorth.EastAsia.Perc, BorninNorth.WestEurope.Perc,
                                                        BorninOceaniaandAntarctica.excludingAustralia.Perc,
                                                        BorninSouth.EastAsia.Perc,
                                                        BorninSouthernandCentralAsia.Perc,
                                                        BorninSouthernandEasternEurope.Perc, 
                                                        BorninSub.SaharanAfrica.Perc,
                                                        LandArea.Km2,
                                                        Persons.Total.no.,
                                                        Totalfamilies.no.) %>%
  summarise(PmBookings = sum(After6Pm), Court.Fee = sum(Court.Fee), Light.Fee= sum(Light.Fee), Admin.Fee = sum(Admin.Fee), Total.Cost = sum(Total.Cost), 
            Total.Bookings = sum(individual.booking), Booking.Duration = sum(Booking.Duration))

bookings.availability <-  bookings.made.v%>%  filter(Booking.Type != "Booking")  %>% 
  group_by(Venue.Name, Suburb, Post.Code, State, Year.OfBooking, Booking.Month, Booking.Day, Australiancitizen.Perc, ClubsInSuburb, recently.advertised,
                                                        Total.Closing.Mins, Total.Opening.Hours, SpeaksaLanguageOtherThanEnglishatHome.Proportionoftotalpopulation.Perc,
                                                        Booking.Weekend, REGIONTYPE, Facility.Type, Organisation.Type, Organisation.Status,
                                                        Density, MaleFemalePerc, Geographical.classification, Populationdensity.ERPat30June.persons.km2,
                                                        Total.Full.Count.Grass                          ,Total.Full.Count.Non.Cushioned.Hard.Court ,      
                                                        Total.Full.Count.Synthetic.Grass                ,Total.Full.Count.Clay                     ,      
                                                        Total.Full.Count.Cushioned.Hard.Court           ,Total.Full.Count.Other                    ,      
                                                        Total.Full.Count.Synthetic.Clay                 ,Hot.Shots.Red.count.All                   ,      
                                                        Hot.Shots.Orange.Count.All                      ,Total.Full.Count.All                      ,      
                                                        Other.Count.All                                 ,Outdoor.Full.Count.All                    ,      
                                                        Indoor.Full.Count.All                           ,Lighted.Full.Count.All                    ,      
                                                        has.Lights                                      ,has.indoor                                ,      
                                                        has.outdoor                                     ,has.grass                                 ,      
                                                        has.clay                                        ,has.hard                                  ,      
                                                        has.hot.shot                                    ,court.options                             ,      
                                                        nbr.courts                                      ,Aboriginal.Perc                           ,      
                                                        Buddhism.Perc                                   ,Christianity.Perc                         ,      
                                                        CompletedYear12orequivalent.Perc                ,Females.Total.no.                         ,      
                                                        Hinduism.Perc                                   ,Islam.Perc                                ,      
                                                        Judaism.Perc                                    ,LandArea.Ha                               ,      
                                                        Males.Total.no.                                 ,MedianAge.Females.years                   ,      
                                                        MedianAge.Males.years                           ,MedianAge.Persons.years                   ,      
                                                        Medianequivalisedtotalhouseholdincome.weekly.AUD,NotanAustraliancitizen.Perc               ,      
                                                        OtherReligions.Perc                             ,Persons.Total.no.                         ,      
                                                        SecularBeliefs.Perc                             ,Establishmentswith15ormorerooms.no.        ,      
                                                        Totalbornoverseas.Perc                          ,Totalnumberofbusinesses.no.               ,      
                                                        WithGraduateDiploma.GraduateCertificate.Perc    ,WorkingAgePopulation.aged15.64years       ,
                                                        WithPostgraduateDegree.Perc, WithPostSchoolQualifications.Perc,
                                                        BorninAmericas.Perc, BorninNorthAfricaandtheMiddleEast.Perc,
                                                        BorninNorth.EastAsia.Perc, BorninNorth.WestEurope.Perc,
                                                        BorninOceaniaandAntarctica.excludingAustralia.Perc,
                                                        BorninSouth.EastAsia.Perc,
                                                        BorninSouthernandCentralAsia.Perc,
                                                        BorninSouthernandEasternEurope.Perc, 
                                                        BorninSub.SaharanAfrica.Perc,
                                                        LandArea.Km2,
                                                        Persons.Total.no.,
                                                        Totalfamilies.no.) %>% summarise(unavailability = sum(Booking.Duration) )
  


bookings.made.v.grouped <- bookings.made.v.grouped %>% filter(Booking.Type == "Booking")
bookings.made.v.grouped <- merge(bookings.made.v.grouped, bookings.availability, all.x = T)
bookings.made.v.grouped[is.na(bookings.made.v.grouped$unavailability), "unavailability"] <- 0
#nrow(bookings.made.v.grouped)
bookings.made.v.grouped <- merge(bookings.made.v.grouped, bookings.closed)
#nrow(bookings.made.v.grouped)
bookings.made.v.grouped$Total.Closing.Mins.All.Days <- bookings.made.v.grouped$Total.Closing.Mins.All.Days*bookings.made.v.grouped$nbr.courts
bookings.made.v.grouped$Total.Unavailablility <-  bookings.made.v.grouped$unavailability + 
  (bookings.made.v.grouped$totaldays*bookings.made.v.grouped$Total.Closing.Mins*bookings.made.v.grouped$nbr.courts)
bookings.made.v.grouped$Total.Availablility <-  bookings.made.v.grouped$totaldays*bookings.made.v.grouped$nbr.courts*1440 - bookings.made.v.grouped$Total.Unavailablility 
bookings.made.v.grouped$Utilisation <-  (bookings.made.v.grouped$Booking.Duration/bookings.made.v.grouped$Total.Availablility)*100
bookings.made.v.grouped$Total.Opening.Hours <- gsub(":00", "", bookings.made.v.grouped$Total.Opening.Hours)
bookings.made.v.grouped$Total.Opening.Hours <- gsub(":30", ".5", bookings.made.v.grouped$Total.Opening.Hours)
bookings.made.v.grouped$Total.Opening.Hours <- gsub(":15", ".25", bookings.made.v.grouped$Total.Opening.Hours)
bookings.made.v.grouped$Total.Opening.Hours <- gsub(":45", ".75", bookings.made.v.grouped$Total.Opening.Hours)
bookings.made.v.grouped$Total.Opening.Hours <- as.numeric(bookings.made.v.grouped$Total.Opening.Hours)
#View(unique(bookings.made.v.grouped[,c("Venue.Name", "Facility.Type", "Suburb", "Organisation.Type", "Organisation.Status")]))

#names(bookings.made.v.grouped)

bookings.made.v.grouped$Has.Late.Bookings <- bookings.made.v.grouped$Total.Opening.Hours>12
venue.regions <- read.csv("venueRegions.csv", header = T)
bookings.made.v.grouped <- merge(bookings.made.v.grouped, venue.regions, all.x = T)

bookings.made.v.grouped.norm <- bookings.made.v.grouped %>% group_by(Venue.Name, Suburb, Post.Code, State, nbr.courts ) %>% 
                                   mutate(Norm.bookings.count = Total.Bookings/max(Total.Bookings), 
                                          Norm.bookings.duration = normBetweenZeroOne(Booking.Duration),# /max(Booking.Duration),
                                          Norm.utilisation = normBetweenZeroOne(Utilisation),#/max(Utilisation),
                                          Norm.Total.Availablility = normBetweenZeroOne(Total.Availablility),#/max(Total.Availablility),
                                          Norm.Total.Unavailablility = Total.Unavailablility/max(Total.Unavailablility)) %>% ungroup()

#--------------------------------------------------------------------------------------------------------------------------------------------
##H:That venues with higher availability have more casual bookings (Demand is often greater than supply for casual bookers)
#--------------------------------------------------------------------------------------------------------------------------------------------
qplot(Norm.Total.Availablility, Norm.bookings.duration,  col = nbr.courts, data =bookings.made.v.grouped.norm) + 
  xlab("Availabilty (normalised [0,1])") + ylab("Normalised booking duration")+ labs(title = " Venues with higher availability have more casual bookings") 
cor.test(bookings.made.v.grouped.norm$Norm.Total.Availablility, bookings.made.v.grouped.norm$Norm.bookings.duration, use = "complete.obs")
#t.test(bookings.made.v.grouped.norm$Norm.Total.Availablility, bookings.made.v.grouped.norm$Norm.bookings.duration)


bookings.made.v.grouped.utilisation <- bookings.made.v.grouped  %>% group_by(Venue.Name, Suburb, Post.Code, State, Year.OfBooking, Populationdensity.ERPat30June.persons.km2) %>%
  summarise(Utilisation = sum(Utilisation), Total.Bookings = sum(Total.Bookings), Booking.Duration =sum(Booking.Duration), 
            Total.Availablility = sum(Total.Availablility), Total.Unavailablility = sum(Total.Unavailablility))


plot(bookings.made.v.grouped.utilisation$Total.Unavailablility, bookings.made.v.grouped.utilisation$Booking.Duration)
cor.test(bookings.made.v.grouped.utilisation$Total.Availablility, bookings.made.v.grouped.utilisation$Booking.Duration)


#--------------------------------------------------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------------------------------------------------
# list clubs by order of utilisation
#--------------------------------------------------------------------------------------------------------------------------------------------

t<- cbind(head(arrange(bookings.made.v.grouped.utilisation[bookings.made.v.grouped.utilisation$Year.OfBooking ==2016, 
                                                 c("Venue.Name",  "Utilisation", "Booking.Duration", "Total.Availablility")],desc(Utilisation)), n = 5),
head(arrange(bookings.made.v.grouped.utilisation[bookings.made.v.grouped.utilisation$Year.OfBooking ==2017, 
                                                 c("Venue.Name",  "Utilisation", "Booking.Duration", "Total.Availablility")],desc(Utilisation)), n = 5),
head(arrange(bookings.made.v.grouped.utilisation[bookings.made.v.grouped.utilisation$Year.OfBooking ==2018, 
                                                 c("Venue.Name", "Utilisation", "Booking.Duration", "Total.Availablility")],desc(Utilisation)), n = 5)
)
t
#write.csv(t,"most_untilised_clubs.csv")


#--------------------------------------------------------------------------------------------------------------------------------------------
# Rank clubs by Utilisation and Total Bookings
#--------------------------------------------------------------------------------------------------------------------------------------------
data1 <- bookings.made.v.grouped  %>% group_by(Venue.Name, Suburb, Post.Code, State, Populationdensity.ERPat30June.persons.km2) %>%
  summarise(Utilisation = sum(Utilisation), Total.Bookings = sum(Total.Bookings), Booking.Duration =sum(Booking.Duration), 
            Total.Availablility = sum(Total.Availablility), Total.Unavailablility = sum(Total.Unavailablility))
data1 <- as.data.frame(data1)
data1 <- data1[, c("Venue.Name",  "Utilisation", "Booking.Duration", "Total.Availablility")]
data1$Booking.Duration <- normBetweenZeroOne(data1$Booking.Duration)
data1$Total.Availablility <- normBetweenZeroOne(data1$Total.Availablility)
data1$Utilisation <- normBetweenZeroOne(data1$Utilisation)
W1 <- 0.7
W2 <- 0.5
data1$Ranking.Criterion1 <-  W1*(data1$Utilisation) + (1-W1)*(data1$Total.Availablility)
data1$Ranking.Criterion2 <-  W2*(data1$Utilisation) + (1-W2)*(data1$Total.Availablility)
data1 <- arrange(data1, desc(Ranking.Criterion1))

cor.test( ~ Ranking.Criterion1 + Ranking.Criterion2, 
          data=data1,
          method = "spearman",
          continuity = FALSE,
          conf.level = 0.95)

#head(data1)                   
#--------------------------------------------------------------------------------------------------------------------------------------------



