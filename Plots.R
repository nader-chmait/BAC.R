library(ggplot2)
my.data <- bookings.made.v.grouped
colnames(my.data) #Total.Available.

#--------------------------------------------------------------------------------------------------------------------------------------------
# plot bookings by day and hour
#--------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = my.data, aes(x= Populationdensity.ERPat30June.persons.km2  , y =  Utilisation, col=as.factor(Booking.Month))) + geom_point() +
  facet_wrap(~my.data$State)
  #geom_text(aes(label=round(avg.ticket.price)),hjust=0, vjust=0)

#--------------------------------------------------------------------------------------------------------------------------------------------
# plot bookings by day and hour
#--------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = my.data, aes(x=  Populationdensity.ERPat30June.persons.km2 , y =  Booking.Duration, col=as.factor(Booking.Month))) + geom_point() +
  facet_wrap(~my.data$State)

#--------------------------------------------------------------------------------------------------------------------------------------------
# plot bookings by day and hour
#--------------------------------------------------------------------------------------------------------------------------------------------
data1 <- bookings.made.v %>% filter(Booking.Type == "Booking") %>% group_by(Booking.Day, booking.Hour, After6Pm) %>% summarise(count = sum(Booking.Duration))
ggplot(data = data1, aes(x=  booking.Hour , y =  count)) + geom_bar(stat = "identity", aes(fill = as.factor(After6Pm)))+
  facet_wrap(~data1$Booking.Day)
#--------------------------------------------------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------------------------------------------------
# plot bookings by day, hour and age
#--------------------------------------------------------------------------------------------------------------------------------------------
library(eeptools)
members.by.age <- bookings.made.members
members.by.age$DOB <- as.Date(members.by.age$Player.DOB, format = "%d/%m/%Y")
members.by.age[is.na(members.by.age$DOB) | members.by.age$DOB > "2010-01-01","DOB"] <- as.Date("2000-01-01")
members.by.age$Age <- floor(age_calc(members.by.age$DOB, units = "years"))
table(cut(members.by.age$Age,5))
members.by.age$AgeGroup <- cut(members.by.age$Age, breaks = c(0, 12,  17, 29, 49, 70, 88) )
#members.by.age[members.by.age$AgeGroup =="(70.4,86.1] ", "AgeGroup"] <- "(54.8,70.4]"
nrow(unique(members.by.age[,c("Player.First.Name","Player.Last.Name", "Player.DOB", "Venue.Name")])) 

data1 <- members.by.age %>% filter(Booking.Type == "Booking") %>% group_by(Booking.Day, booking.Hour, AgeGroup) %>% summarise(count = sum(Booking.Duration))
ggplot(data = data1, aes(x=  booking.Hour , y =  count)) + geom_bar(stat = "identity", aes(fill = as.factor(AgeGroup)))+
  facet_wrap(~data1$Booking.Day)



#gender ratio and distribution by age
data1 <- members.by.age %>% filter(Booking.Type == "Booking") %>% group_by(Player.First.Name, Player.Last.Name, Player.DOB, Player.Gender, Age) %>% summarise(count = n())
table(data1$Player.Gender)/sum(table(data1$Player.Gender))
nrow(data1)
table(cut(data1$Age,5))
#by gender
data1 <- members.by.age %>% filter(Booking.Type == "Booking") %>% group_by(Booking.Day, booking.Hour, Player.Gender) %>% summarise(count = sum(Booking.Duration))
data1$Player.Gender <- as.character(data1$Player.Gender)
data1[is.na(data1$Player.Gender) |data1$Player.Gender=="", "Player.Gender"] <- "Unkown"
ggplot(data = data1, aes(x=  booking.Hour , y =  count)) + geom_bar(stat = "identity", aes(fill = as.factor(Player.Gender)))+
  facet_wrap(~data1$Booking.Day)
#--------------------------------------------------------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------------------------------------------------------
# plot bookings by socio-econmic factors
#--------------------------------------------------------------------------------------------------------------------------------------------
data1 <- bookings.made.v.grouped %>% filter(Booking.Type == "Booking") %>% 
  group_by(Venue.Name, Medianequivalisedtotalhouseholdincome.weekly.AUD, CompletedYear12orequivalent.Perc, Geographical.classification, State) %>%
  summarise(Total.Bookings = sum(Total.Bookings), Utilisation = mean(Utilisation), Total.Availablility = mean(Total.Availablility))
data1 <- merge(data1, DistFromCBD[,c("Venue.Name", "DistFromCBD", "RegionLocation")], all.x = T)
data1 <- data1 %>% group_by(State, RegionLocation) %>% mutate(median.Income = mean(Medianequivalisedtotalhouseholdincome.weekly.AUD)) %>% ungroup()
data1[is.na(data1$RegionLocation), "RegionLocation"] <- 0
data1[data1$RegionLocation != "Inner City", "median.Income"] <- data1[data1$RegionLocation != "Inner City", "Medianequivalisedtotalhouseholdincome.weekly.AUD"]


ggplot(data = data1, aes(x= median.Income, y =  Utilisation)) +  #, label=State
  geom_point() + facet_wrap(~data1$State) + xlab("Median household income") + ylab("Utilisation") + #+ geom_text()
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# TO DO recalculate median income in city areas as players can come from nearby areas ******************
cor.test(data1$median.Income, data1$Utilisation)
#cor.test(data1[data1$State == "VIC", "median.Income"], data1[data1$State == "VIC", "Total.Bookings"])

ggplot(data = data1, aes(x= CompletedYear12orequivalent.Perc, y =  count)) + 
  geom_bar(stat = "identity") + xlab("Perc. completed year 12") + ylab("Utilisation") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

cor.test(data1$CompletedYear12orequivalent.Perc, data1$count)
#--------------------------------------------------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------------------------------------------------
# plot bookings by ethnicities and backgrounds
#--------------------------------------------------------------------------------------------------------------------------------------------
data1 <- bookings.made.v.grouped %>% filter(Booking.Type == "Booking") %>% summarise(BorninAmericas.Perc = sum(BorninAmericas.Perc),
                                                                                     BorninNorthAfricaandtheMiddleEast.Perc= sum(BorninNorthAfricaandtheMiddleEast.Perc),
                                                                                     BorninNorth.EastAsia.Perc= sum(BorninNorth.EastAsia.Perc),
                                                                                     BorninNorth.WestEurope.Perc= sum(BorninNorth.WestEurope.Perc),
                                                                                     BorninOceaniaandAntarctica.excludingAustralia.Perc= sum(BorninOceaniaandAntarctica.excludingAustralia.Perc),
                                                                                     BorninSouth.EastAsia.Perc= sum(BorninSouth.EastAsia.Perc),
                                                                                     BorninSouthernandCentralAsia.Perc= sum(BorninSouthernandCentralAsia.Perc),
                                                                                     BorninSouthernandEasternEurope.Perc= sum(BorninSouthernandEasternEurope.Perc),
                                                                                     BorninSub.SaharanAfrica.Perc= sum(BorninSub.SaharanAfrica.Perc)#,
                                                                                     #count = n()
                                                                                     )

#data1 <- data1/data1$count[1]
data2 <-t(data1)
op <- par(mar = c(12,4,4,2) + 0.2)
bc<- barplot(data2[,1], axisnames = FALSE) 
axis(1, at=bc, labels=gsub(".Perc|Bornin|.excludingAustralia", "", rownames(data2)), tick=FALSE,  las=2, line=-0.5)#, cex.axis=0.5 
par(op)
#--------------------------------------------------------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------------------------------------------------------
# plot bookings by religious backgrounds
#--------------------------------------------------------------------------------------------------------------------------------------------
data1 <- bookings.made.v.grouped 
data1 <- data1[,c("Booking.Type","SecularBeliefs.Perc", "Islam.Perc", "Hinduism.Perc", "Judaism.Perc", "Christianity.Perc", "Buddhism.Perc", 
                  "OtherReligions.Perc", "Aboriginal.Perc", "NotanAustraliancitizen.Perc")]

data1[is.na(data1$SecularBeliefs.Perc), "SecularBeliefs.Perc"] <- min(data1$SecularBeliefs.Perc, na.rm = T)
data1[is.na(data1$Islam.Perc), "Islam.Perc"] <- min(data1$Islam.Perc, na.rm = T)
data1[is.na(data1$Hinduism.Perc), "Hinduism.Perc"] <- min(data1$Hinduism.Perc, na.rm = T)
data1[is.na(data1$Judaism.Perc), "Judaism.Perc"] <- min(data1$Judaism.Perc, na.rm = T)
data1[is.na(data1$Christianity.Perc), "Christianity.Perc"] <- min(data1$Christianity.Perc, na.rm = T)
data1[is.na(data1$Buddhism.Perc), "Buddhism.Perc"] <- min(data1$Buddhism.Perc, na.rm = T)
data1[is.na(data1$OtherReligions.Perc), "OtherReligions.Perc"] <- min(data1$OtherReligions.Perc, na.rm = T)
data1[is.na(data1$Aboriginal.Perc), "Aboriginal.Perc"] <- min(data1$Aboriginal.Perc, na.rm = T)
data1[is.na(data1$NotanAustraliancitizen.Perc), "NotanAustraliancitizen.Perc"] <- min(data1$NotanAustraliancitizen.Perc, na.rm = T)

data1 <- data1 %>% filter(Booking.Type == "Booking") %>% summarise(SecularBeliefs.Perc = sum(SecularBeliefs.Perc, na.rm = T),
                                                                                     Islam.Perc= sum(Islam.Perc, na.rm = T),
                                                                                     Hinduism.Perc= sum(Hinduism.Perc, na.rm = T),
                                                                                     Judaism.Perc= sum(Judaism.Perc, na.rm = T),
                                                                                     Christianity.Perc= sum(Christianity.Perc, na.rm = T),
                                                                                     Buddhism.Perc= sum(Buddhism.Perc, na.rm = T),
                                                                                     OtherReligions.Perc= sum(OtherReligions.Perc, na.rm = T),
                                                                                     Aboriginal.Perc= sum(Aboriginal.Perc, na.rm = T),
                                                                                     NotanAustraliancitizen.Perc= sum(NotanAustraliancitizen.Perc, na.rm = T)
)
#View(data1)
#data1 <- data1/data1$count[1]
data2 <-t(data1)
op <- par(mar = c(12,4,4,2) + 0.2)
bc<- barplot(data2[,1], axisnames = FALSE) 
axis(1, at=bc, labels=rownames(data2), tick=FALSE,  las=2, line=-0.5)#, cex.axis=0.5 
par(op)
#--------------------------------------------------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------------------------------------------------
# plot bookings by types of bookings
#--------------------------------------------------------------------------------------------------------------------------------------------
data1 <- bookings.made.v  %>% group_by(Booking.Type, After6Pm) %>% summarise(Total.Booking.Duration = sum(Booking.Duration))
data1[data1$After6Pm == 0,  "After6Pm"] <- "Before6pm"
data1[data1$After6Pm == 1,  "After6Pm"] <- "After6pm"

ggplot(data = data1, aes(x=  Booking.Type , y =  Total.Booking.Duration)) + geom_bar(stat = "identity", aes(fill = as.factor(After6Pm))) +
  xlab("") +
  theme(legend.title=element_blank())
#--------------------------------------------------------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------------------------------------------------------
# plot bookings by recurrence
#--------------------------------------------------------------------------------------------------------------------------------------------
data1 <- table(bookings.made.members$Booking.Recurrence)
bc<- barplot(data1, xlab = "Booking recurrence") 
text(x=bc, y =  data1 ,label= data1, pos=3, xpd=NA)#

#--------------------------------------------------------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------------------------------------------------------
# plot bookings by month
#--------------------------------------------------------------------------------------------------------------------------------------------
data1 <- bookings.made.v.grouped %>% filter(Booking.Type == "Booking") %>% group_by(Booking.Month) %>% summarise(count = sum(Booking.Duration), 
                                                                                                                 Total.Availablility = mean(Total.Availablility),
                                                                                                                 booking.duration.mean =  mean(Booking.Duration))
ggplot(data = data1, aes(x=  reorder(Booking.Month, booking.duration.mean) , y =  booking.duration.mean)) + geom_bar(stat = "identity", aes(fill = Total.Availablility)) + xlab("") + ylab("Mean booking duration")


#--------------------------------------------------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------------------------------------------------
# plot bookings by day
#--------------------------------------------------------------------------------------------------------------------------------------------
data1 <- bookings.made.v.grouped %>% filter(Booking.Type == "Booking") %>% group_by(Booking.Day) %>% summarise(Total.Booking.Duration = sum(Booking.Duration), Total.Availablility = sum(Total.Availablility))
data1$utilisation <- data1$Total.Booking.Duration*100/data1$Total.Availablility

p2<- ggplot(data = data1, aes(x=  reorder(Booking.Day, Total.Booking.Duration) , y =  Total.Booking.Duration)) + geom_bar(stat = "identity", aes(fill = Total.Availablility)) + xlab("") + ylab("Total booking duration")
p1<- ggplot(data = data1, aes(x=  reorder(Booking.Day, utilisation) , y =  Total.Booking.Duration)) + geom_bar(stat = "identity") + xlab("") + ylab("Utilisation")
library(reshape2)
data1$Total.Booking.Duration <- normBetweenZeroOne(data1$Total.Booking.Duration)
data1$Total.Availablility <- normBetweenZeroOne(data1$Total.Availablility)
data1$utilisation <- normBetweenZeroOne(data1$utilisation)

data1 <- melt(data1)
data1 <- data1[data1$variable!= "Total.Availablility",]
ggplot(data = data1, aes(x=  reorder(Booking.Day, value) , y =  value, fill=variable)) + geom_bar(stat = "identity", position = "dodge") + xlab("") + ylab("Total booking duration")


ggplot(df, aes(experiment, value, fill=metric)) + 
  geom_bar(position="dodge")

par(mfrow = c(1:2))
p2
p1
#test <- cbind(p1,p2) #for barplots only
#barplot(test,beside=T)

# repeat and devide by day/night
data1 <- bookings.made.v %>% filter(Booking.Type == "Booking") %>% group_by(Booking.Day, After6Pm) %>% summarise(count = sum(Booking.Duration))#, Total.Availablility = mean(Total.Availablility)
data1[data1$After6Pm == 0,  "After6Pm"] <- "Before6pm"
data1[data1$After6Pm == 1,  "After6Pm"] <- "After6pm"
p2<- ggplot(data = data1, aes(x=  reorder(Booking.Day, count) , y =  count)) + geom_bar(stat = "identity") + facet_wrap(~data1$After6Pm) + 
  xlab("") + ylab("Total booking duration") 
p2

#--------------------------------------------------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------------------------------------------------
# plot bookings by median age of suburb
#--------------------------------------------------------------------------------------------------------------------------------------------
data1 <- bookings.made.v.grouped  %>% group_by( MedianAge.Persons.years) %>%
  summarise(Utilisation = sum(Utilisation), Total.Bookings = sum(Total.Bookings), Booking.Duration =sum(Booking.Duration), 
            Total.Availablility = sum(Total.Availablility), Total.Unavailablility = sum(Total.Unavailablility))

ggplot(data = data1, aes(x=  MedianAge.Persons.years , y =  Utilisation)) + geom_bar(stat = "identity") +
  xlab("") +
  theme(legend.title=element_blank())

cor.test(data1$MedianAge.Persons.years, data1$Utilisation)

#--------------------------------------------------------------------------------------------------------------------------------------------




ggplot(my.data,aes(x=Session_Code,y=Ticket_Sale_Cnt,fill=factor(Entitlement_Onsale_Tournament)))+
  geom_bar(stat="identity",position="dodge") + scale_fill_discrete(name="Year") + ylab("Ticket Sales") + xlab("RLA Session")  + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_text(aes(label=round(AvgPrice)), vjust=0, angle = 90)



myvif <-function(R){ return (1/(1-R^2))}
R <- seq(0.1, 0.99, 0.05)
plot(y=myvif(R), x=R^2)
abline(a=2.5, b=0)
abline(a=1.8, b=0)


