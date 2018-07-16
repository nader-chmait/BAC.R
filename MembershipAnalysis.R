library(lattice)

#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
#H:That those people who transition from casual booker to club member at a club do so after x number of bookings / $ spent
summary(members.changed$Populationdensity.ERPat30June.persons.km2) #, Populationdensity.ERPat30June.persons.km2>10000
t<- members.changed %>% filter(Transition.Time > 0) %>% summarise(median.plays.before.transition = median(Total.Bookings),
                                                                  avg.plays.before.transition = mean(Total.Bookings),
                                                                  median.days.before.transition = median(Transition.Time),
                                                                  mean.days.before.transition = mean(Transition.Time))


t <-  members.changed %>% filter(Transition.Time > 0) %>% group_by(Player.First.Name, Player.Last.Name, Player.DOB, 
                                                                   Player.Gender, Venue.Name, Facility.Name)  %>% summarise(avg.plays.before.transition = mean(Total.Bookings),
                                                                                                                            mean.days.before.transition = mean(Transition.Time))
t <- as.data.frame(t[t$avg.plays.before.transition<quantile(t$avg.plays.before.transition)[4], ])
t$avg.plays <- normBetweenZeroOne(as.integer(t$avg.plays.before.transition))
set.seed(1987)
t$avg.days <- normBetweenZeroOne(as.integer(t$mean.days.before.transition))
#t$avg.plays.before.transition <- scale(t$avg.plays.before.transition)
membersCluster <- kmeans(t[, c("avg.plays", "avg.days")], 3, nstart = 1)
membersCluster$cluster <- as.factor(membersCluster$cluster)
library(ggrepel)
ggplot(t, aes(avg.plays, avg.days, color = membersCluster$cluster, label=Player.Last.Name)) + geom_point() +  geom_text(aes(vjust=-1), check_overlap= T, show.legend = F)
#geom_text_repel(xlim= c(0.35, 1), ylim= c(0.4, 1), arrow = NULL)

# t<- members.changed %>% filter(Transition.Time < 0) %>% summarise(median.plays.before.transition = median(Total.Bookings),
#                                                               avg.plays.before.transition = mean(Total.Bookings),
#                                                               median.days.before.transition = -1*median(Transition.Time),
#                                                               mean.days.before.transition = -1*mean(Transition.Time))

#bc<- barchart(t(t), main = "Member to non-member transitions", horiz = T) 
#bc
# text(x=bc, y = c(1:4) ,label= as.integer(c(1:4)), pos=3, xpd=NA)#
t2 <- as.data.frame(t(t))
t2$V1 <- gsub(" days","", t2$V1)
t2$V1 <- round(as.numeric(as.character(t2$V1)), 1)
bc<- barplot(t2$V1, main = "Transitioned to members", horiz = F) 
bc
text(x=bc, y =  t2$V1 ,label= as.integer(t2$V1), pos=3, xpd=NA)#
axis(1, at=bc, labels=rownames(t2), tick=FALSE,  las=1, line=-0.5)#, cex.axis=0.5 


#H:That outer suburban, Regional, Country clubs transition casual booker into club members in a shorter time frame
turned.to.members <- members.changed %>% filter(Transition.Time > 0)
turned.to.members$Transition.Time <- as.integer(turned.to.members$Transition.Time)
turned.to.members <- as.data.frame(turned.to.members)
str(turned.to.members)
cor.test(turned.to.members$Populationdensity.ERPat30June.persons.km2, turned.to.members$Transition.Time)
turned.to.members$Venue.Name2 <- turned.to.members$Venue.Name
turned.to.members[turned.to.members$Transition.Time <400 ,"Venue.Name2"] <- NA

DistFromCBD <- read.csv("../../DistFromCBD.csv", header = T)
DistFromCBD$DistFromCBD <- as.integer(DistFromCBD$DistFromCBD)
DistFromCBD$RegionLocation <- as.character("Remote area")
DistFromCBD$RegionLocation <- as.character(DistFromCBD$RegionLocation)
DistFromCBD[DistFromCBD$DistFromCBD<= 5 , "RegionLocation"] <- "Inner City"
DistFromCBD[DistFromCBD$DistFromCBD>= 6  & DistFromCBD$DistFromCBD< 11 , "RegionLocation"] <- "City"
DistFromCBD[DistFromCBD$DistFromCBD>= 11 & DistFromCBD$DistFromCBD< 21 , "RegionLocation"] <- "City Border"
DistFromCBD[DistFromCBD$DistFromCBD>= 21 & DistFromCBD$DistFromCBD< 40 , "RegionLocation"] <- "Suburb"
DistFromCBD[DistFromCBD$DistFromCBD>= 41 & DistFromCBD$DistFromCBD< 80 , "RegionLocation"] <- "Outer Suburb"

turned.to.members <- merge(turned.to.members, DistFromCBD)
#View(table(turned.to.members[,c("RegionLocation")]))
#write.csv(unique(bookings.made.v[bookings.made.v$Venue.Name %in% unique(turned.to.members$Venue.Name),
#                     c("ClubsInSuburb", "Facility.Name", "Venue.Name", "State", "Populationdensity.ERPat30June.persons.km2")]), "../../DistFromCBD") 
#regional.location <- data.frame(Venue.Name = unique(turned.to.members$Venue.Name),
#                                Distance.From.CBD.Km = c(60, 5, 15.2, 9.2, 17.5, 22.2, 6.3, 33.2, 58.7, 3.2, 31.7, 36.2, 5.6,
#                                                         15, 45.6, 73.2, 14, 7.2))
#
#
#plot(turned.to.members$Distance.From.CBD.Km, turned.to.members$Transition.Time)

#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------

qplot(turned.to.members$DistFromCBD, Transition.Time, size= Populationdensity.ERPat30June.persons.km2,
      data=turned.to.members) + xlab("Distance from CBD") + 
  guides(size=guide_legend(title="Population Density"))+ 
  geom_text(aes(label=Venue.Name2 ), vjust = -0.8) 

cor.test(turned.to.members$DistFromCBD, turned.to.members$Transition.Time)


turned.to.members.grouped <- turned.to.members %>% group_by(RegionLocation) %>%
  summarise(Transition.Time = median(Transition.Time))
ggplot(turned.to.members.grouped, aes(reorder(RegionLocation, Transition.Time), Transition.Time)) + geom_col() +
  ylab("Median tranisiton time") + xlab("")



# Members by club
data1 <- members.by.age %>% filter(Booking.Type == "Booking") %>% 
  group_by(Venue.Name, Player.First.Name, Player.Last.Name, Player.DOB, Player.Gender) %>% summarise(count = n())
table(data1$Venue.Name)

