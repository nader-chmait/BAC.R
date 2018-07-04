library(lattice)
all.members <- read.csv("Member_Report.csv", header = T)
facility.names <- facilities[,c("Club.Name", "Club.Facility.Name")]
facility.names <- facility.names[!duplicated(facility.names),]

members <- all.members[ , c("Club.Name", "First.Name", "Last.Name", "Date.of.Birth", "Gender",
                        "Mailing.Suburb", "Mailing.State", "Updated.On", "Membership.Payment.Date")]

#notinmembers <- which(members$Club.Name %in% facilities$Club.Facility.Name)#which()
#table(notinmembers)
#View(members[-notinmembers, ])
members <- merge(members, facility.names)
members$Updated.On <- as.Date(members$Updated.On, "%d/%m/%Y")
members$Membership.Payment.Date <- as.Date(members$Membership.Payment.Date, "%d/%m/%Y")
members$First.Name <- as.character(members$First.Name)
members$Last.Name <- as.character(members$Last.Name)
#members$Full.Name = paste(members$First.Name, members$Last.Name, sep = " ")
                         
members <- members %>% group_by(Club.Facility.Name, First.Name, Last.Name, Date.of.Birth, Gender) %>% #, Date.of.Birth, Gender
  summarise(Membership.Updated.On = min(Updated.On), First.Membership.Date = min(Membership.Payment.Date)
            )
members <- as.data.frame(members)
#members2 <- members %>% group_by(Club.Facility.Name, Full.Name, First.Name, Last.Name,  Date.of.Birth, Gender) %>%
#  summarise(n = n())
#members$Club.Facility.Name <- trimws(members$Club.Facility.Name)s
#members$First.Name <- trimws(members$First.Name)
#members$Last.Name <- trimws(members$Last.Name)
#nrow(members)
booker.details <- read.csv("bookings-played-report-with-gender.csv", header = T)
#booker.details <- booker.details[,-which(names(booker.details) == "Venue.ID")]
names(booker.details) %in% names(bookings.made)
booker.details$Player.First.Name <- as.character(booker.details$Player.First.Name)
booker.details$Player.Last.Name <- as.character(booker.details$Player.Last.Name)
booker.details[is.na(booker.details)]<- 0
booker.details <- booker.details[!duplicated(booker.details),]
booker.details <- booker.details[!duplicated(booker.details[,c("Venue.Name", "Venue.ID","Player.Last.Name", "Player.First.Name")]),]
#booker.details$Player.DOB <- as.character(booker.details$Player.DOB)
#members$Date.of.Birth <- as.Date(members$Date.of.Birth)
# booker.details$Player.Gender <- as.character(booker.details$Player.Gender)
# members$Gender <- as.character(members$Gender)
# members$Club.Facility.Name <- as.factor(members$Club.Facility.Name)

bookings.made.members <- bookings.made.v
  
bookings.made.members<- merge(bookings.made.members, booker.details, all.x=T) #, all.x=T
bookings.made.members$In.Members <- 0
bookings.made.members<- as.data.frame(bookings.made.members)
in.members <- which(do.call(paste0, bookings.made.members[,c("Venue.Name","Player.Last.Name", "Player.First.Name", "Player.DOB", "Player.Gender")]) %in% 
                      do.call(paste0, members[,c("Club.Facility.Name", "Last.Name", "First.Name", "Date.of.Birth", "Gender")])
                    )
bookings.made.members[in.members, "In.Members"] <- 1
#nrow(bookings.made.members)
bookings.made.members<- merge(bookings.made.members, members, by.x = c("Venue.Name", "Player.Last.Name", "Player.First.Name", "Player.DOB", "Player.Gender"),
                              by.y = c("Club.Facility.Name", "Last.Name", "First.Name", "Date.of.Birth", "Gender"),
                              all.x = T)
#nrow(bookings.made.members)
bookings.made.members <- bookings.made.members %>% group_by(Venue.Name, Player.Last.Name, Player.First.Name, Player.DOB, Player.Gender) %>%
  mutate(total.bookings.made = n()) %>% ungroup()
bookings.made.members <- bookings.made.members %>% filter(total.bookings.made > 1)
bookings.made.members <- as.data.frame(bookings.made.members)

bookings.made.members$Is.Member <- "Member"
bookings.made.members$Membership.Changed <- 0
bookings.made.members[is.na(bookings.made.members$First.Membership.Date), "Is.Member"] <- "Non-Member"
bookings.made.members[!is.na(bookings.made.members$First.Membership.Date) & 
                        (bookings.made.members$Booking.Date < bookings.made.members$First.Membership.Date), "Is.Member"] <- "Non-Member"
bookings.made.members[!is.na(bookings.made.members$First.Membership.Date) & 
                        (bookings.made.members$Booking.Date < bookings.made.members$First.Membership.Date ), "Membership.Changed"] <- 1


#table(bookings.made.members$Is.Member)

bookings.made.members.grouped <- bookings.made.members %>% group_by(Player.First.Name, Player.Last.Name, Is.Member, Player.DOB, Player.Gender,
                                                            Membership.Changed, Venue.Name, 
                                Facility.Name, Geographical.classification, Populationdensity.ERPat30June.persons.km2,
                                Medianequivalisedtotalhouseholdincome.weekly.AUD) %>%
  summarise(Total.Booking.Duration= sum(Booking.Duration), Total.Bookings = n(), 
            First.Booking.Date = min(Booking.Date), First.Membership.Date=min(First.Membership.Date))


bookings.made.members.grouped <- bookings.made.members.grouped %>% group_by(Player.First.Name, 
                                                                            Player.Last.Name, Player.DOB, Player.Gender, 
                                                                            Venue.Name, Facility.Name,
                                Geographical.classification, Populationdensity.ERPat30June.persons.km2,
                                Medianequivalisedtotalhouseholdincome.weekly.AUD) %>%
  mutate(Membership.Changed = max(Membership.Changed)) %>% ungroup() #, Membership.Index = seq(n())

#table(members$Membership.Changed)
#table(members$Is.Member)

members.changed <- bookings.made.members.grouped %>% filter(Membership.Changed >0) 
members.changed <-  members.changed %>% group_by(Player.First.Name, Player.Last.Name, Player.DOB, Player.Gender, Venue.Name, Facility.Name, 
                                                 Geographical.classification, Populationdensity.ERPat30June.persons.km2,
                                                 Medianequivalisedtotalhouseholdincome.weekly.AUD) %>%
  mutate(Transition.Time =  First.Membership.Date -  First.Booking.Date, Appears = n()
         ) %>% ungroup()

members.changed$Transition.Index <- 1
members.changed[members.changed$Transition.Time <0, "Transition.Index"] <- -1
members.changed <-  members.changed %>% group_by(Player.First.Name, Player.Last.Name, Player.DOB, Player.Gender, Venue.Name, Facility.Name, 
                                                 Geographical.classification, Populationdensity.ERPat30June.persons.km2,
                                                 Medianequivalisedtotalhouseholdincome.weekly.AUD) %>%
  mutate(Dropped.Membership =  sum(Transition.Index) <0)  %>% ungroup()

  #mutate(Transition.Time = as.numeric(-1*(Membership.Status.Date - lag(Membership.Status.Date)))) %>% ungroup()

#members.changed$Dropped.Membership <- FALSE
#members.changed$Dropped.Membership <- ( members.changed$Transition.Time < 0) & ( members.changed$appears <2)
table(members.changed$Dropped.Membership)

# which clubs had most transitions
members.changed.club <-  members.changed[members.changed$Transition.Time> 0,c("Player.First.Name", "Player.Last.Name", "Player.DOB", "Venue.Name", 
                                                                              "First.Membership.Date", "Transition.Time")]
members.changed.club<- members.changed.club[!duplicated(members.changed.club),] 
nrow(members.changed.club)#706 transitions
View(table(members.changed.club$Venue.Name))
write.csv(members.changed.club, "member.transitions.list.csv")

#H:That those people who transition from casual booker to club member at a club do so after x number of bookings / $ spent
summary(members.changed$Populationdensity.ERPat30June.persons.km2) #, Populationdensity.ERPat30June.persons.km2>10000
t<- members.changed %>% filter(Transition.Time > 0) %>% summarise(median.plays.before.transition = median(Total.Bookings),
                                                              avg.plays.before.transition = mean(Total.Bookings),
                                                              median.days.before.transition = median(Transition.Time),
                                                              mean.days.before.transition = mean(Transition.Time))



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

DistFromCBD <- read.csv("DistFromCBD.csv", header = T)
DistFromCBD$DistFromCBD <- as.integer(DistFromCBD$DistFromCBD)
DistFromCBD$RegionLocation <- as.character("Remote area")
DistFromCBD$RegionLocation <- as.character(DistFromCBD$RegionLocation)
DistFromCBD[DistFromCBD$DistFromCBD<= 5 , "RegionLocation"] <- "Inner City"
DistFromCBD[DistFromCBD$DistFromCBD>= 6  & DistFromCBD$DistFromCBD< 11 , "RegionLocation"] <- "City"
DistFromCBD[DistFromCBD$DistFromCBD>= 11 & DistFromCBD$DistFromCBD< 21 , "RegionLocation"] <- "City Border"
DistFromCBD[DistFromCBD$DistFromCBD>= 21 & DistFromCBD$DistFromCBD< 40 , "RegionLocation"] <- "Suburb"
DistFromCBD[DistFromCBD$DistFromCBD>= 41 & DistFromCBD$DistFromCBD< 80 , "RegionLocation"] <- "Outer Suburb"

turned.to.members <- merge(turned.to.members, DistFromCBD)
View(table(turned.to.members[,c("RegionLocation")]))
#write.csv(unique(bookings.made.v[bookings.made.v$Venue.Name %in% unique(turned.to.members$Venue.Name),
#                     c("ClubsInSuburb", "Facility.Name", "Venue.Name", "State", "Populationdensity.ERPat30June.persons.km2")]), "DistFromCBD") 
#regional.location <- data.frame(Venue.Name = unique(turned.to.members$Venue.Name),
#                                Distance.From.CBD.Km = c(60, 5, 15.2, 9.2, 17.5, 22.2, 6.3, 33.2, 58.7, 3.2, 31.7, 36.2, 5.6,
#                                                         15, 45.6, 73.2, 14, 7.2))
#
#
#plot(turned.to.members$Distance.From.CBD.Km, turned.to.members$Transition.Time)

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

