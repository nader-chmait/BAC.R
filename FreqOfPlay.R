library(eeptools)

booker.details <- read.csv("../../bookings-played-report-with-gender.csv", header = T)
booker.details$Player.First.Name <- as.character(booker.details$Player.First.Name)
booker.details$Player.Last.Name <- as.character(booker.details$Player.Last.Name)
booker.details[is.na(booker.details)]<- 0
booker.details <- booker.details[!duplicated(booker.details),]
booker.details <- booker.details[!duplicated(booker.details[,c("Venue.Name", "Venue.ID","Player.Last.Name", "Player.First.Name")]),]



freq.play <- bookings.played.v[bookings.played.v$Booking.Type != "Closed" &
                                 bookings.played.v$Booking.Type != "Maintenance" & bookings.played.v$Booking.Type != "Coaching" , ]
freq.play$Year.OfBooking <-  year(freq.play$Booking.Date) 
freq.play$Booking.Day.Number <-  day(freq.play$Booking.Date) 
freq.play$Booking.Month.Number <-  month(freq.play$Booking.Date) 
freq.play$Booking.Week.Number <-  week(freq.play$Booking.Date) 


freq.play <- merge(freq.play, booker.details, all.x = T)

# freq.play <- freq.play %>% group_by(Year.OfBooking, Booking.Week.Number, Player.First.Name, Player.Last.Name, Player.DOB,
#                                     Player.Gender) %>%
#   summarise(n=n())
#freq.play<- freq.play[freq.play$n <4, ]
# freq.play <- freq.play %>% group_by(Player.First.Name, Player.Last.Name, Player.DOB, Player.Gender) %>%
#   summarise(weekly.play.freq = mean(n))

# freq.play.yearly <- freq.play %>% group_by(Year.OfBooking, Player.First.Name, Player.Last.Name, Player.DOB, Player.Gender) %>%
#   summarise(weekly.play.freq=n()/52) 
# freq.play.monthly <- freq.play %>% group_by(Year.OfBooking, Booking.Month, Player.First.Name, Player.Last.Name, Player.DOB, Player.Gender) %>%
#   summarise(weekly.play.freq=n()/12)
# freq.play.weekly <- freq.play %>% group_by(Year.OfBooking,  Booking.Week.Number, Player.First.Name, Player.Last.Name, Player.DOB, Player.Gender) %>%
#   summarise(weekly.play.freq=n())  #%>%
#   #complete(Year.OfBooking, Booking.Week.Number, Player.First.Name, Player.Last.Name, Player.DOB, Player.Gender, fill = list(weekly.play.freq = 0))

freq.play.weekly.2016 <- freq.play %>%  filter(Year.OfBooking ==2016) %>% group_by(Booking.Week.Number, Player.First.Name, Player.Last.Name, Player.DOB, Player.Gender) %>%
  summarise(weekly.play.freq=n()) 

players.2016 <- unique(freq.play.weekly.2016[,c("Player.First.Name", "Player.Last.Name", "Player.DOB", "Player.Gender")])
players.2016<- players.2016[!is.na(players.2016$Player.Last.Name) & players.2016$Player.Last.Name != "Coach",]
players.2016.weekly <- players.2016[rep(seq_len(nrow(players.2016)), each=52),]
players.2016.weekly <- players.2016.weekly %>% group_by(Player.First.Name, Player.Last.Name, Player.DOB, Player.Gender) %>% 
  mutate(Booking.Week.Number = seq(1:52)) %>% ungroup()



freq.play.weekly.2016.all <- merge(freq.play.weekly.2016, players.2016.weekly, all.y = T)
freq.play.weekly.2016.all[is.na(freq.play.weekly.2016.all$weekly.play.freq), "weekly.play.freq"] <- 0

mean.freq.play.weekly.2016 <- freq.play.weekly.2016.all %>%  group_by(Booking.Week.Number, Player.First.Name, Player.Last.Name, Player.DOB, Player.Gender) %>%
  summarise(weekly.play.freq=mean(weekly.play.freq)) 
mean.freq.play.weekly.2016 <- mean.freq.play.weekly.2016[mean.freq.play.weekly.2016$weekly.play.freq <5, ]


mean(mean.freq.play.weekly.2016$weekly.play.freq)

# df <- freq.play.weekly %>%
#   xtabs(formula =  ~ Booking.Week.Number + Year.OfBooking) %>%
#   as.data.frame()

freq.play.weekly.2017 <- freq.play %>%  filter(Year.OfBooking ==2017) %>% group_by(Booking.Week.Number, Player.First.Name, Player.Last.Name, Player.DOB, Player.Gender) %>%
  summarise(weekly.play.freq=n()) 

players.2017 <- unique(freq.play.weekly.2017[,c("Player.First.Name", "Player.Last.Name", "Player.DOB", "Player.Gender")])
players.2017<- players.2017[!is.na(players.2017$Player.Last.Name) & players.2017$Player.Last.Name != "Coach",]
players.2017.weekly <- players.2017[rep(seq_len(nrow(players.2017)), each=52),]
players.2017.weekly <- players.2017.weekly %>% group_by(Player.First.Name, Player.Last.Name, Player.DOB, Player.Gender) %>% 
  mutate(Booking.Week.Number = seq(1:52)) %>% ungroup()


freq.play.weekly.2017.all <- merge(freq.play.weekly.2017, players.2017.weekly, all.y = T)
freq.play.weekly.2017.all[is.na(freq.play.weekly.2017.all$weekly.play.freq), "weekly.play.freq"] <- 0

mean.freq.play.weekly.2017 <- freq.play.weekly.2017.all %>%  group_by(Booking.Week.Number, Player.First.Name, Player.Last.Name, Player.DOB, Player.Gender) %>%
  summarise(weekly.play.freq=mean(weekly.play.freq)) 
mean.freq.play.weekly.2017 <- mean.freq.play.weekly.2017[mean.freq.play.weekly.2017$weekly.play.freq <5, ]


mean(mean.freq.play.weekly.2017$weekly.play.freq)

mean.freq.play.weekly <- rbind(mean.freq.play.weekly.2016, mean.freq.play.weekly.2017)
mean(mean.freq.play.weekly$weekly.play.freq)
# freq.play.weekly$weekly.play.freq <- as.numeric(freq.play.weekly$weekly.play.freq)
# freq.play <- rbind(as.data.frame(freq.play.weekly[,-c(2)]), as.data.frame(freq.play.monthly[,-c(2)]))
# freq.play <- rbind(as.data.frame(freq.play), as.data.frame(freq.play.yearly))
#   
# mean(freq.play.monthly$weekly.play.freq)
# median(freq.play.monthly$weekly.play.freq)
# mean(freq.play$weekly.play.freq)
# 
# mean(freq.play$weekly.play.freq)
# 
# freq.play <- freq.play.monthly
#--------------------------------------------------------------------------------------------------------------------------------------------
#By gender
#--------------------------------------------------------------------------------------------------------------------------------------------

freq.play.male <- mean.freq.play.weekly[mean.freq.play.weekly$Player.Gender == "Male" , ]
freq.play.female <- mean.freq.play.weekly[mean.freq.play.weekly$Player.Gender == "Female" , ]

male.mean.freq.play<-  mean(freq.play.male$weekly.play.freq, na.rm = T)
female.mean.freq.play<-  mean(freq.play.female$weekly.play.freq, na.rm = T)
pie(c(male.mean.freq.play, female.mean.freq.play), labels = c(paste("male - ", round(male.mean.freq.play,3)) , 
                                                              paste("female - ", round(female.mean.freq.play,3))),
    main ="Average weekly frequency of booking")

x<- data.frame(Gender= c("Male", "Female"), Freq= c(male.mean.freq.play, female.mean.freq.play))
plot(x, main ="Average weekly frequency of booking")

mean(freq.play.male$weekly.play.freq, na.rm = T)
mean(freq.play.female$weekly.play.freq, na.rm = T)

#--------------------------------------------------------------------------------------------------------------------------------------------
#By age group
#--------------------------------------------------------------------------------------------------------------------------------------------


freq.play.age<- mean.freq.play.weekly
freq.play.age$DOB <- as.Date(freq.play.age$Player.DOB, format = "%d/%m/%Y")
freq.play.age<- freq.play.age[!is.na(freq.play.age$DOB) & year(freq.play.age$DOB) < 2012, ] 
freq.play.age$Age <- floor(age_calc(freq.play.age$DOB, units = "years"))
freq.play.age$Age.Group <- cut(freq.play.age$Age, breaks = c(0, 12,  17, 29, 49, 70, 88), include.lowest = T)

freq.play.age <- freq.play.age %>% group_by(Age.Group) %>% summarise(weekly.play.freq = mean(weekly.play.freq))
freq.play.age <- freq.play.age[!is.na(freq.play.age$Age.Group),]
#pie(freq.play.age$weekly.play.freq, labels = paste(freq.play.age$Age.Group, round(freq.play.age$weekly.play.freq, 2), sep = "-"))

bc<- barplot(freq.play.age$weekly.play.freq, horiz = F, main = "Average weekly frequency of booking by age group", xlab = "Age group")#, ylim=c(1,1.4)
text(x=bc, y =  freq.play.age$weekly.play.freq ,label= round(freq.play.age$weekly.play.freq, 2), pos=1)#
axis(1, at=bc, labels=freq.play.age$Age.Group, tick=TRUE,  las=0, line=0)#, cex.axis=0.5 

#--------------------------------------------------------------------------------------------------------------------------------------------
#By membership
#--------------------------------------------------------------------------------------------------------------------------------------------
freq.play.member <- mean.freq.play.weekly
freq.play.member$In.Members <- "Non-Members"
freq.play.member<- as.data.frame(freq.play.member)
in.members <- which(do.call(paste0, freq.play.member[,c("Player.Last.Name", "Player.First.Name", "Player.DOB", "Player.Gender")]) %in% 
                      do.call(paste0, members[,c("Last.Name", "First.Name", "Date.of.Birth", "Gender")])
)
freq.play.member[in.members, "In.Members"] <- "Members"

freq.play.member <- freq.play.member %>% group_by(In.Members) %>%
  summarise(mean.weekly.play.freq = mean(weekly.play.freq))


freq.play.member

bc<- barplot(freq.play.member$mean.weekly.play.freq, horiz = F, main = "Average weekly frequency of booking")#
text(x=bc, y =  freq.play.member$mean.weekly.play.freq ,label= round(freq.play.member$mean.weekly.play.freq,3), pos=1)#
axis(1, at=bc, labels=freq.play.member$In.Members, tick=TRUE,  las=0, line=0)#, cex.axis=0.5 


 

