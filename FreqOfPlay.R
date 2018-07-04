library(eeptools)

booker.details <- read.csv("bookings-played-report-with-gender.csv", header = T)
booker.details$Player.First.Name <- as.character(booker.details$Player.First.Name)
booker.details$Player.Last.Name <- as.character(booker.details$Player.Last.Name)
booker.details[is.na(booker.details)]<- 0
booker.details <- booker.details[!duplicated(booker.details),]
booker.details <- booker.details[!duplicated(booker.details[,c("Venue.Name", "Venue.ID","Player.Last.Name", "Player.First.Name")]),]



freq.play <- bookings.played.v[bookings.played.v$Booking.Type != "Closed" &
                                 bookings.played.v$Booking.Type != "Maintenance", ]
freq.play$Year.OfBooking <-  year(freq.play$Booking.Date) 
freq.play$Booking.Day.Number <-  day(freq.play$Booking.Date) 
freq.play$Booking.Month.Number <-  month(freq.play$Booking.Date) 
freq.play$Booking.Week.Number <-  week(freq.play$Booking.Date) 


freq.play <- merge(freq.play, booker.details, all.x = T)

freq.play <- freq.play %>% group_by(Year.OfBooking, Year.OfBooking, Booking.Week.Number, Player.First.Name, Player.Last.Name, Player.DOB,
                                    Player.Gender) %>%
  summarise(n=n())

freq.play <- freq.play %>% group_by(Player.First.Name, Player.Last.Name, Player.DOB, Player.Gender) %>%
  summarise(weekly.play.freq = mean(n))


mean(freq.play$weekly.play.freq)

#--------------------------------------------------------------------------------------------------------------------------------------------
#By gender
#--------------------------------------------------------------------------------------------------------------------------------------------

freq.play.male <- freq.play[freq.play$Player.Gender == "Male" , ]
freq.play.female <- freq.play[freq.play$Player.Gender == "Female" , ]

male.mean.freq.play<-  mean(freq.play.male$weekly.play.freq, na.rm = T)
female.mean.freq.play<-  mean(freq.play.female$weekly.play.freq, na.rm = T)
pie(c(male.mean.freq.play, female.mean.freq.play), labels = c(paste("male - ", round(male.mean.freq.play,2)) , 
                                                              paste("female - ", round(female.mean.freq.play,2))),
    main ="Average weekly frequency of play")


median(freq.play.male$weekly.play.freq, na.rm = T)
median(freq.play.female$weekly.play.freq, na.rm = T)

#--------------------------------------------------------------------------------------------------------------------------------------------
#By age group
#--------------------------------------------------------------------------------------------------------------------------------------------


freq.play.age<- freq.play
freq.play.age$DOB <- as.Date(freq.play.age$Player.DOB, format = "%d/%m/%Y")
freq.play.age<- freq.play.age[!is.na(freq.play.age$DOB) & year(freq.play.age$DOB) < 2012, ] 
freq.play.age$Age <- floor(age_calc(freq.play.age$DOB, units = "years"))
freq.play.age$Age.Group <- cut(freq.play.age$Age, breaks = c(0, 12,  17, 29, 49, 70, 88), include.lowest = T)

freq.play.age <- freq.play.age %>% group_by(Age.Group) %>% summarise(weekly.play.freq = mean(weekly.play.freq))
freq.play.age <- freq.play.age[!is.na(freq.play.age$Age.Group),]
#pie(freq.play.age$weekly.play.freq, labels = paste(freq.play.age$Age.Group, round(freq.play.age$weekly.play.freq, 2), sep = "-"))

bc<- barplot(freq.play.age$weekly.play.freq, horiz = F, main = "Average weekly frequency of play by age group", xlab = "Age group")#, ylim=c(1,1.4)
text(x=bc, y =  freq.play.age$weekly.play.freq ,label= round(freq.play.age$weekly.play.freq, 2), pos=1)#
axis(1, at=bc, labels=freq.play.age$Age.Group, tick=TRUE,  las=0, line=0)#, cex.axis=0.5 

#--------------------------------------------------------------------------------------------------------------------------------------------
#By membership
#--------------------------------------------------------------------------------------------------------------------------------------------
freq.play.member <- freq.play
freq.play.member$In.Members <- "Non-Members"
freq.play.member<- as.data.frame(freq.play.member)
in.members <- which(do.call(paste0, freq.play.member[,c("Player.Last.Name", "Player.First.Name", "Player.DOB", "Player.Gender")]) %in% 
                      do.call(paste0, members[,c("Last.Name", "First.Name", "Date.of.Birth", "Gender")])
)
freq.play.member[in.members, "In.Members"] <- "Members"

freq.play.member <- freq.play.member %>% group_by(In.Members) %>%
  summarise(mean.weekly.play.freq = mean(weekly.play.freq))


freq.play.member

bc<- barplot(freq.play.member$mean.weekly.play.freq, horiz = F, main = "Average weekly frequency of play")#
text(x=bc, y =  freq.play.member$mean.weekly.play.freq ,label= round(freq.play.member$mean.weekly.play.freq,3), pos=1)#
axis(1, at=bc, labels=freq.play.member$In.Members, tick=TRUE,  las=0, line=0)#, cex.axis=0.5 


 

