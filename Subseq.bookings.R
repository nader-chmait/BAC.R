library(lubridate)
#H:Those casual bookers who book more than once (repeat bookers) make their second booking x days after their first
subseq.bookings <- bookings.made %>% filter(Booking.Recurrence == "None", Booking.Type == "Booking") 
subseq.bookings <- subseq.bookings  %>% group_by(Player.First.Name, Player.Last.Name, Venue.Name) %>% #Player.My.Tennis.ID, 
  mutate(More.Than.One.Booking = n()) %>% ungroup()
subseq.bookings <- subseq.bookings %>% filter(More.Than.One.Booking > 1)

subseq.bookings <- subseq.bookings %>% group_by(Player.My.Tennis.ID, Player.First.Name, Player.Last.Name, Venue.Name) %>%
  mutate(Days.Since.Last.Booking = (c(0, diff(Booking.Date))))

subseq.bookings <- as.data.frame(subseq.bookings)
subseq.bookings$Days.Since.Last.Booking <- as.integer(subseq.bookings$Days.Since.Last.Booking)
str(subseq.bookings)

median(subseq.bookings[subseq.bookings$Days.Since.Last.Booking > 0, "Days.Since.Last.Booking"])
plot(subseq.bookings$Days.Since.Last.Booking, ylab= "Days since last booked") + abline(h=365, col="red")  
mean(subseq.bookings[subseq.bookings$Days.Since.Last.Booking < 93 & subseq.bookings$Days.Since.Last.Booking > 0, "Days.Since.Last.Booking"])

ggplot(data= subseq.bookings[subseq.bookings$Days.Since.Last.Booking<92,], aes(x=Days.Since.Last.Booking)) + geom_density() 
ggplot(data= subseq.bookings[subseq.bookings$Days.Since.Last.Booking<92,], aes(x=Days.Since.Last.Booking)) +  geom_car()


#H:That (inner-city) clubs have a higher amount of repeat, casual bookings as opposed to initial experience transitioning to Club Membership
unique.subseq.bookers <- subseq.bookings %>% filter(Days.Since.Last.Booking >0) %>% 
  group_by(Player.My.Tennis.ID, Player.First.Name, Player.Last.Name, Player.Member.Status, Venue.Name)%>%
  summarise(Days.Since.Last.Booking = mean(Days.Since.Last.Booking))
table(unique.subseq.bookers$Player.Member.Status)
(table(unique.subseq.bookers$Player.Member.Status)["Non-Member"])*100/sum(table(unique.subseq.bookers$Player.Member.Status))
nrow(turned.to.members)*100/sum(table(unique.subseq.bookers$Player.Member.Status))


#H:Those casual bookers who book more than once (repeat bookers) make their second booking x days after their first
subseq.bookings.by.month <- bookings.made %>% filter(Booking.Recurrence == "None", Booking.Type == "Booking") 
subseq.bookings.by.month$Booking.Month <-  months(subseq.bookings.by.month$Booking.Date) 
subseq.bookings.by.month <- subseq.bookings.by.month  %>% group_by(Player.First.Name, Player.Last.Name, Venue.Name, Booking.Month) %>% #Player.My.Tennis.ID, 
  mutate(More.Than.One.Booking = n()) %>% ungroup()
subseq.bookings.by.month <- subseq.bookings.by.month %>% filter(More.Than.One.Booking > 1)
subseq.bookings.by.month$Year.OfBooking <-  year(subseq.bookings.by.month$Booking.Date) 

subseq.bookings.by.month <- subseq.bookings.by.month %>% group_by(Player.My.Tennis.ID, Player.First.Name, Player.Last.Name, Venue.Name, 
                                                                  Booking.Month, Year.OfBooking) %>%
  mutate(Days.Since.Last.Booking = (c(0, diff(Booking.Date))))

plot(subseq.bookings.by.month$Days.Since.Last.Booking, ylab= "Days since last booked") + abline(h=7, col="red")  

ggplot(data= subseq.bookings.by.month[subseq.bookings.by.month$Booking.Type=="Booking",], aes(x=Days.Since.Last.Booking)) + # x=as.numeric(row.names(subseq.bookings.by.month))
  geom_density() + facet_wrap(~Booking.Month)

