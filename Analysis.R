library(ggplot2)
library(car)



#--------------------------------------------------------------------------------------------------------------------------------------------
##H:That venues with higher availability have more casual bookings (Demand is often greater than supply for casual bookers)
#--------------------------------------------------------------------------------------------------------------------------------------------
qplot(Norm.Total.Availablility, Norm.bookings.duration,  col = nbr.courts, data =bookings.made.v.grouped.norm) + 
  xlab("Availabilty (normalised [0,1])") + ylab("Normalised booking duration")+ labs(title = "Venues with higher availability have more casual bookings") 
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



