#H:That most casual bookers make their booking shortly before playing (spontaneous bookings)
play.diff <- read.csv("bookings-played-with-booked-times.csv", header = T, fileEncoding="latin1")
book.play.diff <- play.diff %>% filter(Booking.Recurrence == "None", Booking.Type == "Booking")
#str(book.play.diff)

book.play.diff$Booking.Date <- as.Date(book.play.diff$Booking.Date, "%d/%m/%y")
book.play.diff$Booked.Date <- as.Date(book.play.diff$Booked.Date, "%d/%m/%y")
book.play.diff$Played.After <- book.play.diff$Booking.Date - book.play.diff$Booked.Date 
book.play.diff$Played.After <- as.integer(book.play.diff$Played.After )


played.in.six.month <- book.play.diff %>% filter(Played.After < 11 & Played.After > -1)
ggplot(data=played.in.six.month, aes(Played.After)) + geom_bar() + xlab("Played x days after booking")
