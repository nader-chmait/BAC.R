
all.members <- read.csv("../../Member_Report.csv", header = T)
facility.names <- facilities[,c("Club.Name", "Club.Facility.Name")]
facility.names <- facility.names[!duplicated(facility.names),]

facility.names$Club.Name <- as.character(facility.names$Club.Name)
facility.names$Club.Name[facility.names$Club.Name==""] <- as.character(facility.names$Club.Facility.Name[facility.names$Club.Name==""])
facility.names$Club.Name[is.na(facility.names$Club.Name)] <- as.character(facility.names$Club.Facility.Name[is.na(facility.names$Club.Name)])

facility.names$Club.Name <- standardise.club.names(facility.names$Club.Name)

members <- all.members[ , c("Club.Name", "First.Name", "Last.Name", "Date.of.Birth", "Gender",
                        "Mailing.Suburb", "Mailing.State", "Updated.On", "Membership.Payment.Date")]

members$Club.Name <- standardise.club.names(members$Club.Name)

#unique(members[!(((members$Club.Name)) %in% unique((facility.names$Club.Facility.Name))), "Club.Name"])

members <- merge(members, facility.names)
members$Updated.On <- as.Date(members$Updated.On, "%d/%m/%Y")
members$Membership.Payment.Date <- as.Date(members$Membership.Payment.Date, "%d/%m/%Y")
members$First.Name <- as.character(members$First.Name)
members$Last.Name <- as.character(members$Last.Name)

members <- members %>% group_by(Club.Facility.Name, First.Name, Last.Name, Date.of.Birth, Gender) %>% #, Date.of.Birth, Gender
  summarise(Membership.Updated.On = min(Updated.On), First.Membership.Date = min(Membership.Payment.Date)
            )
members <- as.data.frame(members)

booker.details <- read.csv("../../bookings-played-report-with-gender.csv", header = T)
#booker.details <- booker.details[,-which(names(booker.details) == "Venue.ID")]
names(booker.details) %in% names(bookings.made)
booker.details$Player.First.Name <- as.character(booker.details$Player.First.Name)
booker.details$Player.Last.Name <- as.character(booker.details$Player.Last.Name)
booker.details[is.na(booker.details)]<- 0
booker.details <- booker.details[!duplicated(booker.details),]
booker.details <- booker.details[!duplicated(booker.details[,c("Venue.Name", "Venue.ID","Player.Last.Name", "Player.First.Name")]),]
#booker.details$Player.DOB <- as.character(booker.details$Player.DOB)
#members$Date.of.Birth <- as.Date(members$Date.of.Birth)
#booker.details$Player.Gender <- as.character(booker.details$Player.Gender)
#members$Gender <- as.character(members$Gender)
#members$Club.Facility.Name <- as.factor(members$Club.Facility.Name)

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
#nrow(members.changed.club)#706 transitions
#View(table(members.changed.club$Venue.Name))
#write.csv(members.changed.club, "../../member.transitions.list.csv")

