library(dplyr)
library(stringi)
library(tidyr)
library(lubridate)

proper <- function(x) {
  stri_trans_totitle(x)
}

bookings.played <- read.csv("bookings-played-report-2016-05-30-2018-05-30.csv", header = T)
bookings.made <- read.csv("bookings-made-report-2016-05-30-2018-05-30.csv", header = T)
cancellations <- read.csv("cancellations-report-2018-01-01-2018-06-30.csv", header = T)
contacts <- read.csv("contacts-report-2018-06-01-2018-06-30.csv", header = T)

#summary(bookings.played)
#summary(bookings.made)
#summary(contacts)


cancelled<- (do.call(paste0, bookings.made[,c("Venue.ID", "Booking.ID", "Booking.Date")]) %in% do.call(paste0, cancellations[,c("Venue.ID", "Booking.ID", "Booking.Date")]))
cancelled<- which(cancelled == TRUE)
bookings.made <- bookings.made[-cancelled,]

cancelled.played <- (do.call(paste0, bookings.played[,c("Venue.ID", "Booking.ID", "Booking.Date")]) %in% do.call(paste0, cancellations[,c("Venue.ID", "Booking.ID", "Booking.Date")]))
cancelled.played<- which(cancelled.played == TRUE)
bookings.made <- bookings.made[-cancelled.played,]



bookings.made$Booking.Date <- as.Date(bookings.made$Booking.Date, "%d/%m/%Y")
bookings.made <- bookings.made[bookings.made$Booking.Date < "2018-06-01", ]
bookings.played$Booking.Date <- as.Date(bookings.played$Booking.Date, "%d/%m/%Y")
bookings.made$Booking.Time <- as.character(bookings.made$Booking.Time)
bookings.played$Booking.Time <- as.character(bookings.played$Booking.Time)

bookings.played$Booker.Venue.Role <- as.character(bookings.played$Booker.Venue.Role)
bookings.played$Booker.First.Name <- as.character(bookings.played$Booker.First.Name)
bookings.played$Booker.Last.Name <- as.character(bookings.played$Booker.Last.Name)
bookings.played$Player.First.Name <- as.character(bookings.played$Player.First.Name)
bookings.played$Player.Last.Name <- as.character(bookings.played$Player.Last.Name)
#bookings.played$Player.Venue.Role <- as.character(bookings.played$Player.Venue.Role)

bookings.made$Booker.Venue.Role <- as.character(bookings.made$Booker.Venue.Role)
bookings.made$Booker.First.Name <- as.character(bookings.made$Booker.First.Name)
bookings.made$Booker.Last.Name <- as.character(bookings.made$Booker.Last.Name)
bookings.made$Player.First.Name <- as.character(bookings.made$Player.First.Name)
bookings.made$Player.Last.Name <- as.character(bookings.made$Player.Last.Name)
#bookings.made$Player.Venue.Role <- as.character(bookings.made$Player.Venue.Role)

bookings.made  <- bookings.made[grepl("Demo ", bookings.made$Venue.Name, ignore.case = T) == 0, ]
bookings.played <- bookings.played[grepl("Demo ", bookings.played$Venue.Name, ignore.case = T)== 0, ]
bookings.made  <- bookings.made[grepl("Sportlabs", bookings.made$Venue.Name, ignore.case = T) == 0, ]
bookings.played <- bookings.played[grepl("Sportlabs", bookings.played$Venue.Name, ignore.case = T)== 0, ]

bookings.made <- bookings.made[!duplicated(bookings.made[,-c(which(names(bookings.made)=="Booking.ID"))]),]
bookings.played <- bookings.played[!duplicated(bookings.played[,-c(which(names(bookings.played)=="Booking.ID"))]),]
#-------------------------------------------------------------------------------------------------------------------------------
#Regrion data extracted from Bureau of Statistics website : http://stat.data.abs.gov.au/Index.aspx?QueryId=920
#http://stat.abs.gov.au/itt/r.jsp?databyregion#/
#-------------------------------------------------------------------------------------------------------------------------------
region.stats.2 <- read.csv("ABS_REGIONAL_ASGS2016_08062018161204286.population.csv", header = T)
region.stats.2 <- region.stats.2[!duplicated(region.stats.2[,c("Data.item", "REGIONTYPE", "Region", "ASGS_2016")]), ]
region.stats.2 <- region.stats.2[region.stats.2$TIME == 2016, ]
region.stats.2 <- region.stats.2[,-c(1,7,8)]
#region.stats.2 <- region.stats.2[!duplicated(region.stats.2[,c("Data.item", "REGIONTYPE", "Region", "ASGS_2016")]),]
#View(region.stats.2[duplicated(region.stats.2),])

region.stats <-  spread(region.stats.2, Data.item, Value)  
names(region.stats) <- gsub(" \\(\\%\\)", ".Perc", names(region.stats))
names(region.stats) <- gsub(", Other Spiritual Beliefs and No Religious Affiliation.Perc", ".Perc", names(region.stats))
names(region.stats) <- gsub(" and Torres Strait Islander Peoples - Proportion of total population.Perc", ".Perc", names(region.stats))
names(region.stats) <- gsub(" ", "", names(region.stats))
names(region.stats) <- gsub("\\)", "", names(region.stats))
names(region.stats) <- gsub("\\(", ".", names(region.stats))
names(region.stats) <- gsub("-", ".", names(region.stats))
names(region.stats) <- gsub("\\$", "AUD", names(region.stats))
names(region.stats) <- gsub("\\/", ".", names(region.stats))

region.stats$StateIniCode <- substr(as.character(region.stats$ASGS_2016), 1, 1)
state.codes <- data_frame(StateIniCode = c(0:9), State = c("Australia", "NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT", "Other"))
state.codes <- data_frame(StateIniCode = c(0:9), State = c("Australia", "NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT", "Other"))
#rm(region.stats.2)
region.stats <- merge(region.stats, state.codes)
#region.stats <- region.stats[region.stats$Region== "Willoughby - Castle Cove - Northbridge",]


region.stats$Region <- as.character(region.stats$Region)
region.stats$Region <- gsub("- .*", "", region.stats$Region, ignore.case = T) 
region.stats$Region <- gsub("\\(.*", "", region.stats$Region, ignore.case = T) 
region.stats$Region <- trimws(region.stats$Region)
region.stats$LandArea.Km2 <- 0.01*as.numeric(region.stats$LandArea.Ha)
region.stats$Density <- region.stats$Persons.Total.no./region.stats$LandArea.Km2
region.stats$Density <- round(region.stats$Density, 1)
  
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Ardeer" & region.stats$State == "VIC", ] 
region.stats[nrow(region.stats), "Region"] <- "Albion"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Lorne" & region.stats$State == "VIC", ] 
region.stats[nrow(region.stats), "Region"] <- "Anglesea"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Aspendale Gardens" & region.stats$State == "VIC", ] 
region.stats[nrow(region.stats), "Region"] <- "Aspendale"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Rosebud" & region.stats$State == "VIC", ] 
region.stats[nrow(region.stats), "Region"] <- "Boneo"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Wambo" & region.stats$State == "QLD", ] 
region.stats[nrow(region.stats), "Region"] <- "Dalby"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Nedlands" & region.stats$State == "WA", ] 
region.stats[nrow(region.stats), "Region"] <- "Dalkeith"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Queanbeyan West" & region.stats$State == "NSW", ] 
region.stats[nrow(region.stats), "Region"] <- "Jerrabomberra"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "East Victoria Park" & region.stats$State == "WA", ] 
region.stats[nrow(region.stats), "Region"] <- "Burswood"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Phillip Island" & region.stats$State == "VIC", ] 
region.stats[nrow(region.stats), "Region"] <- "Cowes"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Keilor" & region.stats$ASGS_2016 ==  "210011228" & region.stats$State == "VIC", ] 
region.stats[nrow(region.stats), "Region"] <- "Keilor Park"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Port Macquarie" & region.stats$ASGS_2016 == "108041163" & region.stats$State == "NSW", ] 
region.stats[nrow(region.stats), "Region"] <- "Lake Cathie"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Mount Barker" & region.stats$State == "SA", ] 
region.stats[nrow(region.stats), "Region"] <- "Littlehampton"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "North Ryde" & region.stats$State == "NSW", ] 
region.stats[nrow(region.stats), "Region"] <- "Marsfield"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Beacon Hill" & region.stats$State == "NSW", ] 
region.stats[nrow(region.stats), "Region"] <- "Narraweena"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Townsville City" & region.stats$State == "QLD", ] 
region.stats[nrow(region.stats), "Region"] <- "North Ward"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Brunswick Heads" & region.stats$State == "NSW", ] 
region.stats[nrow(region.stats), "Region"] <- "Ocean Shores"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Mordialloc" & region.stats$State == "VIC", ] 
region.stats[nrow(region.stats), "Region"] <- "Parkdale"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Moyne" & region.stats$ASGS_2016 == "217041478" & region.stats$State == "VIC", ] 
region.stats[nrow(region.stats), "Region"] <- "Port Fairy"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Flinders" & region.stats$State == "VIC", ] 
region.stats[nrow(region.stats), "Region"] <- "Red Hill"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Nelson Bay Peninsula" & region.stats$State == "NSW", ] 
region.stats[nrow(region.stats), "Region"] <- "Soldiers Point"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Parklea" & region.stats$State == "NSW", ] 
region.stats[nrow(region.stats), "Region"] <- "Stanhope Gardens"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Kingswood" & region.stats$State == "NSW", ] 
region.stats[nrow(region.stats), "Region"] <- "Werrington"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Tamborine" & region.stats$State == "QLD", ] 
region.stats[nrow(region.stats), "Region"] <- "North Tamborine"

#-------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------
# Edit facilities
#-------------------------------------------------------------------------------------------------------------------------------
facilities <- read.csv("facilities.csv", header = T)
facilities$Suburb <- proper(facilities$Suburb)
facilities$Club.Facility.Name <- gsub("TC", "Tennis Club",facilities$Club.Name, ignore.case =  T)
facilities[facilities$Facility.Name == "BEAUMARIS LTC" , "Club.Facility.Name"] <- "Beaumaris Lawn Tennis Club" 
facilities[facilities$Facility.Name == "Bendigo Tennis Complex" , "Club.Facility.Name"] <- "Bendigo Tennis Complex" 
facilities[facilities$Facility.Name == "BODLEY STREET TENNIS CENTRE" , "Club.Facility.Name"] <- "BLTC Bodley Street Tennis Courts" 
facilities[facilities$Facility.Name == "BONEO TC" , "Club.Facility.Name"] <- "Boneo Tennis Club"
facilities[facilities$Facility.Name == "Bundaberg & District Junior Tennis Association Inc" , "Club.Facility.Name"] <- "Bundaberg and District Junior Tennis Association" 
facilities[facilities$Facility.Name == "BUNINYONG DISTRICT TA" , "Club.Facility.Name"] <- "Buninyong & District Tennis Association" 
facilities[facilities$Facility.Name == "Burleigh Heads Tennis Club Inc" , "Club.Facility.Name"] <- "Burleigh Heads Tennis Club" 
facilities[facilities$Facility.Name == "Dalby & District Tennis Association Inc" , "Club.Facility.Name"] <- "Dalby and District Tennis Association" 
facilities[facilities$Facility.Name == "DAYLESFORD LTC" , "Club.Facility.Name"] <- "Daylesford Lawn Tennis Club" 
facilities[facilities$Facility.Name == "Freshwater Tennis Club Inc" , "Club.Facility.Name"] <- "Freshwater Tennis Club"
facilities[facilities$Facility.Name == "Hume Tennis and Community Centre" , "Club.Facility.Name"] <- "Hume Tennis & Community Centre" 
facilities[facilities$Facility.Name == "Englands Park Tennis Club Inc" , "Club.Facility.Name"] <- "Jetty Tennis at Englands Park" 
facilities[facilities$Facility.Name == "Stanhope Gardens Tennis Centre" , "Club.Facility.Name"] <- "Jonas Bradley Park" 
facilities[facilities$Facility.Name == "Lambton Park Tennis Club Inc" , "Club.Facility.Name"] <- "Lambton Park Tennis Club" 
facilities[facilities$Facility.Name == "MCC TC" , "Club.Facility.Name"] <- "MCC Roy Street St Kilda"  #????
facilities[facilities$Facility.Name == "Narraweena Tennis Club " , "Club.Facility.Name"] <- "Narraweena Tennis Club" 
facilities[facilities$Facility.Name == "Ocean Shores Tennis Club Inc" , "Club.Facility.Name"] <- "Ocean Shores Tennis Club" 
facilities[facilities$Facility.Name == "Parramatta City Tennis Inc." , "Club.Facility.Name"] <- "Parramatta City Tennis" 
facilities[facilities$Facility.Name == "Redcliffe Tennis Association Inc" , "Club.Facility.Name"] <- "Redcliffe Tennis Association" 
facilities[facilities$Facility.Name == "Redland Bay Tennis Club Inc" , "Club.Facility.Name"] <- "Redland Bay Tennis Club" 
facilities[facilities$Facility.Name == "Redlynch Valley Tennis Club Inc" , "Club.Facility.Name"] <- "Redlynch Valley Tennis Club" 
facilities[facilities$Facility.Name == "Bar Beach Tennis Club Inc." , "Club.Facility.Name"] <- "Reid Park Tennis Courts" 
facilities[facilities$Facility.Name == "Roma & District Tennis Club Inc" , "Club.Facility.Name"] <- "Roma and District Tennis Club" 
facilities[facilities$Facility.Name == "Saltwater Reserve" , "Club.Facility.Name"] <- "Saltwater Tennis Centre" 
facilities[facilities$Facility.Name == "Sorell Tennis Club" , "Club.Facility.Name"] <- "Sorell Tennis Club" 
facilities[facilities$Facility.Name == "South Perth Tennis Club" , "Club.Facility.Name"] <- "South Perth Tennis Centre" 
facilities[facilities$Facility.Name == "SUNBURY Tennis Club" , "Club.Facility.Name"] <- "Sunbury Lawn Tennis Club" 
facilities[facilities$Facility.Name == "Tamborine Mountain Tennis Club Inc" , "Club.Facility.Name"] <- "Tamborine Mountain Tennis Club" 
facilities[facilities$Facility.Name == "SUNBURY Tennis Club" , "Club.Facility.Name"] <- "Sunbury Lawn Tennis Club" 
facilities[facilities$Facility.Name == "Tamborine Mountain Tennis Club Inc" , "Club.Facility.Name"] <- "Tamborine Mountain Tennis Club" 
facilities[facilities$Facility.Name == "Tennis SA (North Adelaide)" & facilities$Club.Name == "Tennis SA", "Club.Facility.Name"] <- "Tennis SA (North Adelaide)" 
facilities[facilities$Facility.Name == "Tennis Townsville Inc" , "State"] <- "QLD" 
facilities[facilities$Facility.Name == "Tennis Townsville Inc" , "Club.Facility.Name"] <- "Tennis Townsville" 
facilities[facilities$Facility.Name == "Tewantin Tennis Club Inc" , "Club.Facility.Name"] <- "Tewantin Tennis Club" 
facilities[facilities$Facility.Name == "WANGARATTA  LTC" , "Club.Facility.Name"] <- "Wangaratta Lawn Tennis Club" 
facilities[facilities$Facility.Name == "Warwick & District Tennis Association Inc" , "Club.Facility.Name"] <- "Warwick and District Tennis Association" 
facilities[facilities$Facility.Name == "Werrington Tennis Courts" , "Club.Facility.Name"] <- "Werrington Tennis Courts" 
facilities[facilities$Facility.Name == "Wesley Uniting Church" , "Club.Facility.Name"] <- "Wesley Uniting Church Tennis Club" 
facilities[facilities$Facility.Name == "Western Suburbs Tennis Club Inc" , "Club.Facility.Name"] <- "Western Suburbs Tennis Club" 

facilities[facilities$Facility.Name=="Loton Park Tennis Club", "Suburb"] <- "Burswood"


#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
venues <- facilities[, c("Country", "State", "Post.Code", "Longitude", "Latitude", "Suburb", "Club.Facility.Name", "Facility.Name",
                         "Total.Full.Count.Grass", "Total.Full.Count.Non.Cushioned.Hard.Court", "Total.Full.Count.Synthetic.Grass",
                         "Total.Full.Count.Clay", "Total.Full.Count.Cushioned.Hard.Court", "Total.Full.Count.Other","Total.Full.Count.Synthetic.Clay",
                         "Hot.Shots.Red.count.All", "Hot.Shots.Orange.Count.All",
                         "Total.Full.Count.All", "Other.Count.All", "Outdoor.Full.Count.All", "Indoor.Full.Count.All", "Lighted.Full.Count.All",
                         "Geographical.classification", "Facility.Type", "Organisation.Type", "Organisation.Status")]
venues$Count <- 1
venues<- venues %>% group_by(Country, State, Post.Code, Club.Facility.Name) %>% mutate(ClubsInSuburb= sum(Count)) %>% ungroup()
venues$recently.advertised <- FALSE
venues[venues$Club.Facility.Name %in% c("Belconnen Tennis Club",
  "Royal Park Tennis Club",
  "Albion Tennis Club",
  "North Park Tennis Club",
  "Kensington Community Recreation Centre",
  "Ainslie Tennis Club",
  "Princes Hill Tennis Club",
  "Carlton Gardens Tennis Club",
  "Red Hill Tennis Club",
  "Weston Creek Tennis Club",
  "Barton Tennis Club",
  "Eastlake Tennis Club",
  "Yarralumla Tennis Club"), "recently.advertised"] <- TRUE

venues$Longitude <- round(venues$Longitude, 0)
venues$Latitude <- round(venues$Latitude, 0)
venues <- venues[!duplicated(venues[ , c("Club.Facility.Name")], fromLast = FALSE), ]  
venues$has.Lights <- venues$Lighted.Full.Count.All >0
venues$has.indoor <- venues$Indoor.Full.Count.All >0
venues$has.outdoor <- venues$Outdoor.Full.Count.All >0
venues$has.grass <- (venues$Total.Full.Count.Grass >0 | venues$Total.Full.Count.Synthetic.Grass >0)
venues$has.clay <- (venues$Total.Full.Count.Clay >0 | venues$Total.Full.Count.Synthetic.Clay >0)
venues$has.hard <- (venues$Total.Full.Count.Cushioned.Hard.Court >0 | venues$Total.Full.Count.Non.Cushioned.Hard.Court >0)
venues$has.hot.shot <- (venues$Hot.Shots.Red.count.All >0 | venues$Hot.Shots.Orange.Count.All >0)
venues$court.options <- as.numeric(venues$has.Lights) + as.numeric(venues$has.indoor) + as.numeric(venues$has.outdoor) +
  as.numeric(venues$has.grass) +as.numeric(venues$has.clay) +as.numeric(venues$has.hard) +as.numeric(venues$has.hot.shot) +as.numeric(venues$Other.Count.All > 0)
venues$nbr.courts <- venues$Total.Full.Count.All + venues$Other.Count.All

venues[venues$Club.Facility.Name== "Wesley Uniting Church Tennis Club", c("Organisation.Type", "Organisation.Status")] <- c("Club", "Disaffiliated") 

# nrow(bookings.made.v)
# nrow(bookings.played.v)
bookings.made.v <- merge(bookings.made, venues, by.x = "Venue.Name" , by.y = "Club.Facility.Name")
bookings.played.v <- merge(bookings.played, venues, by.x = "Venue.Name" , by.y = "Club.Facility.Name")

opening.times <- read.csv("Venue-opening-closing-times.csv", header = T)
bookings.made.v <- merge(bookings.made.v, opening.times, by = c("Venue.Name"))
bookings.played.v <- merge(bookings.played.v, opening.times, by = c("Venue.Name"))

#v <- v[!duplicated(v$Club.Facility.Name), c("Country", "State", "Post.Code", "Longitude", "Latitude","Suburb", "Club.Facility.Name")]
#unique(bookings.made$Venue.Name)[-which(unique(bookings.made$Venue.Name) %in% venues$Club.Facility.Name)]
#write.csv(v, "venues.csv")

bookings.made.v$Suburb <- proper(bookings.made.v$Suburb)
bookings.made.v$Suburb <- trimws(bookings.made.v$Suburb)
bookings.played.v$Suburb <- proper(bookings.played.v$Suburb)
bookings.played.v$Suburb <- trimws(bookings.played.v$Suburb)
region.stats$Region <- proper(region.stats$Region)

# region.stat <- region.stats[region.stats$REGIONTYPE == "SA2" | region.stats$REGIONTYPE == "SA3", ]
# region.stats.unique <- region.stat[order(region.stat$REGIONTYPE), ]
# region.stats.unique <- region.stats.unique[!duplicated(region.stats.unique[,c("Region", "State")]),]
#   
region.stats.unique <- region.stats %>% group_by(Region, State) %>% mutate(index = seq(1:n()), tot.dup = n())
region.stats.unique$na_count <- apply(region.stats.unique, 1, function(x) sum(is.na(x)))
region.stats.unique <- region.stats.unique[order(region.stats.unique$na_count), ]
#region.stats.unique <- region.stats.unique[region.stats.unique$tot.dup == 1 |( region.stats.unique$tot.dup >1 & region.stats.unique$REGIONTYPE == "SA2"), ]
region.stats.unique <- region.stats.unique <- region.stats.unique[!duplicated(region.stats.unique[,c("Region", "State")]),]

bookings.made.v<- merge(bookings.made.v, region.stats.unique, all.x = T, by.x = c("Suburb", "State"), by.y = c("Region", "State"))
bookings.played.v<- merge(bookings.played.v, region.stats.unique, all.x = T, by.x = c("Suburb", "State"), by.y = c("Region", "State"))

#bookings.made.v$Booking.Month <- format.Date(as.Date(bookings.made.v$Booking.Date), "%m") 
bookings.made.v$Booking.Month <- months(bookings.made.v$Booking.Date) 
bookings.made.v$Booking.Weekend <- "Weekday"
bookings.made.v[bookings.made.v$Booking.Day == "Saturday" | bookings.made.v$Booking.Day == "Sunday", "Booking.Weekend"] <- "Weekend"
bookings.made.v[bookings.made.v$Booking.Month %in% c("December", "January", "February"), "Season"] <- "Summer"
bookings.made.v[bookings.made.v$Booking.Month %in% c("March", "April", "May"), "Season"] <- "Fall"
bookings.made.v[bookings.made.v$Booking.Month %in% c("June", "July", "August"), "Season"] <- "Winter"
bookings.made.v[bookings.made.v$Booking.Month %in% c("September", "October", "November"), "Season"] <- "Spring"
bookings.made.v$MaleFemalePerc<- bookings.made.v$Males.Total.no./bookings.made.v$Females.Total.no.
bookings.made.v$booking.Hour <- as.numeric(gsub( ":.*", "", bookings.made.v$Booking.Time))
bookings.made.v$After6Pm <- 0
bookings.made.v[bookings.made.v$booking.Hour >= 18 ,"After6Pm"] <- 1

bookings.made.v$Year.OfBooking <-  year(bookings.made.v$Booking.Date) 
bookings.made.v$Booking.Day.Number <-  day(bookings.made.v$Booking.Date) 
bookings.made.v$Booking.Month.Number <-  month(bookings.made.v$Booking.Date) 
bookings.made.v$Booking.Week.Number <-  ifelse(bookings.made.v$Booking.Day.Number<22, ifelse(bookings.made.v$Booking.Day.Number<15, ifelse(bookings.made.v$Booking.Day.Number<8, 1, 2), 3), 4)



bookings.played.v$Booking.Month <- months(bookings.played.v$Booking.Date) 
bookings.played.v$Booking.Weekend <- "Weekday"
bookings.played.v[bookings.played.v$Booking.Day == "Saturday" | bookings.played.v$Booking.Day == "Sunday", "Booking.Weekend"] <- "Weekend"
bookings.played.v[bookings.played.v$Booking.Month %in% c("December", "January", "February"), "Season"] <- "Summer"
bookings.played.v[bookings.played.v$Booking.Month %in% c("March", "April", "May"), "Season"] <- "Fall"
bookings.played.v[bookings.played.v$Booking.Month %in% c("June", "July", "August"), "Season"] <- "Winter"
bookings.played.v[bookings.played.v$Booking.Month %in% c("September", "October", "November"), "Season"] <- "Spring"

bookings.made.v[bookings.made.v$Organisation.Type== "", "Organisation.Type"] <- "Other Body/Organisation"

# to.delete<- which(bookings.made.v$Venue.Name =="Carlton Gardens Tennis Club" & bookings.made.v$Year.OfBooking == "2017"
#                   & bookings.made.v$Booking.Month =="April" & bookings.made.v$Booking.Day == "Saturday" &
#                     (bookings.made.v$Player.Last.Name %in% c("Bozzo", "Dawson")) & (bookings.made.v$Booking.Duration %in% c(300, 330, 600, 690)))
# 
# 
# bookings.made.v <- bookings.made.v[-to.delete, ]
nrow(bookings.made.v)
bookings.made.v <- as.data.frame(bookings.made.v)
bookings.made.v <- bookings.made.v %>% group_by(Venue.Name, Booking.Date, Booking.Time, Booker.Last.Name, Player.Last.Name, Player.First.Name) %>%
  mutate(max.Booking.Duration = max(Booking.Duration, na.rm = T), dup.index = seq(1:n()), n=n()) %>% ungroup

booking.duplicates <- bookings.made.v[bookings.made.v$n >1,  c("Venue.Name", "Booker.Last.Name", "Booker.First.Name",
                                                               "Player.Last.Name", "Player.First.Name", "Booking.Date", 
                                                               "Booking.Time",  "Booking.ID", "dup.index")]
write.csv(booking.duplicates, "booking.duplicates.csv")
bookings.made.v <- as.data.frame(bookings.made.v)
bookings.made.v<- bookings.made.v[bookings.made.v$dup.index ==1,]
nrow(bookings.made.v)


# booking.duplicates$Is.Duplicate <- 1
# View(booking.duplicates[booking.duplicates$Player.Last.Name=="HOGAN",])
# booking.duplicates$Match <- booking.duplicates$Booking.Duration == booking.duplicates$sum.booking.duration
# str(booking.duplicates)
# table(booking.duplicates$Match)
# #nrow(t)
# bookings.made.v <- merge(bookings.made.v, booking.duplicates, all.x = T)
# bookings.made.v[is.na(bookings.made.v$Is.Duplicate), "Is.Duplicate"] <- 0
# bookings.made.v<- bookings.made.v[bookings.made.v$Is.Duplicate == 0, ]
# #nrow(t)


# discripenciesx<- (do.call(paste0, x[ , c("Suburb", "State")]) %in% do.call(paste0, region.stats[, c("Region", "State")]))
# discripenciesx<- which(discripenciesx == FALSE)
# View(unique(x[discripenciesx, c("Suburb", "State")]))
  