rm(list = ls())
library(dplyr)
library(stringi)
library(tidyr)
library(lubridate)

proper <- function(x) {
  stri_trans_totitle(x)
}

normBetweenZeroOne <-
  function(x) {
    x <- (x - min(x)) / (max(x) - min(x))
    return(x)
  }

normBetweenMinMax <-
  function(x, a, b) {
    x <- (b-a)*((x - min(x)) /(max(x) - min(x))) + a
    return(x)
  }


bookings.played <- read.csv("../../bookings-played-report-2016-05-30-2018-05-30.csv", header = T)
bookings.made <- read.csv("../../bookings-made-report-2016-05-30-2018-05-30.csv", header = T)
cancellations <- read.csv("../../cancellations-report-2018-01-01-2018-06-30.csv", header = T)
contacts <- read.csv("../../contacts-report-2018-06-01-2018-06-30.csv", header = T)

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
region.stats.2 <- read.csv("../../ABS_REGIONAL_ASGS2016_08062018161204286.population.csv", header = T)
region.stats.2 <- region.stats.2[!duplicated(region.stats.2[,c("Data.item", "REGIONTYPE", "Region", "ASGS_2016")]), ]
region.stats.2 <- region.stats.2[region.stats.2$TIME == 2016, ]
region.stats.2 <- region.stats.2[,-c(1,7,8)]
#region.stats.2 <- region.stats.2[!duplicated(region.stats.2[,c("Data.item", "REGIONTYPE", "Region", "ASGS_2016")]),]
#View(region.stats.2[duplicated(region.stats.2),])

region.stats <-  spread(region.stats.2, Data.item, Value) 
region.stats$Region.Orig <- as.character(region.stats$Region)
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
#region.stats$Region <- gsub("- .*", "", region.stats$Region, ignore.case = T) 
region.stats$Region <- gsub(" - ", " ", region.stats$Region, ignore.case = T) 
region.stats$Region <- gsub("\\(.*", "", region.stats$Region, ignore.case = T) 
region.stats$Region <- trimws(region.stats$Region)
region.stats$LandArea.Km2 <- 0.01*as.numeric(region.stats$LandArea.Ha)
region.stats$Density <- region.stats$Persons.Total.no./region.stats$LandArea.Km2
region.stats$Density <- round(region.stats$Density, 1)

composed.regions <-   region.stats[grepl(" - ", region.stats$Region.Orig)>0,]
composed.regions$Region <- gsub(".*- ", "", composed.regions$Region.Orig, ignore.case = T) 
composed.regions <- composed.regions[composed.regions$Region != "North" & composed.regions$Region != "South" &
                                              composed.regions$Region != "West" & composed.regions$Region != "East", ]
region.stats <- rbind(region.stats, composed.regions)

composed.regions.2 <-   region.stats[grepl(" - ", region.stats$Region.Orig)>0,]
composed.regions.2$Region <- gsub("- .*", "", composed.regions.2$Region.Orig, ignore.case = T) 
region.stats <- rbind(region.stats, composed.regions.2)

composed.regions.3 <-   region.stats[grepl("- (.*?) -", region.stats$Region.Orig)>0,]
library(stringr)
composed.regions.3$Region <- str_match(composed.regions.3$Region.Orig, "- (.*?) -")[,2] 
composed.regions.3$Region <- gsub("- ", "", composed.regions.3$Region)
composed.regions.3$Region <- gsub(" -", "", composed.regions.3$Region)
composed.regions.3$Region <- trimws(composed.regions.3$Region)
region.stats <- rbind(region.stats, composed.regions.3)


composed.regions.4 <-   region.stats[grepl("Mt ", region.stats$Region.Orig)>0,]
composed.regions.4$Region <- gsub("Mt ", "Mount ",  composed.regions.4$Region.Orig, ignore.case = T) 
region.stats <- rbind(region.stats, composed.regions.4)

composed.regions.5 <-   region.stats[grepl("(East|West|North|South|Lower|Upper)", region.stats$Region.Orig)>0,]
composed.regions.5$Region <- sub("(\\w+)\\s(West*)","\\2 \\1", composed.regions.5$Region)
composed.regions.5$Region <- sub("(\\w+)\\s(East*)","\\2 \\1", composed.regions.5$Region)
composed.regions.5$Region <- sub("(\\w+)\\s(North*)","\\2 \\1", composed.regions.5$Region)
composed.regions.5$Region <- sub("(\\w+)\\s(South*)","\\2 \\1", composed.regions.5$Region)
composed.regions.5$Region <- sub("(\\w+)\\s(Lower*)","\\2 \\1", composed.regions.5$Region)
composed.regions.5$Region <- sub("(\\w+)\\s(Upper*)","\\2 \\1", composed.regions.5$Region)
region.stats <- rbind(region.stats, composed.regions.5)


composed.regions.6 <-   region.stats[grepl("Mount ", region.stats$Region.Orig)>0,]
composed.regions.6$Region <- gsub("Mount ", "Mt ",  composed.regions.6$Region.Orig, ignore.case = T) 
region.stats <- rbind(region.stats, composed.regions.6)

region.stats$Region <- trimws(region.stats$Region)
region.stats <- region.stats[!duplicated(region.stats),]





region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Rosebud" & region.stats$State == "VIC", ] 
region.stats[nrow(region.stats), "Region"] <- "Boneo"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Keilor" & region.stats$ASGS_2016 ==  "210011228" & region.stats$State == "VIC", ]
region.stats[nrow(region.stats), "Region"] <- "Keilor Park"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Port Macquarie" & region.stats$ASGS_2016 == "108041163" & region.stats$State == "NSW", ]
region.stats[nrow(region.stats), "Region"] <- "Lake Cathie"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Mount Barker" & region.stats$State == "SA", ]
region.stats[nrow(region.stats), "Region"] <- "Littlehampton"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Phillip Island" & region.stats$State == "VIC", ]
region.stats[nrow(region.stats), "Region"] <- "Cowes"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Toowoomba" & region.stats$ASGS_2016 ==  "317" & region.stats$State == "QLD", ]
region.stats[nrow(region.stats), "Region"] <- "Dalby"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Moyne" & region.stats$ASGS_2016 == "217041478" & region.stats$State == "VIC", ]
region.stats[nrow(region.stats), "Region"] <- "Port Fairy"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Flinders" & region.stats$State == "VIC", ]
region.stats[nrow(region.stats), "Region"] <- "Red Hill"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Tamborine" & region.stats$State == "QLD", ]
region.stats[nrow(region.stats), "Region"] <- "North Tamborine"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Nelson Bay Peninsula" & region.stats$State == "NSW", ]
region.stats[nrow(region.stats), "Region"] <- "Soldiers Point"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Parklea" & region.stats$State == "NSW", ] 
region.stats[nrow(region.stats), "Region"] <- "Stanhope Gardens"

region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Adelaide City" & region.stats$State == "SA", ] 
region.stats[nrow(region.stats), "Region"] <- "Adelaide University"
region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Auburn Central" & region.stats$State == "NSW", ] 
region.stats[nrow(region.stats), "Region"] <- "Auburn"


region.stats <- region.stats[!duplicated(region.stats),]

standardise.club.names <- function(clubname.list){
  clubname.list <- as.character(clubname.list)
  clubname.list <- gsub("Lawn Tennis Club", "LTC",  clubname.list, ignore.case = T)
  clubname.list <- gsub("Lawn TC", "LTC",  clubname.list, ignore.case = T)
  clubname.list <- gsub("Tennis Club", "TC",  clubname.list, ignore.case = T)
  clubname.list <- gsub("Tennis Center", "TC",  clubname.list, ignore.case = T)
  clubname.list <- gsub("Tennis Centre", "TC",  clubname.list, ignore.case = T)
  clubname.list <- gsub("Tennis Association", "TA",  clubname.list, ignore.case = T)
  clubname.list <- gsub("Incorporated", "",  clubname.list, ignore.case = T)
  clubname.list <- gsub("Inc.", "",  clubname.list, ignore.case = T)
  clubname.list <- gsub("Inc", "",  clubname.list, ignore.case = T)
  clubname.list <- gsub("&", "and",  clubname.list, ignore.case = T)
  clubname.list <- trimws(clubname.list)
  clubname.list <- as.character(clubname.list)
  clubname.list <- proper(clubname.list)
  return(clubname.list)
}

#-------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------
# Edit facilities
#-------------------------------------------------------------------------------------------------------------------------------
facilities <- read.csv("../../facilities.csv", header = T)
facilities$Suburb <- proper(facilities$Suburb)
facilities <- as.data.frame(facilities %>% group_by(State, Suburb) %>% mutate(ClubsInSuburb = n()))

facilities$Club.Facility.Name <-  as.character(facilities$Club.Name) 

facilities.1 <- facilities
facilities.1$Club.Facility.Name <-  as.character(facilities.1$Facility.Name) 
facilities <- rbind(facilities, facilities.1)

facilities$Club.Facility.Name <- standardise.club.names(facilities$Club.Facility.Name)
facilities <- facilities[!duplicated(facilities$Club.Facility.Name), ]

facilities$Club.Facility.Name[facilities$Club.Name==""] <- as.character(facilities$Facility.Name[facilities$Club.Name==""])
facilities$Club.Facility.Name[is.na(facilities$Club.Name)] <- as.character(facilities$Facility.Name[is.na(facilities$Club.Name)])


facilities[facilities$Club.Facility.Name=="Tennis Townsville" & facilities$State == "ACT", "State"] <- "QLD"

#facilities$Facility.Name<- as.character(facilities$Facility.Name)
#facilities[facilities$Facility.Name == "Loton Park Tennis Club", "Suburb"] <- "Perth City"

facilities[nrow(facilities) +1, ] <- facilities[facilities$Facility.Name == "Englands Park Tennis Club Inc" , ]
facilities[nrow(facilities), "Club.Facility.Name"] <- "Jetty Tennis At Englands Park" 
facilities[nrow(facilities) +1, ] <- facilities[facilities$Facility.Name == "BODLEY STREET TENNIS CENTRE" , ]
facilities[nrow(facilities), "Club.Facility.Name"] <- "Bltc Bodley Street Tennis Courts" 
facilities[nrow(facilities) +1, ] <- facilities[facilities$Facility.Name == "MCC TC" , ]
facilities[nrow(facilities), "Club.Facility.Name"] <- "Mcc Roy Street St Kilda" 
facilities[nrow(facilities) +1, ] <- head(facilities[facilities$Facility.Name == "Saltwater Reserve" , ], 1)
facilities[nrow(facilities), "Club.Facility.Name"] <- "Saltwater Tc" 
facilities[nrow(facilities) +1, ] <- facilities[facilities$Facility.Name == "Blacktown City Council" , ]
facilities[nrow(facilities), "Club.Facility.Name"] <- "Jonas Bradley Park" 
facilities[nrow(facilities) +1, ] <- head(facilities[facilities$Facility.Name == "Bar Beach Tennis Club Inc." , ], 1)
facilities[nrow(facilities), "Club.Facility.Name"] <- "Reid Park Tennis Courts" 
facilities[nrow(facilities) +1, ] <- facilities[facilities$Facility.Name == "Wesley Uniting Church" , ]
facilities[nrow(facilities), "Club.Facility.Name"] <- "Wesley Uniting Church Tc" 

# facilities[facilities$Facility.Name == "Bendigo Tennis Complex" , "Club.Facility.Name"] <- "Bendigo Tennis Complex" 
# facilities[facilities$Facility.Name == "Englands Park Tennis Club Inc" , "Club.Facility.Name"] <- "Jetty Tennis At Englands Park" 
# facilities[facilities$Facility.Name == "BODLEY STREET TENNIS CENTRE" , "Club.Facility.Name"] <- "Bltc Bodley Street Tennis Courts" 
# facilities[facilities$Facility.Name == "MCC TC" , "Club.Facility.Name"] <- "Mcc Roy Street St Kilda" 
# facilities[facilities$Facility.Name == "Bar Beach Tennis Club Inc." & facilities$Club.Facility.Name == "Reid Park Tc", "Club.Facility.Name"] <- "Reid Park Tennis Courts"
# facilities[facilities$Facility.Name == "Saltwater Reserve" , "Club.Facility.Name"] <- "Saltwater Tc" 
# facilities[facilities$Facility.Name == "South Perth Tennis Club" , "Club.Facility.Name"] <- "South Perth Tc" 
# facilities[facilities$Facility.Name == "Tennis SA (North Adelaide)" , "Club.Facility.Name"] <- "Tennis Sa (North Adelaide)" 
# facilities[facilities$Facility.Name == "Werrington Tennis Courts" , "Club.Facility.Name"] <- "Werrington Tennis Courts" 
# facilities[facilities$Facility.Name == "Blacktown City Council" , "Club.Facility.Name"] <- "Jonas Bradley Park" 
# facilities[facilities$Facility.Name == "Wesley Uniting Church" , "Club.Facility.Name"] <- "Wesley Uniting Church Tc" 
# facilities[facilities$Facility.Name == "Narraweena Tennis Club " , "Club.Facility.Name"] <- "Narraweena Tc" 

facilities$Club.Facility.Name <- standardise.club.names(facilities$Club.Facility.Name)
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
facilities.populate.club.names <- facilities[,c("Club.Facility.Name",  "Facility.Name")]
names(facilities.populate.club.names)[1] <- "Club.Facility.Name4"
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
distances.to.cbd<- read.csv("../../club.distances.to.cbd.csv")

distances.to.cbd$Club.Facility.Name.All <-  as.character(distances.to.cbd$Club.Facility.Name2) 

distances.to.cbd.1 <- distances.to.cbd
distances.to.cbd.1$Club.Facility.Name.All <-  as.character(distances.to.cbd.1$Club.Facility.Name) 

distances.to.cbd.2 <- distances.to.cbd
distances.to.cbd.2$Club.Facility.Name.All <-  as.character(distances.to.cbd.2$Club.Name) 

distances.to.cbd.3 <- distances.to.cbd
distances.to.cbd.3$Club.Facility.Name.All <-  as.character(distances.to.cbd.3$Facility.Name) 

distances.to.cbd<- rbind(distances.to.cbd, distances.to.cbd.1, distances.to.cbd.2, distances.to.cbd.3)
distances.to.cbd<- merge(distances.to.cbd, facilities.populate.club.names, by="Facility.Name", all.x=T)

distances.to.cbd.4 <- distances.to.cbd
distances.to.cbd.4$Club.Facility.Name.All <-  as.character(distances.to.cbd.4$Club.Facility.Name4) 
distances.to.cbd<- rbind(distances.to.cbd, distances.to.cbd.4)

venue.regions <- read.csv("../../venueRegions.csv", header = T)
venue.regions <- venue.regions[,c("Venue.Name", "State", "DistFromCBD","RegionLocation")]
names(venue.regions) <- c("Club.Facility.Name.All", "State", "DistanceToCBD.km","RegionLocation")

distances.to.cbd<- bind_rows(distances.to.cbd, venue.regions)

distances.to.cbd$Club.Facility.Name.All <- standardise.club.names(distances.to.cbd$Club.Facility.Name.All)
distances.to.cbd<- distances.to.cbd[!duplicated(distances.to.cbd$Club.Facility.Name.All),]

# distances.to.cbd[distances.to.cbd$Club.Facility.Name2 == "Bendigo Ta" , "Club.Club.Facility.Name2"] <- "Bendigo Tennis Complex" 
# distances.to.cbd[distances.to.cbd$Club.Facility.Name2 == "Englands Park Tennis Club Inc" , "Club.Club.Facility.Name2"] <- "Jetty Tennis At Englands Park" 
# distances.to.cbd[distances.to.cbd$Club.Facility.Name2 == "BODLEY STREET TENNIS CENTRE" , "Club.Club.Facility.Name2"] <- "Bltc Bodley Street Tennis Courts" 
# distances.to.cbd[distances.to.cbd$Club.Facility.Name2 == "MCC TC" , "Club.Club.Facility.Name2"] <- "Mcc Roy Street St Kilda" 
# distances.to.cbd[distances.to.cbd$Club.Facility.Name2 == "Bar Beach Tennis Club Inc." & distances.to.cbd$Club.Club.Facility.Name2 == "Reid Park Tc", "Club.Club.Facility.Name2"] <- "Reid Park Tennis Courts"
# distances.to.cbd[distances.to.cbd$Club.Facility.Name2 == "Saltwater Reserve" , "Club.Club.Facility.Name2"] <- "Saltwater Tc" 
# distances.to.cbd[distances.to.cbd$Club.Facility.Name2 == "South Perth Tennis Club" , "Club.Club.Facility.Name2"] <- "South Perth Tc" 
# distances.to.cbd[distances.to.cbd$Club.Facility.Name2 == "Tennis SA (North Adelaide)" , "Club.Club.Facility.Name2"] <- "Tennis Sa (North Adelaide)" 
# distances.to.cbd[distances.to.cbd$Club.Facility.Name2 == "Werrington Tennis Courts" , "Club.Club.Facility.Name2"] <- "Werrington Tennis Courts" 
# distances.to.cbd[distances.to.cbd$Club.Facility.Name2 == "Blacktown City Council" , "Club.Club.Facility.Name2"] <- "Jonas Bradley Park" 
# distances.to.cbd[distances.to.cbd$Club.Facility.Name2 == "Wesley Uniting Church" , "Club.Club.Facility.Name2"] <- "Wesley Uniting Church Tc" 
# distances.to.cbd[distances.to.cbd$Club.Facility.Name2 == "Narraweena Tennis Club " , "Club.Club.Facility.Name2"] <- "Narraweena Tc" 

#test <- merge(facilities, distances.to.cbd[,c("RegionLocation", "DistanceToCBD.km", "Club.Facility.Name.All")], by.x="Club.Facility.Name", by.y="Club.Facility.Name.All", all.x=T)
#summary(is.na(test$DistanceToCBD.km))
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
facilities <- merge(facilities, distances.to.cbd[,c("RegionLocation", "DistanceToCBD.km", "Club.Facility.Name.All")], by.x="Club.Facility.Name", by.y="Club.Facility.Name.All", all.x=T)

#distances.to.cbd[!((distances.to.cbd$Club.Facility.Name.All) %in% (facilities$Club.Facility.Name)), ]

#summary(is.na(facilities$DistanceToCBD.km))
#unique(facilities[is.na(facilities$RegionLocation), "Club.Facility.Name"])
#View(facilities[is.na(facilities$RegionLocation), ])
#venue.regions <- read.csv("../../venueRegions.csv", header = T)
#bookings.made.v.grouped <- merge(bookings.made.v.grouped, venue.regions, all.x = T)

#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------

bookings.made$Venue.Name <- standardise.club.names(bookings.made$Venue.Name)
bookings.played$Venue.Name <-  standardise.club.names(bookings.played$Venue.Name)

#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
venues <- facilities[, c("Country", "State", "Post.Code", "Longitude", "Latitude", "Suburb", "Club.Facility.Name", "Facility.Name",
                         "Total.Full.Count.Grass", "Total.Full.Count.Non.Cushioned.Hard.Court", "Total.Full.Count.Synthetic.Grass",
                         "Total.Full.Count.Clay", "Total.Full.Count.Cushioned.Hard.Court", "Total.Full.Count.Other", 
                         "Total.Full.Count.Synthetic.Clay", "Hot.Shots.Red.count.All", "Hot.Shots.Orange.Count.All", 
                         "Total.Full.Count.All", "Other.Count.All", "Outdoor.Full.Count.All", "Indoor.Full.Count.All", 
                         "Lighted.Full.Count.All", "Geographical.classification", "Facility.Type", "Organisation.Type", 
                         "Organisation.Status", "Amenities","ClubsInSuburb", "DistanceToCBD.km", "RegionLocation")]
venues$Count <- 1
venues<- venues %>% group_by(Country, State, Post.Code, Club.Facility.Name) %>% mutate(ClubsInSuburb= sum(Count)) %>% ungroup()
venues$recently.advertised <- FALSE
venues[venues$Club.Facility.Name %in% c("Belconnen Tc",
                                        "Royal Park Tc",
                                        "Albion Tc",
                                        "North Park Tc",
                                        "Kensington Community Recreation Centre",
                                        "Ainslie Tc",
                                        "Princes Hill Tc",
                                        "Carlton Gardens Tc",
                                        "Red Hill Tc",
                                        "Weston Creek Tc",
                                        "Barton Tc",
                                        "Eastlake Tc",
                                        "Yarralumla Tc"), "recently.advertised"] <- TRUE
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
venues$court.options <-  as.numeric(venues$has.indoor) + as.numeric(venues$has.outdoor) +
  as.numeric(venues$has.grass) +as.numeric(venues$has.clay) + as.numeric(venues$has.hard) + as.numeric(venues$Other.Count.All > 0)
#venues$court.specs<- as.numeric(venues$has.Lights)  +as.numeric(venues$has.hot.shot) 

venues$Amenities.options <- str_count(venues$Amenities, ",")
venues[venues$Amenities.options == 0,  "Amenities.options"] <- 1

venues$nbr.courts <- venues$Total.Full.Count.All + venues$Other.Count.All

venues[venues$nbr.courts ==0, "nbr.courts"] <- 2



############################################################################################################################
bookings.made.v <- merge(bookings.made, venues, by.x = "Venue.Name" , by.y = "Club.Facility.Name", all.x = T)
bookings.played.v <- merge(bookings.played, venues, by.x = "Venue.Name" , by.y = "Club.Facility.Name")
############################################################################################################################

bookings.made.v[bookings.made.v$nbr.courts < 1, "nbr.courts"] <- 2
bookings.played.v[bookings.played.v$nbr.courts < 1, "nbr.courts"] <- 2

opening.times <- read.csv("../../Venue-opening-closing-times.csv", header = T)
opening.times$Venue.Name <- standardise.club.names(opening.times$Venue.Name)

bookings.made.v <- merge(bookings.made.v, opening.times, by = c("Venue.Name"))
bookings.played.v <- merge(bookings.played.v, opening.times, by = c("Venue.Name"))

#v <- v[!duplicated(v$Club.Facility.Name), c("Country", "State", "Post.Code", "Longitude", "Latitude","Suburb", "Club.Facility.Name")]
#unique(bookings.made$Venue.Name)[-which(unique(bookings.made$Venue.Name) %in% venues$Club.Facility.Name)]
#write.csv(v, "venues.csv")

bookings.made.v$Suburb <- proper(bookings.made.v$Suburb)
bookings.made.v$Suburb <- trimws(bookings.made.v$Suburb)
bookings.played.v$Suburb <- proper(bookings.played.v$Suburb)
bookings.played.v$Suburb <- trimws(bookings.played.v$Suburb)


# region.stat <- region.stats[region.stats$REGIONTYPE == "SA2" | region.stats$REGIONTYPE == "SA3", ]
# region.stats.unique <- region.stat[order(region.stat$REGIONTYPE), ]
# region.stats.unique <- region.stats.unique[!duplicated(region.stats.unique[,c("Region", "State")]),]
#   
region.stats$Region <- proper(region.stats$Region)
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
#nrow(bookings.made.v)
bookings.made.v <- as.data.frame(bookings.made.v)
bookings.made.v <- bookings.made.v %>% group_by(Venue.Name, Booking.Date, Booking.Time, Booker.Last.Name, Player.Last.Name, Player.First.Name) %>%
  mutate(max.Booking.Duration = max(Booking.Duration, na.rm = T), dup.index = seq(1:n()), n=n()) %>% ungroup

booking.duplicates <- bookings.made.v[bookings.made.v$n >1,  c("Venue.Name", "Booker.Last.Name", "Booker.First.Name",
                                                               "Player.Last.Name", "Player.First.Name", "Booking.Date", 
                                                               "Booking.Time",  "Booking.ID", "dup.index")]
write.csv(booking.duplicates, "../../booking.duplicates.csv")
bookings.made.v <- as.data.frame(bookings.made.v)
bookings.made.v<- bookings.made.v[bookings.made.v$dup.index ==1,]


# discripenciesx<- (do.call(paste0, x[ , c("Suburb", "State")]) %in% do.call(paste0, region.stats[, c("Region", "State")]))
# discripenciesx<- which(discripenciesx == FALSE)
# View(unique(x[discripenciesx, c("Suburb", "State")]))


bookings.closed <-  bookings.made.v%>% group_by(Venue.ID, Venue.Name, Year.OfBooking, Booking.Month, Booking.Day, Booking.Date, Total.Closing.Mins) %>%
  summarise(totaldays = 1)
bookings.closed <-  bookings.closed%>%  group_by(Venue.ID, Venue.Name, Year.OfBooking, Booking.Month, Booking.Day) %>%
  summarise(totaldays = sum(totaldays), Total.Closing.Mins.All.Days = sum(Total.Closing.Mins))

bookings.made.v$individual.booking <- 1
bookings.made.v.grouped <-  bookings.made.v%>% group_by(Venue.Name, Suburb, Post.Code, State, Year.OfBooking, Booking.Month, Booking.Day, Booking.Weekend, Total.Closing.Mins,
                                                        Total.Opening.Hours, SpeaksaLanguageOtherThanEnglishatHome.Proportionoftotalpopulation.Perc, Australiancitizen.Perc,
                                                        ClubsInSuburb, recently.advertised, DistanceToCBD.km, RegionLocation, Amenities.options,
                                                        REGIONTYPE, Facility.Type, Organisation.Type, Organisation.Status, Populationdensity.ERPat30June.persons.km2,
                                                        Booking.Type, Density, MaleFemalePerc, Geographical.classification, 
                                                        Total.Full.Count.Grass                          ,Total.Full.Count.Non.Cushioned.Hard.Court ,      
                                                        Total.Full.Count.Synthetic.Grass                ,Total.Full.Count.Clay                     ,      
                                                        Total.Full.Count.Cushioned.Hard.Court           ,Total.Full.Count.Other                    ,      
                                                        Total.Full.Count.Synthetic.Clay                 ,Hot.Shots.Red.count.All                   ,      
                                                        Hot.Shots.Orange.Count.All                      ,Total.Full.Count.All                      ,      
                                                        Other.Count.All                                 ,Outdoor.Full.Count.All                    ,      
                                                        Indoor.Full.Count.All                           ,Lighted.Full.Count.All                    ,      
                                                        has.Lights                                      ,has.indoor                                ,      
                                                        has.outdoor                                     ,has.grass                                 ,      
                                                        has.clay                                        ,has.hard                                  ,      
                                                        has.hot.shot                                    ,court.options                             ,      
                                                        nbr.courts                                      ,Aboriginal.Perc                           ,      
                                                        Buddhism.Perc                                   ,Christianity.Perc                         ,      
                                                        CompletedYear12orequivalent.Perc                ,Females.Total.no.                         ,      
                                                        Hinduism.Perc                                   ,Islam.Perc                                ,      
                                                        Judaism.Perc                                    ,LandArea.Ha                               ,      
                                                        Males.Total.no.                                 ,MedianAge.Females.years                   ,      
                                                        MedianAge.Males.years                           ,MedianAge.Persons.years                   ,      
                                                        Medianequivalisedtotalhouseholdincome.weekly.AUD,NotanAustraliancitizen.Perc               ,      
                                                        OtherReligions.Perc                             ,Persons.Total.no.                         ,      
                                                        SecularBeliefs.Perc                             ,Establishmentswith15ormorerooms.no.        ,      
                                                        Totalbornoverseas.Perc                          ,Totalnumberofbusinesses.no.               ,      
                                                        WithGraduateDiploma.GraduateCertificate.Perc    ,WorkingAgePopulation.aged15.64years       ,
                                                        WithPostgraduateDegree.Perc, WithPostSchoolQualifications.Perc, 
                                                        BorninAmericas.Perc, BorninNorthAfricaandtheMiddleEast.Perc,
                                                        BorninNorth.EastAsia.Perc, BorninNorth.WestEurope.Perc,
                                                        BorninOceaniaandAntarctica.excludingAustralia.Perc,
                                                        BorninSouth.EastAsia.Perc,
                                                        BorninSouthernandCentralAsia.Perc,
                                                        BorninSouthernandEasternEurope.Perc, 
                                                        BorninSub.SaharanAfrica.Perc,
                                                        LandArea.Km2,
                                                        Persons.Total.no.,
                                                        Totalfamilies.no.) %>%
  summarise(PmBookings = sum(After6Pm), Court.Fee = sum(Court.Fee), Light.Fee= sum(Light.Fee), Admin.Fee = sum(Admin.Fee), Total.Cost = sum(Total.Cost), 
            Total.Bookings = sum(individual.booking), Booking.Duration = sum(Booking.Duration))

bookings.availability <-  bookings.made.v%>%  filter(Booking.Type != "Booking")  %>% 
  group_by(Venue.Name, Suburb, Post.Code, State, Year.OfBooking, Booking.Month, Booking.Day, Australiancitizen.Perc, ClubsInSuburb, recently.advertised,
           Total.Closing.Mins, Total.Opening.Hours, SpeaksaLanguageOtherThanEnglishatHome.Proportionoftotalpopulation.Perc,
           Booking.Weekend, REGIONTYPE, Facility.Type, Organisation.Type, Organisation.Status,
           Density, MaleFemalePerc, Geographical.classification, Populationdensity.ERPat30June.persons.km2,
           Total.Full.Count.Grass                          ,Total.Full.Count.Non.Cushioned.Hard.Court ,      
           Total.Full.Count.Synthetic.Grass                ,Total.Full.Count.Clay                     ,      
           Total.Full.Count.Cushioned.Hard.Court           ,Total.Full.Count.Other                    ,      
           Total.Full.Count.Synthetic.Clay                 ,Hot.Shots.Red.count.All                   ,      
           Hot.Shots.Orange.Count.All                      ,Total.Full.Count.All                      ,      
           Other.Count.All                                 ,Outdoor.Full.Count.All                    ,      
           Indoor.Full.Count.All                           ,Lighted.Full.Count.All                    ,      
           has.Lights                                      ,has.indoor                                ,      
           has.outdoor                                     ,has.grass                                 ,      
           has.clay                                        ,has.hard                                  ,      
           has.hot.shot                                    ,court.options                             ,      
           nbr.courts                                      ,Aboriginal.Perc                           ,      
           Buddhism.Perc                                   ,Christianity.Perc                         ,      
           CompletedYear12orequivalent.Perc                ,Females.Total.no.                         ,      
           Hinduism.Perc                                   ,Islam.Perc                                ,      
           Judaism.Perc                                    ,LandArea.Ha                               ,      
           Males.Total.no.                                 ,MedianAge.Females.years                   ,      
           MedianAge.Males.years                           ,MedianAge.Persons.years                   ,      
           Medianequivalisedtotalhouseholdincome.weekly.AUD,NotanAustraliancitizen.Perc               ,      
           OtherReligions.Perc                             ,Persons.Total.no.                         ,      
           SecularBeliefs.Perc                             ,Establishmentswith15ormorerooms.no.        ,      
           Totalbornoverseas.Perc                          ,Totalnumberofbusinesses.no.               ,      
           WithGraduateDiploma.GraduateCertificate.Perc    ,WorkingAgePopulation.aged15.64years       ,
           WithPostgraduateDegree.Perc, WithPostSchoolQualifications.Perc,
           BorninAmericas.Perc, BorninNorthAfricaandtheMiddleEast.Perc,
           BorninNorth.EastAsia.Perc, BorninNorth.WestEurope.Perc,
           BorninOceaniaandAntarctica.excludingAustralia.Perc,
           BorninSouth.EastAsia.Perc,
           BorninSouthernandCentralAsia.Perc,
           BorninSouthernandEasternEurope.Perc, 
           BorninSub.SaharanAfrica.Perc,
           LandArea.Km2,
           Persons.Total.no.,
           Totalfamilies.no.) %>% summarise(unavailability = sum(Booking.Duration) )



bookings.made.v.grouped <- bookings.made.v.grouped %>% filter(Booking.Type == "Booking")
bookings.made.v.grouped <- merge(bookings.made.v.grouped, bookings.availability, all.x = T)
bookings.made.v.grouped[is.na(bookings.made.v.grouped$unavailability), "unavailability"] <- 0
bookings.made.v.grouped <- merge(bookings.made.v.grouped, bookings.closed)

bookings.made.v.grouped$Total.Closing.Mins.All.Days <- bookings.made.v.grouped$Total.Closing.Mins.All.Days*bookings.made.v.grouped$nbr.courts
bookings.made.v.grouped$Total.Unavailablility <-  bookings.made.v.grouped$unavailability + 
  (bookings.made.v.grouped$totaldays*bookings.made.v.grouped$Total.Closing.Mins*bookings.made.v.grouped$nbr.courts)
bookings.made.v.grouped$Total.Availablility <-  bookings.made.v.grouped$totaldays*bookings.made.v.grouped$nbr.courts*1440 - bookings.made.v.grouped$Total.Unavailablility 
bookings.made.v.grouped$Utilisation <-  (bookings.made.v.grouped$Booking.Duration/bookings.made.v.grouped$Total.Availablility)*100
bookings.made.v.grouped$Total.Opening.Hours <- gsub(":00", "", bookings.made.v.grouped$Total.Opening.Hours)
bookings.made.v.grouped$Total.Opening.Hours <- gsub(":30", ".5", bookings.made.v.grouped$Total.Opening.Hours)
bookings.made.v.grouped$Total.Opening.Hours <- gsub(":15", ".25", bookings.made.v.grouped$Total.Opening.Hours)
bookings.made.v.grouped$Total.Opening.Hours <- gsub(":45", ".75", bookings.made.v.grouped$Total.Opening.Hours)
bookings.made.v.grouped$Total.Opening.Hours <- as.numeric(bookings.made.v.grouped$Total.Opening.Hours)


bookings.made.v.grouped$Has.Late.Bookings <- bookings.made.v.grouped$Total.Opening.Hours>12

#venue.regions <- read.csv("../../venueRegions.csv", header = T)
#bookings.made.v.grouped <- merge(bookings.made.v.grouped, venue.regions, all.x = T)

bookings.made.v.grouped.norm <- bookings.made.v.grouped %>% group_by(Venue.Name, Suburb, Post.Code, State, nbr.courts ) %>% 
  mutate(Norm.bookings.count = Total.Bookings/max(Total.Bookings), 
         Norm.bookings.duration = normBetweenZeroOne(Booking.Duration),# /max(Booking.Duration),
         Norm.utilisation = normBetweenZeroOne(Utilisation),#/max(Utilisation),
         Norm.Total.Availablility = normBetweenZeroOne(Total.Availablility),#/max(Total.Availablility),
         Norm.Total.Unavailablility = Total.Unavailablility/max(Total.Unavailablility)) %>% ungroup()


source("Memberships.R")
source("MembersByCourt.R")
source("MembersByCourtByGender.R")




#rm(list= ls(1)[grep("member", ls())])




# facilities$Club.Facility.Name <- gsub("TC", "Tennis Club",facilities$Club.Name, ignore.case =  T)
# facilities[facilities$Facility.Name == "BEAUMARIS LTC" , "Club.Facility.Name"] <- "Beaumaris Lawn Tennis Club" 
# facilities[facilities$Facility.Name == "BODLEY STREET TENNIS CENTRE" , "Club.Facility.Name"] <- "BLTC Bodley Street Tennis Courts" 
# facilities[facilities$Facility.Name == "BONEO TC" , "Club.Facility.Name"] <- "Boneo Tennis Club"
# facilities[facilities$Facility.Name == "Bundaberg & District Junior Tennis Association Inc" , "Club.Facility.Name"] <- "Bundaberg and District Junior Tennis Association" 
# facilities[facilities$Facility.Name == "BUNINYONG DISTRICT TA" , "Club.Facility.Name"] <- "Buninyong & District Tennis Association" 
# facilities[facilities$Facility.Name == "Burleigh Heads Tennis Club Inc" , "Club.Facility.Name"] <- "Burleigh Heads Tennis Club" 
# facilities[facilities$Facility.Name == "Dalby & District Tennis Association Inc" , "Club.Facility.Name"] <- "Dalby and District Tennis Association" 
# facilities[facilities$Facility.Name == "DAYLESFORD LTC" , "Club.Facility.Name"] <- "Daylesford Lawn Tennis Club" 
# facilities[facilities$Facility.Name == "Freshwater Tennis Club Inc" , "Club.Facility.Name"] <- "Freshwater Tennis Club"
# facilities[facilities$Facility.Name == "Hume Tennis and Community Centre" , "Club.Facility.Name"] <- "Hume Tennis & Community Centre" 
# facilities[facilities$Facility.Name == "Englands Park Tennis Club Inc" , "Club.Facility.Name"] <- "Jetty Tennis at Englands Park" 
# facilities[facilities$Facility.Name == "Stanhope Gardens Tennis Centre" , "Club.Facility.Name"] <- "Jonas Bradley Park" 
# facilities[facilities$Facility.Name == "Lambton Park Tennis Club Inc" , "Club.Facility.Name"] <- "Lambton Park Tennis Club" 
# facilities[facilities$Facility.Name == "MCC TC" , "Club.Facility.Name"] <- "MCC Roy Street St Kilda"  #????
# facilities[facilities$Facility.Name == "Ocean Shores Tennis Club Inc" , "Club.Facility.Name"] <- "Ocean Shores Tennis Club" 
# facilities[facilities$Facility.Name == "Parramatta City Tennis Inc." , "Club.Facility.Name"] <- "Parramatta City Tennis" 
# facilities[facilities$Facility.Name == "Redcliffe Tennis Association Inc" , "Club.Facility.Name"] <- "Redcliffe Tennis Association" 
# facilities[facilities$Facility.Name == "Redland Bay Tennis Club Inc" , "Club.Facility.Name"] <- "Redland Bay Tennis Club" 
# facilities[facilities$Facility.Name == "Redlynch Valley Tennis Club Inc" , "Club.Facility.Name"] <- "Redlynch Valley Tennis Club" 
# facilities[facilities$Facility.Name == "Bar Beach Tennis Club Inc." , "Club.Facility.Name"] <- "Reid Park Tennis Courts" 
# facilities[facilities$Facility.Name == "Roma & District Tennis Club Inc" , "Club.Facility.Name"] <- "Roma and District Tennis Club" 
# facilities[facilities$Facility.Name == "Saltwater Reserve" , "Club.Facility.Name"] <- "Saltwater Tennis Centre" 
# facilities[facilities$Facility.Name == "Sorell Tennis Club" , "Club.Facility.Name"] <- "Sorell Tennis Club" 
# facilities[facilities$Facility.Name == "South Perth Tennis Club" , "Club.Facility.Name"] <- "South Perth Tennis Centre" 
# facilities[facilities$Facility.Name == "SUNBURY Tennis Club" , "Club.Facility.Name"] <- "Sunbury Lawn Tennis Club" 
# facilities[facilities$Facility.Name == "Tamborine Mountain Tennis Club Inc" , "Club.Facility.Name"] <- "Tamborine Mountain Tennis Club" 
# facilities[facilities$Facility.Name == "SUNBURY Tennis Club" , "Club.Facility.Name"] <- "Sunbury Lawn Tennis Club" 
# facilities[facilities$Facility.Name == "Tamborine Mountain Tennis Club Inc" , "Club.Facility.Name"] <- "Tamborine Mountain Tennis Club" 
# facilities[facilities$Facility.Name == "Tennis SA (North Adelaide)" & facilities$Club.Name == "Tennis SA", "Club.Facility.Name"] <- "Tennis SA (North Adelaide)" 
# facilities[facilities$Facility.Name == "Tennis Townsville Inc" , "State"] <- "QLD" 
# facilities[facilities$Facility.Name == "Tennis Townsville Inc" , "Club.Facility.Name"] <- "Tennis Townsville" 
# facilities[facilities$Facility.Name == "Tewantin Tennis Club Inc" , "Club.Facility.Name"] <- "Tewantin Tennis Club" 
# facilities[facilities$Facility.Name == "WANGARATTA  LTC" , "Club.Facility.Name"] <- "Wangaratta Lawn Tennis Club" 
# facilities[facilities$Facility.Name == "Warwick & District Tennis Association Inc" , "Club.Facility.Name"] <- "Warwick and District Tennis Association" 
# facilities[facilities$Facility.Name == "Werrington Tennis Courts" , "Club.Facility.Name"] <- "Werrington Tennis Courts" 
# facilities[facilities$Facility.Name == "Wesley Uniting Church" , "Club.Facility.Name"] <- "Wesley Uniting Church Tennis Club" 
# facilities[facilities$Facility.Name == "Western Suburbs Tennis Club Inc" , "Club.Facility.Name"] <- "Western Suburbs Tennis Club" 

#
# region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Ardeer" & region.stats$State == "VIC", ] 
# region.stats[nrow(region.stats), "Region"] <- "Albion"
# region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Lorne" & region.stats$State == "VIC", ] 
# region.stats[nrow(region.stats), "Region"] <- "Anglesea"
# region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Aspendale Gardens" & region.stats$State == "VIC", ] 
# region.stats[nrow(region.stats), "Region"] <- "Aspendale"
# region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Nedlands" & region.stats$State == "WA", ] 
# region.stats[nrow(region.stats), "Region"] <- "Dalkeith"
# region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Queanbeyan West" & region.stats$State == "NSW", ] 
# region.stats[nrow(region.stats), "Region"] <- "Jerrabomberra"
# region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "East Victoria Park" & region.stats$State == "WA", ] 
# region.stats[nrow(region.stats), "Region"] <- "Burswood"
# region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "North Ryde" & region.stats$State == "NSW", ] 
# region.stats[nrow(region.stats), "Region"] <- "Marsfield"
# region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Beacon Hill" & region.stats$State == "NSW", ] 
# region.stats[nrow(region.stats), "Region"] <- "Narraweena"
# region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Townsville City" & region.stats$State == "QLD", ] 
# region.stats[nrow(region.stats), "Region"] <- "North Ward"
# region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Brunswick Heads" & region.stats$State == "NSW", ] 
# region.stats[nrow(region.stats), "Region"] <- "Ocean Shores"
# region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Mordialloc" & region.stats$State == "VIC", ] 
# region.stats[nrow(region.stats), "Region"] <- "Parkdale"
# region.stats[nrow(region.stats) +1, ] <- region.stats[region.stats$Region == "Kingswood" & region.stats$State == "NSW", ] 
# region.stats[nrow(region.stats), "Region"] <- "Werrington"
