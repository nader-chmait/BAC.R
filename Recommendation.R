rm(list= c("opening.times", "new.regions", "potential.courts"))


opening.times <- read.csv("../../Venue-opening-closing-times.csv", header = T)
opening.times$Total.Opening.Hours <- gsub(":00", "", opening.times$Total.Opening.Hours)
opening.times$Total.Opening.Hours <- gsub(":30", ".5", opening.times$Total.Opening.Hours)
opening.times$Total.Opening.Hours <- gsub(":15", ".25", opening.times$Total.Opening.Hours)
opening.times$Total.Opening.Hours <- gsub(":45", ".75", opening.times$Total.Opening.Hours)
opening.times$Total.Opening.Hours <- as.numeric(opening.times$Total.Opening.Hours)
opening.times$Venue.Name <- as.character(opening.times$Venue.Name)
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Bathurst Street Tennis Courts", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Bathurst Street"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Bega Tennis Club", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Bega Tennis Club Inc."
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Byrnes Street Tennis Courts", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Byrnes Street"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Casino Town Tennis Club", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Casino Town Tennis Club Inc"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Central Gardens Tennis Courts", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Central Gardens"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Civic Park Tennis Courts", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Civic Park"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Anglesea Tennis Club", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Croydon Tennis Centre"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Currumbin Tennis Club", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Currumbin Tennis Club Inc."
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Dirrabarri Tennis Courts", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Dirrabarri Tennis Centre"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Anglesea Tennis Club", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Elsternwick Park Tennis Centre"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Fullagar Road Tennis Courts", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Fullagar Road Tennis Centre"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Anglesea Tennis Club", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Dingley Tennis Club"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Anglesea Tennis Club", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Gardens Tennis"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Greystanes Sportsground Tennis Courts", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Greystanes Sportsground"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Anglesea Tennis Club", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Hinton & District Tennis Club Inc"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Innisfail District Tennis Association", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Innisfail District Tennis Association Inc"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Kalynda Chase Regional Tennis Centre", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Kalynda Chase Tennis Centre"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Anglesea Tennis Club", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Laverton Park Tennis Club"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Lawson Square Park Tennis Courts", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Lawson Square Park"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Anglesea Tennis Club", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Lismore Tennis Club"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Anglesea Tennis Club", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Macleod Tennis Club"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Anglesea Tennis Club", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Mentone Tennis Club"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Merrylands Park Tennis Courts", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Merrylands Park"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Mill Park Tennis Club South Morang", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Mill Park Tennis Club"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Mount Evelyn Tennis Club", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Mt Evelyn Tennis Club"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Murwillumbah Tennis Club", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Murwillumbah Tennis Inc"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Paramount Tennis Club Dubbo", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Paramount Tennis Club - Dubbo"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Anglesea Tennis Club", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Raymond Terrace & District Tennis Club"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Tennis Rockhampton", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Tennis Rockhampton Ltd"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Thorneside Community Tennis Association", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Thorneside Community Tennis Association Inc"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Anglesea Tennis Club", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Tullamarine Tennis Club"
opening.times[nrow(opening.times) +1, ] <- opening.times[opening.times$Venue.Name == "Anglesea Tennis Club", ] 
opening.times[nrow(opening.times), "Venue.Name"] <- "Wyong District Tennis Association Inc"


new.regions <- region.stats.unique


# new.regions[nrow(new.regions) +1, ] <- new.regions[new.regions$Region == "Croydon Park" & new.regions$State == "NSW", ] 
# new.regions[nrow(new.regions), "Region"] <- "Croydon"
# new.regions[nrow(new.regions) +1, ] <- new.regions[new.regions$Region == "Granville" & new.regions$State == "NSW", ] 
# new.regions[nrow(new.regions), "Region"] <- "South Granville"
# new.regions[nrow(new.regions) +1, ] <- new.regions[new.regions$Region == "Heidelberg West" & new.regions$State == "VIC", ] 
# new.regions[nrow(new.regions), "Region"] <- "Rosanna"
# new.regions[nrow(new.regions) +1, ] <- new.regions[new.regions$Region == "Darwin City" & new.regions$State == "NT", ] 
# new.regions[nrow(new.regions), "Region"] <- "The Gardens"


new.regions[nrow(new.regions) +1, ] <- new.regions[new.regions$Region == "Sunshine West" & new.regions$State == "VIC", ] 
new.regions[nrow(new.regions), "Region"] <- "Brooklyn"
new.regions[nrow(new.regions) +1, ] <- new.regions[new.regions$Region == "Bentleigh East" & new.regions$State == "VIC", ] 
new.regions[nrow(new.regions), "Region"] <- "East Bentleigh"
new.regions[nrow(new.regions) +1, ] <- new.regions[new.regions$Region == "Maitland" & new.regions$State == "NSW", ] 
new.regions[nrow(new.regions), "Region"] <- "Hinton"
new.regions[nrow(new.regions) +1, ] <- new.regions[new.regions$Region == "Orange Region" & new.regions$State == "NSW", ] 
new.regions[nrow(new.regions), "Region"] <- "Millthorpe"
new.regions[nrow(new.regions) +1, ] <- new.regions[new.regions$Region == "Watsonia" & new.regions$State == "VIC", ] 
new.regions[nrow(new.regions), "Region"] <- "Macleod"
new.regions[nrow(new.regions) +1, ] <- new.regions[new.regions$Region == "Mount Evelyn" & new.regions$State == "VIC", ] 
new.regions[nrow(new.regions), "Region"] <- "Mt Evelyn"
new.regions[nrow(new.regions) +1, ] <- new.regions[new.regions$Region == "Greystanes" & new.regions$State == "NSW", ] 
new.regions[nrow(new.regions), "Region"] <- "Pemulway"
new.regions[nrow(new.regions) +1, ] <- new.regions[new.regions$Region == "Cambridge" & new.regions$State == "TAS", ] 
new.regions[nrow(new.regions), "Region"] <- "Richmond"
new.regions[nrow(new.regions) +1, ] <- new.regions[new.regions$Region == "Rockhampton City" & new.regions$State == "QLD", ] 
new.regions[nrow(new.regions), "Region"] <- "Wandal"

new.regions <- new.regions[!duplicated(new.regions[ ,c("Region", "State")]), ]
new.regions$Region <- proper(new.regions$Region)
new.regions$Region <- trimws(new.regions$Region)

#IMPORTANT
#potential.courts <- read.csv("../../PotentialCourts.csv", header = T)
potential.courts <- data.frame(Club.Facility.Name = unique(members.by.court$Club.Facility.Name2))
#
  
potential.courts[-which(potential.courts$Club.Facility.Name %in% venues$Club.Facility.Name),]
#potential.courts$Club.Facility.Name <- standardise.club.names(potential.courts$Club.Facility.Name)

potential.courts <- merge(potential.courts, venues)

potential.courts$Suburb <- proper(potential.courts$Suburb)
potential.courts$Suburb <- trimws(potential.courts$Suburb)

potential.courts <- merge(potential.courts, new.regions, all.x=T, by.x = c("Suburb", "State"), by.y = c("Region", "State"))
potential.courts$Venue.Name <- potential.courts$Club.Facility.Name


# potential.courts$Venue.Name <- as.character(potential.courts$Venue.Name)
# potential.courts[grepl("Tennis Club",potential.courts$Club.Facility.Name) == T, "Venue.Name"] <- 
#   c(gsub("Tennis Club", "TC" , potential.courts[grepl("Tennis Club", potential.courts$Club.Facility.Name) == T, c("Venue.Name")]))

opening.times$Venue.Name <- standardise.club.names(opening.times$Venue.Name)

potential.courts <- merge(potential.courts, opening.times, by.x = "Club.Facility.Name",  by = "Venue.Name", all.x = T)
potential.courts$Year.OfBooking <- 2018
potential.courts$Booking.Month <- "January"
potential.courts$Booking.Day <- "Sunday"
potential.courts$recently.advertised <- FALSE


#potential.suburbs.distanceFromCBD <- read.csv("../../potential.suburbs.distanceFromCBD.csv", header = T)
#potential.courts <- merge(potential.courts, potential.suburbs.distanceFromCBD)

potential.courts <- merge(potential.courts, venues[,c("Club.Facility.Name", "DistanceToCBD.km", "RegionLocation")])
nrow(potential.courts )

# potential.courts$DistFromCBD <- as.integer(potential.courts$DistFromCBD)
# potential.courts$RegionLocation <- as.character("Remote area")
# potential.courts$RegionLocation <- as.character(potential.courts$RegionLocation)
# potential.courts[potential.courts$DistFromCBD<= 5 , "RegionLocation"] <- "Inner City"
# potential.courts[potential.courts$DistFromCBD>= 6  & potential.courts$DistFromCBD< 11 , "RegionLocation"] <- "City"
# potential.courts[potential.courts$DistFromCBD>= 11 & potential.courts$DistFromCBD< 21 , "RegionLocation"] <- "City Border"
# potential.courts[potential.courts$DistFromCBD>= 21 & potential.courts$DistFromCBD< 40 , "RegionLocation"] <- "Suburb"
# potential.courts[potential.courts$DistFromCBD>= 41 & potential.courts$DistFromCBD< 80 , "RegionLocation"] <- "Outer Suburb"
 potential.courts$MaleFemalePerc = potential.courts$Males.Total.no./potential.courts$Females.Total.no.
# 
# 

potential.courts <- merge(potential.courts, bookings.members.by.court, all.x = T)
members.by.nbr.of.courts <- members.by.court.final[members.by.court.final$nbr.courts>0 , c("nbr.courts", "MembersByCourt")]
members.by.nbr.of.courts<- members.by.nbr.of.courts %>% group_by(nbr.courts) %>% summarise(MembersByCourt = median(MembersByCourt)) 

potential.courts[is.na(potential.courts$MembersByCourt), "MembersByCourt"] <- 
  members.by.nbr.of.courts[potential.courts[is.na(potential.courts$MembersByCourt), "nbr.courts"] , "MembersByCourt"]

potential.courts <- merge(potential.courts, bookings.members.by.court, all.x = T)

nrow(potential.courts )
