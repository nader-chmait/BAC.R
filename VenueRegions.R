

venue.regions <- read.csv("venueRegions.csv", header = T)

# venue.regions$DistFromCBD <- as.integer(venue.regions$DistFromCBD)
# venue.regions$RegionLocation <- as.character("Remote area")
# venue.regions$RegionLocation <- as.character(venue.regions$RegionLocation)
# venue.regions[venue.regions$DistFromCBD<= 5 , "RegionLocation"] <- "Inner City"
# venue.regions[venue.regions$DistFromCBD>= 6  & venue.regions$DistFromCBD< 11 , "RegionLocation"] <- "City"
# venue.regions[venue.regions$DistFromCBD>= 11 & venue.regions$DistFromCBD< 21 , "RegionLocation"] <- "City Border"
# venue.regions[venue.regions$DistFromCBD>= 21 & venue.regions$DistFromCBD< 40 , "RegionLocation"] <- "Suburb"
# venue.regions[venue.regions$DistFromCBD>= 41 & venue.regions$DistFromCBD< 80 , "RegionLocation"] <- "Outer Suburb"