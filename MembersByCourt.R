member.list <- read.csv("../../Member_Report.csv", header = T)
member.list$Payment.Date <- as.Date(member.list$Membership.Payment.Date, "%d/%m/%Y")
member.list$Payment.Year <- year(member.list$Payment.Date)

members.by.court<- unique(member.list[,c("Club.Name",  "Last.Name", "First.Name", "MT2ID")])
members.by.court<- members.by.court %>% group_by(Club.Name) %>% summarise(members = n())

all.venues <- venues[,c("Club.Facility.Name", "Facility.Name", "nbr.courts", "State")]
all.venues$Club.Facility.Name2 <- all.venues$Club.Facility.Name
all.venues$Club.Facility.Name2 <- gsub("Lawn Tennis Club", "LTC",  all.venues$Club.Facility.Name2, ignore.case = T)
all.venues$Club.Facility.Name2 <- gsub("Tennis Club", "TC",  all.venues$Club.Facility.Name2, ignore.case = T)
all.venues$Club.Facility.Name2 <- gsub("Tennis Center", "TC",  all.venues$Club.Facility.Name2, ignore.case = T)
all.venues$Club.Facility.Name2 <- gsub("Tennis Centre", "TC",  all.venues$Club.Facility.Name2, ignore.case = T)
all.venues$Club.Facility.Name2 <- gsub("Inc.", "",  all.venues$Club.Facility.Name2, ignore.case = T)
all.venues$Club.Facility.Name2 <- gsub("Inc", "",  all.venues$Club.Facility.Name2, ignore.case = T)
all.venues$Club.Facility.Name2 <- trimws(all.venues$Club.Facility.Name2)
all.venues$Club.Facility.Name2 <- as.character(all.venues$Club.Facility.Name2)


members.by.court$Club.Facility.Name2 <- members.by.court$Club.Name
members.by.court$Club.Facility.Name2 <- gsub("Lawn Tennis Club", "LTC",  members.by.court$Club.Facility.Name2, ignore.case = T)
members.by.court$Club.Facility.Name2 <- gsub("Tennis Club", "TC",  members.by.court$Club.Facility.Name2, ignore.case = T)
members.by.court$Club.Facility.Name2 <- gsub("Tennis Center", "TC",  members.by.court$Club.Facility.Name2, ignore.case = T)
members.by.court$Club.Facility.Name2 <- gsub("Tennis Centre", "TC",  members.by.court$Club.Facility.Name2, ignore.case = T)
members.by.court$Club.Facility.Name2 <- gsub("Inc.", "",  members.by.court$Club.Facility.Name2, ignore.case = T)
members.by.court$Club.Facility.Name2 <- gsub("Inc", "",  members.by.court$Club.Facility.Name2, ignore.case = T)
members.by.court$Club.Facility.Name2 <- trimws(members.by.court$Club.Facility.Name2)
members.by.court$Club.Facility.Name2 <- as.character(members.by.court$Club.Facility.Name2)

#table(members.by.court$Club.Facility.Name2 %in%  unique(all.venues$Club.Facility.Name2) )

members.by.court.final <- merge(members.by.court, all.venues, by= "Club.Facility.Name2", all.x = T)
members.by.court.final <- members.by.court.final[!is.na(members.by.court.final$nbr.courts), ]
members.by.court.final$MembersByCourt <- as.integer(members.by.court.final$members)/as.integer(members.by.court.final$nbr.courts)

#write.csv(members.by.court.final, "../../members.by.court.all.venues.csv")

bookings.members.by.court <- members.by.court.final[members.by.court.final$nbr.courts>0 , c("Club.Facility.Name", "Facility.Name", "Club.Name", "MembersByCourt")]
bookings.members.by.court$Facility.Name <- proper(bookings.members.by.court$Facility.Name)


#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------


existing.members <- member.list
all.existing.members <- unique(existing.members[,c("Club.Name", "First.Name", "Last.Name", "Date.of.Birth", "Gender")])
all.existing.members <-all.existing.members %>% group_by(Club.Name) %>% summarise(members = n())
all.existing.members$Club.Name <- gsub("Tennis Club", "TC",  all.existing.members$Club.Name, ignore.case = T)
all.existing.members$Club.Name <- gsub("Tennis Center", "TC",  all.existing.members$Club.Name, ignore.case = T)
all.existing.members$Club.Name <- gsub("Tennis Centre", "TC",  all.existing.members$Club.Name, ignore.case = T)
all.existing.members$Club.Name <- gsub("Inc.", "",  all.existing.members$Club.Name, ignore.case = T)
all.existing.members$Club.Name <- gsub("Inc", "",  all.existing.members$Club.Name, ignore.case = T)
all.existing.members$Club.Name <- trimws(all.existing.members$Club.Name)

source("Recommendation.R")
new.courts <- potential.courts
new.courts$Club.Facility.Name2 <- gsub("Tennis Club", "TC",  new.courts$Club.Facility.Name, ignore.case = T)
new.courts$Club.Facility.Name2 <- gsub("Tennis Center", "TC",  new.courts$Club.Facility.Name2, ignore.case = T)
new.courts$Club.Facility.Name2 <- gsub("Tennis Centre", "TC",  new.courts$Club.Facility.Name2, ignore.case = T)
new.courts$Club.Facility.Name2 <- gsub("Inc.", "",  new.courts$Club.Facility.Name2, ignore.case = T)
new.courts$Club.Facility.Name2 <- gsub("Inc", "",  new.courts$Club.Facility.Name2, ignore.case = T)
new.courts$Club.Facility.Name2 <- trimws(new.courts$Club.Facility.Name2)
new.courts$Club.Facility.Name2 <- as.character(new.courts$Club.Facility.Name2)
  
new.courts[-which(new.courts$Club.Facility.Name2 %in% all.existing.members$Club.Name), "Club.Facility.Name2"] 
new.courts <- merge(new.courts, all.existing.members, by.x = "Club.Facility.Name2", by.y = "Club.Name", all.x = T)
new.courts <- new.courts[,c("Venue.Name", "Club.Facility.Name2", "nbr.courts", "members")]

new.courts$members.by.court  <- new.courts$members/new.courts$nbr.courts
#write.csv(new.courts, "../../new.courts.members.by.court.csv")
