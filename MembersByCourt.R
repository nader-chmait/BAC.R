member.list <- read.csv("../../Member_Report.csv", header = T)
member.list$Payment.Date <- as.Date(member.list$Membership.Payment.Date, "%d/%m/%Y")
member.list$Payment.Year <- year(member.list$Payment.Date)

rm(list= c("members.by.court", "all.venues", "members.by.court.final"))

members.by.court<- unique(member.list[,c("Club.Name",  "Last.Name", "First.Name", "MT2ID")])
members.by.court<- members.by.court %>% group_by(Club.Name) %>% summarise(members = n())

all.venues <- venues[,c("Club.Facility.Name", "Facility.Name", "nbr.courts", "State")]
all.venues[nrow(all.venues) +1, ] <- all.venues[all.venues$Club.Facility.Name == "Mcc Roy Street St Kilda" & all.venues$Facility.Name == "MCC TC", ]
all.venues[nrow(all.venues), "Club.Facility.Name"] <- "MCC TC"
all.venues[nrow(all.venues) +1, ] <- all.venues[all.venues$Club.Facility.Name == "Reid Park Tennis Courts" & all.venues$Facility.Name == "Bar Beach Tennis Club Inc.", ]
all.venues[nrow(all.venues), "Club.Facility.Name"] <- "Bar Beach Tennis Club"

all.venues$Club.Facility.Name2 <- standardise.club.names(all.venues$Club.Facility.Name)
members.by.court$Club.Facility.Name2 <- standardise.club.names(members.by.court$Club.Name)

#table(unique(members.by.court$Club.Facility.Name2) %in%  unique(all.venues$Club.Facility.Name2) )


members.by.court.final <- merge(members.by.court, all.venues, by= "Club.Facility.Name2", all.x = T)
#View(members.by.court.final[is.na(members.by.court.final$Club.Facility.Name),])
#summary(is.na(members.by.court.final$Club.Facility.Name))


members.by.court.final <- members.by.court.final[!is.na(members.by.court.final$nbr.courts), ]
members.by.court.final$MembersByCourt <- as.integer(members.by.court.final$members)/as.integer(members.by.court.final$nbr.courts)


bmc <- members.by.court.final[, c("Club.Facility.Name", "Club.Facility.Name2","Facility.Name", "Club.Name", "MembersByCourt")]
bmc$Facility.Name <- proper(bmc$Facility.Name)
bmc$Club.Facility.Name2 <- proper(bmc$Club.Facility.Name2)

#write.csv(members.by.court.final, "../../members.by.court.all.venues.csv")


bookings.members.by.court <- members.by.court.final[members.by.court.final$nbr.courts>0 , c("Club.Facility.Name", "Club.Facility.Name2","Facility.Name", "Club.Name", "MembersByCourt")]
bookings.members.by.court$Facility.Name <- proper(bookings.members.by.court$Facility.Name)
bookings.members.by.court$Club.Facility.Name2 <- proper(bookings.members.by.court$Club.Facility.Name2)

#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------


existing.members <- member.list
all.existing.members <- unique(existing.members[,c("Club.Name", "First.Name", "Last.Name", "Date.of.Birth", "Gender")])
all.existing.members <-all.existing.members %>% group_by(Club.Name) %>% summarise(members = n())

all.existing.members$Club.Name <- standardise.club.names(all.existing.members$Club.Name)
  

source("Recommendation.R")
new.courts <- potential.courts
new.courts$Club.Facility.Name2 <- standardise.club.names(new.courts$Club.Facility.Name)

new.courts[-which(new.courts$Club.Facility.Name2 %in% all.existing.members$Club.Name), "Club.Facility.Name2"] 
new.courts <- merge(new.courts, all.existing.members, by.x = "Club.Facility.Name2", by.y = "Club.Name", all.x = T)
new.courts <- new.courts[,c("Venue.Name", "Club.Facility.Name2", "nbr.courts", "members")]

new.courts$members.by.court  <- new.courts$members/new.courts$nbr.courts
#write.csv(new.courts, "../../new.courts.members.by.court.csv")
