

members.by.court.by.gender<- unique(member.list[,c("Club.Name",  "Last.Name", "First.Name",  "Gender",  "Date.of.Birth", "MT2ID")])
members.by.court.by.gender<- members.by.court.by.gender %>% group_by(Club.Name, Gender) %>% summarise(members = n())

members.by.court.by.gender$Club.Facility.Name2 <- members.by.court.by.gender$Club.Name
members.by.court.by.gender$Club.Facility.Name2 <- gsub("Lawn Tennis Club", "LTC",  members.by.court.by.gender$Club.Facility.Name2, ignore.case = T)
members.by.court.by.gender$Club.Facility.Name2 <- gsub("Tennis Club", "TC",  members.by.court.by.gender$Club.Facility.Name2, ignore.case = T)
members.by.court.by.gender$Club.Facility.Name2 <- gsub("Tennis Center", "TC",  members.by.court.by.gender$Club.Facility.Name2, ignore.case = T)
members.by.court.by.gender$Club.Facility.Name2 <- gsub("Tennis Centre", "TC",  members.by.court.by.gender$Club.Facility.Name2, ignore.case = T)
members.by.court.by.gender$Club.Facility.Name2 <- gsub("Inc.", "",  members.by.court.by.gender$Club.Facility.Name2, ignore.case = T)
members.by.court.by.gender$Club.Facility.Name2 <- gsub("Inc", "",  members.by.court.by.gender$Club.Facility.Name2, ignore.case = T)
members.by.court.by.gender$Club.Facility.Name2 <- trimws(members.by.court.by.gender$Club.Facility.Name2)
members.by.court.by.gender$Club.Facility.Name2 <- as.character(members.by.court.by.gender$Club.Facility.Name2)


members.by.court.by.gender.final <- merge(members.by.court.by.gender, all.venues, by= "Club.Facility.Name2", all.x = T)
members.by.court.by.gender.final <- members.by.court.by.gender.final[!is.na(members.by.court.by.gender.final$nbr.courts), ]
members.by.court.by.gender.final$MembersByCourt <- as.integer(members.by.court.by.gender.final$members)/as.integer(members.by.court.by.gender.final$nbr.courts)

members.by.gender  <- members.by.court.by.gender.final %>% group_by(Club.Facility.Name, Club.Facility.Name2, Club.Name, State) %>%
  mutate(total.members = sum(members)) %>% ungroup()

members.gender.ratio <- members.by.gender[members.by.gender$Gender=="Male", ] %>% mutate(male.member.ratio = (members/total.members)*100)

members.gender.ratio$Facility.Name <- proper(members.gender.ratio$Facility.Name)
# bookings.members.by.court.by.gender <- members.by.court.by.gender.final[members.by.court.by.gender.final$nbr.courts>0 , c("Club.Facility.Name", "Facility.Name", "Club.Name", "MembersByCourt")]
# bookings.members.by.court.by.gender$Facility.Name <- proper(bookings.members.by.court.by.gender$Facility.Name)
# 
# 
# #------------------------------------------------------------------------------------------------------
# #------------------------------------------------------------------------------------------------------
# 
# 
# existing.members <- member.list
# all.existing.members <- unique(existing.members[,c("Club.Name", "First.Name", "Last.Name", "Date.of.Birth", "Gender")])
# all.existing.members <-all.existing.members %>% group_by(Club.Name) %>% summarise(members = n())
# all.existing.members$Club.Name <- gsub("Tennis Club", "TC",  all.existing.members$Club.Name, ignore.case = T)
# all.existing.members$Club.Name <- gsub("Tennis Center", "TC",  all.existing.members$Club.Name, ignore.case = T)
# all.existing.members$Club.Name <- gsub("Tennis Centre", "TC",  all.existing.members$Club.Name, ignore.case = T)
# all.existing.members$Club.Name <- gsub("Inc.", "",  all.existing.members$Club.Name, ignore.case = T)
# all.existing.members$Club.Name <- gsub("Inc", "",  all.existing.members$Club.Name, ignore.case = T)
# all.existing.members$Club.Name <- trimws(all.existing.members$Club.Name)
# 
# source("Recommendation.R")
# new.courts <- potential.courts
# new.courts$Club.Facility.Name2 <- gsub("Tennis Club", "TC",  new.courts$Club.Facility.Name, ignore.case = T)
# new.courts$Club.Facility.Name2 <- gsub("Tennis Center", "TC",  new.courts$Club.Facility.Name2, ignore.case = T)
# new.courts$Club.Facility.Name2 <- gsub("Tennis Centre", "TC",  new.courts$Club.Facility.Name2, ignore.case = T)
# new.courts$Club.Facility.Name2 <- gsub("Inc.", "",  new.courts$Club.Facility.Name2, ignore.case = T)
# new.courts$Club.Facility.Name2 <- gsub("Inc", "",  new.courts$Club.Facility.Name2, ignore.case = T)
# new.courts$Club.Facility.Name2 <- trimws(new.courts$Club.Facility.Name2)
# new.courts$Club.Facility.Name2 <- as.character(new.courts$Club.Facility.Name2)
# 
# new.courts[-which(new.courts$Club.Facility.Name2 %in% all.existing.members$Club.Name), "Club.Facility.Name2"] 
# new.courts <- merge(new.courts, all.existing.members, by.x = "Club.Facility.Name2", by.y = "Club.Name", all.x = T)
# new.courts <- new.courts[,c("Venue.Name", "Club.Facility.Name2", "nbr.courts", "members")]
# 
# new.courts$members.by.court.by.gender  <- new.courts$members/new.courts$nbr.courts
# #write.csv(new.courts, "../../new.courts.members.by.court.by.gender.csv")
