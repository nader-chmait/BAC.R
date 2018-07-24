

members.by.court.by.gender.init <- unique(member.list[,c("Club.Name",  "Last.Name", "First.Name",  "Gender",  "Date.of.Birth", "MT2ID")])

rm(list= c("members.by.court.by.gender", "members.by.court.by.gender.final", "members.by.gender"))
members.by.court.by.gender<- members.by.court.by.gender.init %>% group_by(Club.Name, Gender) %>% summarise(members = n())

members.by.court.by.gender$Club.Facility.Name2 <- standardise.club.names(members.by.court.by.gender$Club.Name)


members.by.court.by.gender.final <- merge(members.by.court.by.gender, all.venues, by= "Club.Facility.Name2", all.x = T)
members.by.court.by.gender.final <- members.by.court.by.gender.final[!is.na(members.by.court.by.gender.final$nbr.courts), ]
members.by.court.by.gender.final$MembersByCourt <- as.integer(members.by.court.by.gender.final$members)/as.integer(members.by.court.by.gender.final$nbr.courts)

# #------------------------------------------------------------------------------------------------------
# #------------------------------------------------------------------------------------------------------
#Find male ratio of members by club 

members.by.gender  <- members.by.court.by.gender.final %>% group_by(Club.Facility.Name, Club.Facility.Name2, Club.Name, State) %>%
  mutate(total.members = sum(members)) %>% ungroup()

members.gender.ratio <- members.by.gender[members.by.gender$Gender=="Male", ] %>% mutate(male.member.ratio = (members/total.members)*100)

members.gender.ratio$Facility.Name <- proper(members.gender.ratio$Facility.Name)


# #------------------------------------------------------------------------------------------------------
# #------------------------------------------------------------------------------------------------------
#Find mean age of members by club 
library(eeptools)
data1<- unique(member.list[,c("Club.Name",  "Last.Name", "First.Name", "Date.of.Birth", "MT2ID")])
data1$DOB <- as.Date(data1$Date.of.Birth, format = "%d/%m/%Y")
data1<- data1[!is.na(data1$DOB) & (data1$DOB < "2010-01-01" & data1$DOB > "1920-01-01"), ] 
#data1[is.na(data1$DOB) | data1$DOB > "2010-01-01","DOB"] <- as.Date("2000-01-01")
data1$Age <- floor(age_calc(data1$DOB, units = "years"))


data2<- data1 %>% group_by(Club.Name) %>% summarise(Mean.Age = mean(Age), Median.Age = median(Age))

data2$Club.Facility.Name2 <-standardise.club.names(data2$Club.Name)

data2.final <- merge(data2, all.venues, by= "Club.Facility.Name2", all.x = T)
members.ages.by.club <- data2.final#[!is.na(data2.final$nbr.courts), ]

members.ages.by.club$Facility.Name <- proper(members.ages.by.club$Facility.Name)

# #------------------------------------------------------------------------------------------------------
# #------------------------------------------------------------------------------------------------------
# 
