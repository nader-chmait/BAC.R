library(devtools)
library(gmapsdistance)
#devtools::install_github("rodazuero/gmapsdistance")

#source("../../Gmapskey.R")
#set.api.key(APIkey)


club.distance.to.cbd <- members.by.court.final
state.CBD <- data.frame(State = c("SA",  "VIC", "QLD", "NSW", "WA",  "NT",  "TAS", "ACT" ),
                        CBD.query = c("Adelaide+South+Australia+5000",
                                       "Melbourne+Central+Railway+Station+Melbourne+VIC+3000", 
                                       "Brisbane+Queensland",
                                       "Sydney+New+South+Wales+2000", 
                                       "Perth+Western+Australia+6000", 
                                       "Darwin+City+Northern+Territory+0800", 
                                       "Hobart+Tasmania+7000", 
                                       "Canberra+Australian+Capital+Territory")
)

club.distance.to.cbd<- merge(club.distance.to.cbd, state.CBD, all.x = T)
club.distance.to.cbd$Club <- gsub(" ", "+", club.distance.to.cbd$Club.Facility.Name) 
club.distance.to.cbd$Club <- paste(club.distance.to.cbd$Club, club.distance.to.cbd$State, sep = "+")
club.distance.to.cbd$Club <- gsub("\\+Inc", "", club.distance.to.cbd$Club, ignore.case = T)
club.distance.to.cbd$Club <- gsub("\\+-", "", club.distance.to.cbd$Club, ignore.case = T)


distances.to.cbd<- read.csv("../../club.distances.to.cbd.csv")
d<- distances.to.cbd
d<- d[d$DistQueryStatus!= "OK", ]
write.csv(d, "../../d.csv")

club.distance.to.cbd <- merge(club.distance.to.cbd, distances.to.cbd)


head(club.distance.to.cbd)
transitions.perc.by.regional.location <- club.distance.to.cbd %>% group_by(RegionLocation) %>%
  summarise(total.members.by.region = sum(members), median.members.by.court = median(MembersByCourt) )

# #------------------------------------------------------------------------------------------------------
# #------------------------------------------------------------------------------------------------------
# 
# getDistToCBD <- function(orig, dest){
#   #Sys.sleep(0.1)
#   out <- (gmapsdistance(origin = orig,
#                 destination = dest,
#                 mode = "driving"))
#   #print(out)
#   
#   #write.table(out, "../../FacilityDistancesToCBD.csv", col.names = F, row.names = F, sep=",", append = TRUE)
#   return(out)
#   }
# 
# # hey <- data.frame(Time.or= c("North+Woden+Tennis+Club+ACT",
# #                      "Campbell+Tennis+Club+ACT",
# #                      "Southlands+Mawson+Tennis+Club+ACT"),
# #                      Time.Time.Canberra.Australian.Capital.Territory=c(723,
# #                          527,
# #                          1072),
# #                      c=c("North+Woden+Tennis+Club+ACT",
# #                          "Campbell+Tennis+Club+ACT",
# #                          "Southlands+Mawson+Tennis+Club+ACT"),
# #                      d=c(1, 1, 1),
# #                      e=c("a", "a", "a"))
# 
# club.distance.to.cbd$TimeToCBD <- 0
# club.distance.to.cbd$DistanceToCBD <- 0
# club.distance.to.cbd$DistQueryStatus <- NA
# 
# start.time <- Sys.time()
# #lapply(club.distance.to.cbd$Club[1:10],  getDistToCBD, dest=club.distance.to.cbd$CBD.query[1:10])
# result <- NULL
# counter <- 0
# step=1
# for (index in seq(0, nrow(club.distance.to.cbd), by=step)){
#   counter <- counter + 1 
#   i<-index + 1
#   j= min((i + step - 1), nrow(club.distance.to.cbd))
#   #print(paste(i, j, sep = "-"))
#   out <- getDistToCBD(club.distance.to.cbd$Club[i:j], club.distance.to.cbd$CBD.query[i:j])
#   club.distance.to.cbd$TimeToCBD[i:j] <- out$Time
#   club.distance.to.cbd$DistanceToCBD[i:j] <- out$Distance
#   club.distance.to.cbd$DistQueryStatus[i:j] <- out$Status
# }
# #getDistToCBD(club.distance.to.cbd$Club, club.distance.to.cbd$CBD.query)
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken
# 
# View(club.distance.to.cbd)
# club.distance.to.cbd$DistanceToCBD.km <- club.distance.to.cbd$DistanceToCBD/1000
# 
# club.distance.to.cbd$DistanceToCBD.km <- as.integer(club.distance.to.cbd$DistanceToCBD.km)
# club.distance.to.cbd[is.na(club.distance.to.cbd$DistanceToCBD.km), "DistanceToCBD.km"] <- as.integer(99999)
# club.distance.to.cbd$RegionLocation <- as.character("Remote area")
# club.distance.to.cbd$RegionLocation <- as.character(club.distance.to.cbd$RegionLocation)
# club.distance.to.cbd[club.distance.to.cbd$DistanceToCBD.km<= 5 , "RegionLocation"] <- "Inner City"
# club.distance.to.cbd[club.distance.to.cbd$DistanceToCBD.km>= 6  & club.distance.to.cbd$DistanceToCBD.km< 11 , "RegionLocation"] <- "City"
# club.distance.to.cbd[club.distance.to.cbd$DistanceToCBD.km>= 11 & club.distance.to.cbd$DistanceToCBD.km< 21 , "RegionLocation"] <- "City Border"
# club.distance.to.cbd[club.distance.to.cbd$DistanceToCBD.km>= 21 & club.distance.to.cbd$DistanceToCBD.km< 40 , "RegionLocation"] <- "Suburb"
# club.distance.to.cbd[club.distance.to.cbd$DistanceToCBD.km>= 41 & club.distance.to.cbd$DistanceToCBD.km< 80 , "RegionLocation"] <- "Outer Suburb"
# club.distance.to.cbd$RegionLocation <- as.factor(club.distance.to.cbd$RegionLocation)
# 
# 
# 
# 
# # gmapsdistance(origin = club.distance.to.cbd$Club[1],
# #               destination = club.distance.to.cbd$CBD.query[1],
# #               mode = "driving")
# # gmapsdistance(origin = "Campbell+Tennis+Club",
# #               destination = "Canberra+Australian+Capital+Territory",
# #               mode = "driving")
# 
# 
# 
# 
# DistancesToCBD <- getDistToCBD(club.distance.to.cbd$Club, club.distance.to.cbd$CBD.query)
# club.distance.to.cbd$DistToCBD <- DistancesToCBD[,2]
# club.distance.to.cbd$DistQueryStatus <- DistancesToCBD[,3]
# club.distance.to.cbd$TimeToCBDByCar <- DistancesToCBD[,1]
# 
# 
# 
# #------------------------------------------------------------------------------------------------------
# #------------------------------------------------------------------------------------------------------
# 
# results = gmapsdistance(origin = "Washington+DC",
#                         destination = "New+York+City+NY",
#                         mode = "driving")
# 
# 
# gmapsdistance(origin = "Croydon+Tennis+Club+Croydon+Park+Hewish+Road+Croydon+VIC+3136",
#                         destination = "Melbourne+Central+Railway+Station+Melbourne+VIC+3000",
#                         mode = "driving")
# 
# 
# 
# 
# 
# 
