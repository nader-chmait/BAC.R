
library(arules)

# data1 <- bookings.made.v.grouped[ ,  c("State" , "Populationdensity.ERPat30June.persons.km2", "RegionLocation" , "ClubsInSuburb" , 
#                                        "recently.advertised", "Medianequivalisedtotalhouseholdincome.weekly.AUD" , "MedianAge.Persons.years", 
#                                        "WorkingAgePopulation.aged15.64years", "MaleFemalePerc", "Australiancitizen.Perc", "Booking.Month" , 
#                                        "Year.OfBooking" , "Booking.Day","Total.Opening.Hours", "Facility.Type" , "Organisation.Type" ,
#                                        "nbr.courts",  "Lighted.Full.Count.All","has.Lights", "has.indoor", "has.outdoor", "has.hard", 
#                                        "has.clay", "has.grass", "has.hot.shot", "Utilisation.Category")]
# data1[,2] <- discretize(data1[,2], breaks = 4)
# data1[,4] <- factor(data1[,4])
# data1[,6] <- discretize(data1[,6], breaks = 4)
# data1[,7] <- discretize(data1[,7], breaks = 4)
# data1[,8] <- discretize(data1[,8], breaks = 4)
# data1[,9] <- discretize(data1[,9], breaks = 4)
# data1[,10] <- discretize(data1[,10])
# data1[,12] <- factor(data1[,12])
# data1[,14] <- discretize(data1[,14], breaks = 4)
# data1[,17] <- discretize(data1[,17], breaks = 4)
# data1[,18] <- discretize(data1[,18], breaks = 4)
# data1[,26] <- factor(data1[,26])
# 
# data1 <- bookings.made.v.grouped[ ,  c("Populationdensity.ERPat30June.persons.km2", "RegionLocation" , "ClubsInSuburb" , 
#                                        "recently.advertised", "Medianequivalisedtotalhouseholdincome.weekly.AUD" , "MedianAge.Persons.years", 
#                                        "WorkingAgePopulation.aged15.64years", "MaleFemalePerc", "Australiancitizen.Perc", "Total.Opening.Hours", 
#                                        "nbr.courts",  "has.Lights", "has.clay", "has.grass", "has.hot.shot", "has.indoor", "has.hard", "Utilisation.Category")]

data1 <- data.model[ ,c("Populationdensity.ERPat30June.persons.km2", "RegionLocation" , "ClubsInSuburb" , "recently.advertised", 
                                     "Medianequivalisedtotalhouseholdincome.weekly.AUD" , "MedianAge.Persons.years", 
                                     "WorkingAgePopulation.aged15.64years", "MaleFemalePerc", "Australiancitizen.Perc", "Total.Opening.Hours", 
                                     "nbr.courts",  "court.options", "MembersByCourt",  "Total.Bookings.Category")]#"Utilisation.Category",

data1[,1] <- discretize(data1[,1], breaks = 5, labels = c("very.low", "low", "med", "high", "very.high"))
data1[,3] <- factor(data1[,3])
data1[,5] <- discretize(data1[,5], breaks = 5, labels = c("very.low", "low", "med", "high", "very.high"))
data1[,6] <- discretize(data1[,6], breaks = 5, labels = c("very.low", "low", "med", "high", "very.high"))
data1[,7] <- discretize(data1[,7], breaks = 5, labels = c("very.low", "low", "med", "high", "very.high"))
data1[,8] <- discretize(data1[,8], breaks = 5, labels = c("very.low", "low", "med", "high", "very.high"))
data1[,9] <- discretize(data1[,9], breaks = 5, labels = c("very.low", "low", "med", "high", "very.high"))
data1[,10] <- discretize(data1[,10], breaks = 3, labels = c( "low", "med", "high"))
data1[,11] <- discretize(data1[,11], breaks = 5, labels = c("very.low", "low", "med", "high", "very.high"))
data1[,12] <- discretize(data1[,12], breaks = 3, labels = c( "low", "med", "high"))
data1[,13] <- discretize(data1[,13], breaks = 5, labels = c("very.low", "low", "med", "high", "very.high"))
#data1[, "Utilisation.Category"] <- factor(data1[,"Utilisation.Category"])
data1[, "Total.Bookings.Category"] <- factor(data1[,"Total.Bookings.Category"])
tData <- as (data1, "transactions") # convert to 'transactions' class

LIST(head(tData, 5))
frequentItems <- eclat (tData, parameter = list(supp = 0.07, maxlen = 10)) # calculates support for frequent items
inspect(frequentItems)
itemFrequencyPlot(tData, topN=20, type="absolute", main="Item Frequency") # plot frequent items

# rules <- apriori (tData, parameter = list(supp = 0.001, conf = 0.5)) # Min Support as 0.001, confidence as 0.8.
# rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
# inspect(head(rules_conf)) # show the support, lift and confidence for all rules
# 
# rules_lift <- sort (rules, by="lift", decreasing=TRUE) # 'high-lift' rules.
# inspect(head(rules_lift)) # show the support, lift and confidence for all rules


#-------
high.utilisation <- apriori(tData, parameter = list(target = "frequent",  supp=0.001))

rules <- apriori (data=tData, parameter=list (supp=0.001, conf = 0.2, minlen=2, maxlen=5), appearance = list (default="lhs",rhs="Total.Bookings.Category=+2"), control = list (verbose=F)) # get rules that lead to buying 'whole milk'
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf, 40))


rules_lift <- sort (rules, by="lift", decreasing=TRUE) # 'high-lift' rules.
inspect(head(rules_lift)) 

subsetRules <- which(colSums(is.subset(rules, rules)) > 1) # get subset rules in vector
length(subsetRules)  #> 3913
rules <- rules[-subsetRules]
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf, 10))


