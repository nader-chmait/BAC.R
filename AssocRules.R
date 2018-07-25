
library(arules)


# 
# data1 <- bookings.made.v.grouped[ ,  c("Populationdensity.ERPat30June.persons.km2", "RegionLocation" , "ClubsInSuburb" , 
#                                        "recently.advertised", "Medianequivalisedtotalhouseholdincome.weekly.AUD" , "MedianAge.Persons.years", 
#                                        "WorkingAgePopulation.aged15.64years", "MaleFemalePerc", "Australiancitizen.Perc", "Total.Opening.Hours", 
#                                        "nbr.courts",  "has.Lights", "has.clay", "has.grass", "has.hot.shot", "has.indoor", "has.hard", "Utilisation.Category")]

data1 <- data.model[ ,c( "State","Populationdensity.ERPat30June.persons.km2", "DistanceToCBD.km.truncated",
                          "recently.advertised" , "Medianequivalisedtotalhouseholdincome.weekly.AUD" , "MedianAge.Persons.years", 
                          "WorkingAgePopulation.aged15.64years", "MaleFemalePerc", "Australiancitizen.Perc", 
                          "has.Lights", "has.hot.shot", "MembersByCourt", "court.options", "SecularBeliefs.Perc",
                           "Utilisation.Category")]#"Utilisation.Category",


nums <- unlist(lapply(data1, is.numeric))  
#dplyr::select_if(data1, is.numeric)

for(i in which(nums)){
  data1[,i] <- discretize(data1[, i], breaks = 3, labels = c("low", "med", "high"))
  print(i)
}

data1[, "Utilisation.Category"] <- factor(data1[,"Utilisation.Category"])
tData <- as (data1, "transactions") # convert to 'transactions' class


# data1[,1] <- discretize(data1[,1], breaks = 5, labels = c("very.low", "low", "med", "high", "very.high"))
# data1[,3] <- factor(data1[,3])




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
#high.utilisation <- apriori(tData, parameter = list(target = "frequent",  supp=0.001))

rules <- apriori (data=tData, parameter=list (supp=0.001, conf = 0.2, minlen=2, maxlen=5), appearance = list (default="lhs",rhs="Utilisation.Category=+2"), control = list (verbose=F)) # get rules that lead to buying 'whole milk'
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf, 40))

subsetRules <- which(colSums(is.subset(rules, rules)) > 1) # get subset rules in vector
length(subsetRules)  #> 3913
rules <- rules[-subsetRules]
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf, 10))



rules_lift <- sort (rules, by="lift", decreasing=TRUE) # 'high-lift' rules.
inspect(head(rules_lift)) 



