
search.terms.by.date <- read.csv("../../search.terms.by.date.csv", header = T)

search.terms.by.date$Date <- as.Date(search.terms.by.date$Day.Index, format = "%d/%m/%y")
search.terms.by.date$Day <- weekdays(search.terms.by.date$Date)
search.terms.by.date$Month <- months(search.terms.by.date$Date)
search.terms.by.date$Year <- year(search.terms.by.date$Date)
str(search.terms.by.date)

search.terms.by.day <- search.terms.by.date %>% group_by(Day) %>% summarise(Total.Unique.Searches = mean(Total.Unique.Searches))
search.terms.by.Month <- search.terms.by.date %>% group_by(Month) %>% summarise(Total.Unique.Searches = mean(Total.Unique.Searches))
search.terms.by.Year <- search.terms.by.date %>% group_by(Year) %>% summarise(Total.Unique.Searches = mean(Total.Unique.Searches))

table(search.terms.by.date$Day)
table(search.terms.by.date$Month)
table(search.terms.by.date$Year)

ggplot(data=search.terms.by.day[!is.na(search.terms.by.day$Day), ], aes(x=reorder(Day, Total.Unique.Searches), y=Total.Unique.Searches)) + 
  geom_col() + xlab("") + ylab("Avg # of search queries")
ggplot(data=search.terms.by.Month[!is.na(search.terms.by.Month$Month), ], aes(x=reorder(Month, Total.Unique.Searches), y=Total.Unique.Searches)) + geom_col() + xlab("") + ylab("Avg # of search queries")
ggplot(data=search.terms.by.Year[!is.na(search.terms.by.Year$Year), ], aes(x=reorder(Year, Total.Unique.Searches), y=Total.Unique.Searches)) + geom_col() + xlab("") + ylab("Avg # of search queries")


search.terms.by.keyword <- read.csv("../../search.terms.by.keyword.csv", header = T)
search.terms.by.keyword$keyword <- proper(as.character(search.terms.by.keyword$Search.Term))
search.terms.by.keyword$Search.Exits.Perc <- as.numeric(gsub("%","" ,search.terms.by.keyword$X..Search.Exits))
search.terms.by.keyword$Search.Refinements.Perc <- as.numeric(gsub("%","" ,search.terms.by.keyword$X..Search.Refinements))
str(search.terms.by.keyword)


search.terms.by.keyword.grouped <- search.terms.by.keyword[search.terms.by.keyword$keyword != "",-c(which(names(search.terms.by.keyword) %in% c("Search.Term", "X..Search.Exits", "X..Search.Refinements")))] 
search.terms.by.keyword.grouped <- search.terms.by.keyword.grouped %>% group_by(keyword) %>% summarise_all(funs(mean))
search.terms.by.keyword.grouped$keyword.PostCode <-  as.character(gsub("[^\\d]+", "", search.terms.by.keyword.grouped$keyword, perl=TRUE)) 
search.terms.by.keyword.post.code <- search.terms.by.keyword.grouped %>% group_by(keyword.PostCode) %>% summarise(Total.Unique.Searches =sum(Total.Unique.Searches),
                                                                                                                  Mean.Time.after.Search = mean(Time.after.Search),
                                                                                                                  Tot.Time.after.Search = sum(Time.after.Search),
                                                                                                                  region = tail(keyword,1))

#post.codes<- 

ggplot(data=search.terms.by.keyword.post.code[search.terms.by.keyword.post.code$Total.Unique.Searches > 500, ], #!is.na(search.terms.by.keyword.grouped$keyword.PostCode)
       aes(x=reorder(region, Total.Unique.Searches), y=Total.Unique.Searches, label=(Total.Unique.Searches) )) + geom_col() + geom_text() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("")

ggplot(data=search.terms.by.keyword.post.code[search.terms.by.keyword.post.code$Mean.Time.after.Search > 300, ], #!is.na(search.terms.by.keyword.grouped$keyword.PostCode)
       aes(x=reorder(region, Mean.Time.after.Search), y=Mean.Time.after.Search, label=(round(Mean.Time.after.Search)))) + geom_col() + #geom_text() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("")

ggplot(data=search.terms.by.keyword.post.code[search.terms.by.keyword.post.code$Tot.Time.after.Search > 400, ], #!is.na(search.terms.by.keyword.grouped$keyword.PostCode)
       aes(x=reorder(region, Tot.Time.after.Search), y=Tot.Time.after.Search, label=(round(Tot.Time.after.Search)))) + geom_col() + #geom_text() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("")

#View(search.terms.by.keyword.post.code) 



