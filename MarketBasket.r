setwd("/home/jzavgren/Documents/NEC_Mining/MarketBasketAnalysis")
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(plyr)

retail <- read_excel('Online_retail.xlsx')
retail <- retail[complete.cases(retail), ]
retail <- retail %>% mutate(Description = as.factor(Description))
retail <- retail %>% mutate(Country = as.factor(Country))
retail$Date <- as.Date(retail$InvoiceDate)
retail$Time <- format(retail$InvoiceDate,"%H:%M:%S")
retail$InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))
glimpse(retail)
#Let's get the distribution of shopping times.
retail$Time <- as.factor(retail$Time)
a <- hms(as.character(retail$Time))
retail$Time = hour(a)
retail %>% 
  ggplot(aes(x=Time)) + 
  geom_histogram(stat="count",fill="indianred")
#How many items each customer buy?
detach("package:plyr", unload=TRUE)
retail %>% 
group_by(InvoiceNo) %>% 
summarize(n_items = mean(Quantity)) %>%
ggplot(aes(x=n_items))+
geom_histogram(fill="indianred", bins = 100000) + 
geom_rug()+
coord_cartesian(xlim=c(0,80))

#People mostly purchased less than 10 items (less than 10 items in each invoice).

#Top 10 best sellers

tmp <- retail %>% 
  group_by(StockCode, Description) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))
tmp <- head(tmp, n=10)
tmp
tmp %>% 
  ggplot(aes(x=reorder(Description,count), y=count))+
  geom_bar(stat="identity",fill="indian red")+
  coord_flip()

#Association rules for online retailer
retail_sorted <- retail[order(retail$CustomerID),]
library(plyr)
itemList <- ddply(retail,c("CustomerID","Date"), 
                  function(df1)paste(df1$Description, 
                                     collapse = ","))
itemList$CustomerID <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("items")
#Now, read them back in.
write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = TRUE)
tr <- read.transactions('market_basket.csv', format = 'basket', sep=',')
tr
summary(tr)

itemFrequencyPlot(tr, topN=20, type='absolute')

rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)
inspect(rules[1:10])
topRules <- rules[1:10]
plot(topRules)
plot(topRules, method="graph")
plot(topRules, method = "grouped")