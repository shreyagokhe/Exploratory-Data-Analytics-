library(mlbench)
library(imputeTS)
library(caret)
library(dplyr)
library(tidyverse)
library(lubridate)
library(rgdal)
library(cartography)
library(sp)
library(rfm)
library(openair)
#library(extrafont)
#library(scales)
#library(ggridges)
#library(corrplot)
library(Hmisc)
#library(reshape2)
library(ggplot2)
library(ggpubr)
#library(forecast)
#library(corrr)

library(corrplot)

#loading data
#converting blanks to NA
data_anz <- read.csv('ANZ synthesised transaction dataset.csv', header=T, na.strings=c("","NA"))

#cleaning data and checking null values
head(data_anz)

percent_missing<-(colSums(is.na(data_anz))/nrow(data_anz))*100
print(percent_missing)

colnames(data_anz)


#removing the columns
data_anz<-select (data_anz,-c(2,3,8,9,22))

#data_anz %>% drop_na(bpay_biller_code)
#data_anz %>% mutate_all(~ifelse(. %in% c("N/A", "null", ""), NA, .)) %>% na.omit()
  #na.omit(data_anz, cols=c("bpay_biller_code","merchant_id","merchant_suburb","merchant_state","merchant_long_lat"))



colSums(is.na(data_anz))

data_anz <- na.omit(data_anz) 

percent_missing<-(colSums(is.na(data_anz))/nrow(data_anz))*100
print(percent_missing)
#Find insights

#1.what is the average transaction amount? 

sum(data_anz$amount)
mean(data_anz$amount)


#2.How many transactions do customers make each month, on average?
summary(data_anz$date)

#converting date of type %m/%d/%Y to Date type which changes the format
data_anz$date <- as.Date(data_anz$date, format = "%m/%d/%Y" )

months(as.Date("2018-08-01")) 

#--split date into y m d columns
#data_anz%>% separate(data_anz$date, sep = "/",into = c("year", "month", "day"))
data_anz <- separate(data_anz, date, c("Year", "Month", "Day"))


newdata_8 <- subset(data_anz, Month=="08",select=transaction_id)
newdata_9 <- subset(data_anz, Month =="09",select= transaction_id)
newdata_10 <- subset(data_anz, Month =="10", select = transaction_id)

data_8 <-count(newdata_8)
data_9 <-count(newdata_9)
data_10 <-count(newdata_10)

mean_8 <- data_8/31
mean_9 <- data_9/30
mean_10 <- data_10/31

cat('Average for August',as.numeric(mean_8))
cat("Average for September",as.numeric(mean_9)) 
cat("Average for October", as.numeric(mean_10))

#3.What age group does the most transactions

#find max and min of ages
summary(data_anz$age)

#age_group <-c(18,28,38,48,58,68,78)

#creating column age group
data_anz$age_group <- cut(data_anz$age, breaks = c(seq(18,81, by = 3), Inf), right = FALSE)


#summerizing the data for various age groups
aggregate(data_anz$amount, by=list(age_group=data_anz$age_group),FUN=sum)

#visualization
ggplot(data_anz, aes(x = age_group, y = amount)) +
  geom_bar(fill = "#5EB296", stat = "identity") +
  theme_pubclean()+ylim(-1,50000)


#transaction age
hist(data_anz$age,
     main="HistoGram for Transaction age",
     xlab="Transaction age Class",
     ylab="Frequency",
     col="#5EB296",
     labels=TRUE)

##boxplot(data_anz$gender, col = 'blue',main='Analysis for Boxplot')

#Age grp 24-27 does the most transactions


#.4 Segment the dataset by transaction date and time for year 2018

date_time <- subset(data_anz, select = c("extraction","amount","gender")) %>% group_by(extraction)
date_time






#5.Visualise transaction volume and spending over the course of an average day or week.

#combining day n month into new column
data_anz$date <- as.Date(with(data_anz, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
data_anz$date

data_anz <- data_anz %>% mutate(week = cut.Date(date, breaks = "1 week", labels = FALSE))%>%
  arrange(date)

Transactions_per_week<- data_anz %>%
  group_by(week) %>%
  summarise('Number_of_Transcations' = n()) %>%
  arrange(desc(`Number_of_Transcations`)) %>%
  top_n(20)

names(Transactions_per_week)[1] <- "Week_num"
names(Transactions_per_week)[2]<-"Number_of_Transcations"



Transaction_per_week_viz <-
  ggplot(Transactions_per_week,mapping = aes(x=reorder(Week_num, -Number_of_Transcations), y=`Number_of_Transcations`)) +
  geom_bar(stat = 'identity', fill = "#5EB296") + #coord_flip()+
  # scale_y_continuous(labels = comma) +
  geom_text(aes(label = `Number_of_Transcations`), vjust = -0.5) +
  ggtitle('States by Number of Transactions in descending order') +
  xlab('Week') +
  ylab('Number of Transactions') +
  theme_minimal() 
print(Transaction_per_week_viz) 






#choropleth map with the Cartography package--- research on tht

Aus = readOGR(dsn=".", layer="States Map")
Aus@data$id = rownames(Aus@data)
Aus.points = fortify(Aus, region="merchant_state")
Aus.df = join(Aus.points, Aus@data, by="merchant_state")







summary(data_anz$merchant_state)
##### Filtering for top 10 countries by transaction
Transactions_per_state<- data_anz %>%
  group_by(merchant_state) %>%
  summarise('Number_of_Transcations' = n()) %>%
  arrange(desc(`Number_of_Transcations`)) %>%
  top_n(8)

names(Transactions_per_state)[1] <- "merchant_state"
names(Transactions_per_state)[2]<-"Number_of_Transcations"

#visualize transactions per state


library(ggthemes)


Transaction_per_state_Visz <-
  ggplot(Transactions_per_state,mapping = aes(x=reorder(merchant_state, -Number_of_Transcations), y=`Number_of_Transcations`)) +
  geom_bar(stat = 'identity', fill = "#5EB296") + #coord_flip()+
# scale_y_continuous(labels = comma) +
  geom_text(aes(label = `Number_of_Transcations`), vjust = -0.5) +
  ggtitle('States by Number of Transactions in descending order') +
  xlab('States') +
  ylab('Number of Transactions') +
  theme_minimal() 
print(Transaction_per_state_Visz)

summary(Transactions_per_state$Number_of_Transcations)










