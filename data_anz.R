library(mlbench)
library(imputeTS)
library(caret)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(rgdal)
library(cartography)
library(sp)
library(rfm)
library(openair)
library(Hmisc)
library(ggplot2)
library(ggpubr)
library(corrplot)

######-------loading data and converting blanks to NA------######

data_anz <- read.csv('ANZ synthesised transaction dataset.csv', header=T, na.strings=c("","NA"))

#########cleaning data and checking null values#########

head(data_anz)

percent_missing<-(colSums(is.na(data_anz))/nrow(data_anz))*100
print(percent_missing)

#####removing the columns#####
data_anz<-select (data_anz,-c(2,3,8,9,22))

colSums(is.na(data_anz))

data_anz <- na.omit(data_anz) 

percent_missing<-(colSums(is.na(data_anz))/nrow(data_anz))*100
print(percent_missing)

######Finding insights######

#1.what is the average transaction amount? 

sum(data_anz$amount)
mean(data_anz$amount)


#2.How many transactions do customers make each month, on average

#converting date of type %m/%d/%Y to Date type which changes the format
data_anz$date <- as.Date(data_anz$date, format = "%m/%d/%Y" )

#split date into y m d columns
data_anz <- separate(data_anz, date, c("Year", "Month", "Day"))

#calculating the average

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

#find max and min values of age
summary(data_anz$age)

#creating column 'age_group'
data_anz$age_group <- cut(data_anz$age, breaks = c(seq(18,81, by = 3), Inf), right = FALSE)

#summerizing the data for various age groups
aggregate(data_anz$amount, by=list(age_group=data_anz$age_group),FUN=sum)


#visualization
ggplot(data_anz, aes(x = age_group, y = amount)) +
  geom_bar(fill = "#5EB296", stat = "identity") +
  theme_pubclean()+ylim(-1,50000)

#Age grp 24-27 does the most transactions


#.4 Segment the dataset by transaction date and time for year 2018

#combining day, month, year into new column
data_anz$dates <- as.Date(with(data_anz, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
data_anz$dates

date_time <- subset(data_anz, select = c("extraction","amount","dates")) %>% group_by(extraction)
date_time


#5.Visualise transaction volume and spending over the course of an average day or week.

data_anz <- data_anz %>% mutate(week = cut.Date(dates, breaks = "1 week", labels = FALSE))%>%
  arrange(dates)

Transactions_per_week<- data_anz %>%
  group_by(week) %>%
  summarise('Number_of_Transcations' = n()) %>%
  arrange(desc(`Number_of_Transcations`)) %>%
  top_n(20)

names(Transactions_per_week)[1] <- "Week_num"
names(Transactions_per_week)[2]<-"Number_of_Transcations"

#visualization 

Transaction_per_week_viz <-
  ggplot(Transactions_per_week,mapping = aes(x=reorder(Week_num, -Number_of_Transactions), y=`Number_of_Transactions`)) +
  geom_bar(stat = 'identity', fill = "#5EB296") + #coord_flip()+
  # scale_y_continuous(labels = comma) +
  geom_text(aes(label = `Number_of_Transactions`), vjust = -0.5) +
  ggtitle('States by Number of Transactions in descending order') +
  xlab('Week') +
  ylab('Number of Transactions') +
  theme_minimal() 
print(Transaction_per_week_viz) 


#6.Filtering for top 8 states by transaction
Transactions_per_state<- data_anz %>%
  group_by(merchant_state) %>%
  summarise('Number_of_Transactions' = n()) %>%
  arrange(desc(`Number_of_Transactions`)) %>%
  top_n(8)

names(Transactions_per_state)[1] <- "merchant_state"
names(Transactions_per_state)[2]<-"Number_of_Transactions"


#visualize transactions per state

Transaction_per_state_Visz <-
  ggplot(Transactions_per_state,mapping = aes(x=reorder(merchant_state, -Number_of_Transactions), y=`Number_of_Transactions`)) +
  geom_bar(stat = 'identity', fill = "#5EB296") + 
  geom_text(aes(label = `Number_of_Transactions`), vjust = -0.5) +
  ggtitle('States by Number of Transactions in descending order') +
  xlab('States') +
  ylab('Number of Transactions') +
  theme_minimal() 
print(Transaction_per_state_Visz)

summary(Transactions_per_state$Number_of_Transcations)





