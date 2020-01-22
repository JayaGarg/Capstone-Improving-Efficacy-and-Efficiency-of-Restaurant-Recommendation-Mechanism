rm(list=ls())
library(DT) # table format display of data
library(dplyr)
library(stringr)
setwd("C:\\Jaya\\GL\\capstone\\data new")

#################################################################
#Load Data(Business, Review, User)
#################################################################
#1. Get cleaned business data from Saurabh's master table
#master_business <- read.csv("Master_Data_File_V1.csv", header = TRUE, sep = ",")
master_business <- read.csv("Master_Data_File.csv", header = TRUE, sep = ",")
#attach(master_business)
#datatable(head(master_business), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
#summary(master_business)
#master_business$X <- NULL
length(unique(master_business$Business.Id))

#32601

#creating review table based on our master business table
library(RMySQL)
#Connecting to MySQL:
mydb = dbConnect(MySQL(), user='root', password='root', 
                 dbname='yelp_db', host='localhost')
on.exit(dbDisconnect(mydb))

#retrieving review data from MySQL

review_query = dbSendQuery(mydb, "select * from review")
review = fetch(review_query, n=-1)
head(review)
str(review)
summary(review)

master_review <- review %>% filter(business_id %in% master_business$Business.Id)
dim(master_review)
#2481102       9
count_reviews1 <- unique(master_review$id)
length(count_reviews1)
write.csv(master_review, file = "master_review.csv")

review_datewise_2013 <- master_review %>% filter(date >= '2013-01-01 00:00:00')
review_datewise_2013 <- review_datewise_2013 %>% filter(date < '2014-01-01 00:00:00')
dim(review_datewise_2013)
#206264      9

review_datewise_2014 <- master_review %>% filter(date >= '2014-01-01 00:00:00')
review_datewise_2014 <- review_datewise_2014 %>% filter(date < '2015-01-01 00:00:00')
dim(review_datewise_2014)
#314958      9

review_datewise_2015 <- master_review %>% filter(date >= '2015-01-01 00:00:00')
review_datewise_2015 <- review_datewise_2015 %>% filter(date < '2016-01-01 00:00:00')
dim(review_datewise_2015)
#434719      9

review_datewise_2016 <- master_review %>% filter(date >= '2016-01-01 00:00:00')
review_datewise_2016 <- review_datewise_2016 %>% filter(date < '2017-01-01 00:00:00')
dim(review_datewise_2016)
#513866      9

review_datewise_2017 <- master_review %>% filter(date >= '2017-01-01 00:00:00')
dim(review_datewise_2017)
#586526      9
write.csv(review_datewise_2013,file = "review_dateswise_2013.csv")
write.csv(review_datewise_2014,file = "review_dateswise_2014.csv")
write.csv(review_datewise_2015,file = "review_dateswise_2015.csv")
write.csv(review_datewise_2016,file = "review_dateswise_2016.csv")
write.csv(review_datewise_2017,file = "review_dateswise_2017.csv")

#creating user table based on our master review table
#retrieving user data from MySQL

user_query = dbSendQuery(mydb, "select * from user")
user = fetch(user_query, n=-1)
head(user)
str(user)
summary(user)

master_user <- user %>% filter(user$id %in% master_review$user_id)
dim(master_user)
#758138     20
count_user <- unique(master_user$id) #758138
length(count_user)
write.csv(master_user,file = "master_user.csv")

#extract user for only 2014 and 2013 data
master_user_14 <- user %>% filter(user$id %in% review_datewise_2014$user_id)
dim(master_user_14)
#135752     20
count_user <- unique(master_user_14$id) #135752
length(count_user)
write.csv(master_user_14,file = "master_user_14.csv")
