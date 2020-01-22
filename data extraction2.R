rm(list=ls())
library(DT) # table format display of data
library(ggplot2)
library(gridExtra)
library(dplyr)
library(stringr)
setwd("C:\\Jaya\\GL\\capstone\\data")

#################################################################
#Load Data(Business, Review, User)
#################################################################
#1. Get cleaned business data from Saurabh's master table
#master_business <- read.csv("Master_Data_File_V1.csv", header = TRUE, sep = ",")
master_business <- read.csv("Master_Data_File.csv", header = TRUE, sep = ",")
attach(master_business)
datatable(head(master_business), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
summary(master_business)
master_business$X <- NULL
count_business <- unique(master_business$Business.Id)
length(count_business)

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
#3096720       9
count_reviews1 <- unique(master_review$id)
length(count_reviews1)
write.csv(master_review, file = "master_review.csv")

#creating user table based on our master review table
#retrieving user data from MySQL

user_query = dbSendQuery(mydb, "select * from user")
user = fetch(user_query, n=-1)
head(user)
str(user)
summary(user)

master_user <- user %>% filter(user$id %in% master_review$user_id)
dim(master_user)
count_user <- unique(master_user$id)
length(count_user)
write.csv(master_user,file = "master_user.csv")

#
datatable(head(master_user), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
summary(master_user)