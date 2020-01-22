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
#46973

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
#2943230       9
count_reviews1 <- unique(master_review$id)
length(count_reviews1)
write.csv(master_review, file = "master_review.csv")

review_datewise_2013 <- master_review %>% filter(date >= '2013-01-01 00:00:00')
review_datewise_2013 <- review_datewise_2013 %>% filter(date < '2014-01-01 00:00:00')
dim(review_datewise_2013)
#247286      9

review_datewise_2014 <- master_review %>% filter(date >= '2014-01-01 00:00:00')
review_datewise_2014 <- review_datewise_2014 %>% filter(date < '2015-01-01 00:00:00')
dim(review_datewise_2014)
#369462      9

review_datewise_2015 <- master_review %>% filter(date >= '2015-01-01 00:00:00')
review_datewise_2015 <- review_datewise_2015 %>% filter(date < '2016-01-01 00:00:00')
dim(review_datewise_2015)
#513708      9

review_datewise_2016 <- master_review %>% filter(date >= '2016-01-01 00:00:00')
review_datewise_2016 <- review_datewise_2016 %>% filter(date < '2017-01-01 00:00:00')
dim(review_datewise_2016)
#609359      9

review_datewise_2017 <- master_review %>% filter(date >= '2017-01-01 00:00:00')
dim(review_datewise_2017)
#686672      9
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
#865880     20
count_user <- unique(master_user$id) #865880
length(count_user)
write.csv(master_user,file = "master_user.csv")

#extract user for only 2014 and 2013 data
master_user_14 <- user %>% filter(user$id %in% review_datewise_2014$user_id)
dim(master_user_14)
#155382     20
count_user <- unique(master_user_14$id) #865880
length(count_user)
write.csv(master_user_14,file = "master_user_14.csv")



datatable(head(master_user), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
summary(master_user)

####################Data gathering ends#################
######################################################################
#Data Cleaning
######################################################################
#1.Find if there are any missing values
cat("\n Variables with number of missing values in master_business: \n")
sapply(master_business, function(x) sum(is.na(x))) # To report missing values in business table

#Observation: There are NO MISSING VALUES in master_business table

cat("\n Variables with number of missing values in master_review: \n")
sapply(master_review, function(x) sum(is.na(x))) # To report missing values in review table

#Observation: There are NO MISSING VALUES in master_review table

cat("\n Variables with number of missing values in master_user: \n")
sapply(master_user, function(x) sum(is.na(x))) # To report missing values in user table

#Observation: There are NO MISSING VALUES in master_review table
######################################################################
#EDA
######################################################################

count <- count(master_user, vars="id")
#total users = 846067
user_rate_1 <- sum(master_user$review_count == 1)
print(paste("Number of users reviewed only once:",user_rate_1))
print(paste("Percentage of users reviewed only once:",
            round((user_rate_1/count$n)*100,digits = 2)))
#[1] "Percentage of users reviewed only once: 13.42"

user_rate_10 <- sum(master_user$review_count <= 10)
print(paste("Number of users having less than 10 reviews:",user_rate_10))
print(paste("Percentage of users having less than 10 reviews:",
            round((user_rate_10/count$n)*100,digits = 2)))
#[1] "Percentage of users having less than 10 reviews: 63.18"

user_rate_20 <- sum(master_user$review_count <= 20)
print(paste("Number of users having less than 20 reviews:",user_rate_20))
print(paste("Percentage of users having less than 20 reviews:",
            round((user_rate_20/count$n)*100,digits = 2)))
#[1] "Percentage of users having less than 20 reviews: 77.65"

user_rate_greater20 <- sum(master_user$review_count > 20)
print(paste("Number of users having more than 20 reviews:",user_rate_greater20))
print(paste("Percentage of users having more than 20 reviews:",
            round((user_rate_greater20/count$n)*100,digits = 2)))
#[1] "Percentage of users having more than 20 reviews: 77.65"

user_rate_greater1000 <- sum(master_user$review_count > 1000)
print(paste("Number of users having more than 1000 reviews:",user_rate_greater1000))
print(paste("Percentage of users having more than 1000 reviews:",
            round((user_rate_greater1000/count$n)*100,digits = 2)))
#[1] "Percentage of users having more than 1000 reviews: 77.65"

#ratings distribution
#distribution of ratings

p1 <- master_business %>% ggplot(aes(x=as.factor(Stars))) + geom_bar(fill = "steelblue")
p2 <- master_business %>% ggplot(aes(x=as.numeric(Review_Count))) + 
  geom_histogram(bins=50, color = "black", fill = "steelblue") + 
  scale_x_log10() 

##a <- ggplot(master_business, aes(x = as.numeric(Review_Count)))
#a1 <- a + geom_area(bins = 50, stat = "bin", color = "black", fill = "#00AFBB")

#b <- ggplot(master_business, aes(x = as.numeric(Review_Count)))
##b1 <- b + geom_bar(bins = 50, stat = "bin")

c <- ggplot(master_business, aes(x = as.numeric(Review_Count), y = Stars))
c1 <- c + geom_point(aes(color = Stars))

grid.arrange(p1,p2,c1, nrow = 2, ncol=2)

#Most popular categories of the restaurants on yelp
categories = str_split(master_business$Category,";")
categories = as.data.frame(unlist(categories))
colnames(categories) <- c("CategoryName")

categories %>%
  group_by(CategoryName) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(CategoryName = reorder(CategoryName,Count)) %>%
  head(10) %>%
  
  geom_text(aes(x = CategoryName, y = 1, label = paste0("(",Count,")",sep="")),
  ggplot(aes(x = CategoryName,y = Count)) +
  geom_bar(stat='identity',colour="white", fill="steelblue") +
            hjust=0, vjust=.5, size = 4, colour = 'black', fontface = 'bold') +
  labs(x = 'Categories', y = 'Count', title = 'Top 10 Categories of Business') +
  coord_flip() + theme_bw()

#Highest Reviewed restaurants
eda_business <- master_business %>%
  group_by(Name) %>% 
  summarize(m=mean(Stars),s=sum(as.numeric(Review_Count))) %>% 
  filter(s > 100) %>%
  arrange(desc(s)) %>% head(10)

eda_business %>% ggplot(aes(x=Name,y=s))+geom_col(aes(fill=m)) + coord_flip() + labs(fill= 'Mean Rating') + ylab('Highest Reviewed')

#We see that the highest reviewed restaurant is Yard House, followed by Star bucks 
#and Mcdonalds. 