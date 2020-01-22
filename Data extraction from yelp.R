# Clear all variables in workspace
rm(list = ls())

#load packages
library(RMySQL)

#Connecting to MySQL:
mydb = dbConnect(MySQL(), user='root', password='root', 
                 dbname='yelp_db', host='localhost')
on.exit(dbDisconnect(mydb))

dbListTables(mydb)
#[1] "attribute"   "business"    "category"    "checkin"     "elite_years" "friend"     
#[7] "hours"       "photo"       "photosdb"    "review"      "tip"         "user" 

########################################################
#1. extract attribute table
attrib1 = dbSendQuery(mydb, "select count(*) from attribute")
attrib_da = fetch(attrib1, n=-1)
attrib_da
#  count(*)
#1  1310575

attrib2 = dbSendQuery(mydb, "select * from attribute")
attrib_tb = fetch(attrib2, n=-1)
head(attrib_tb)
str(attrib_tb)
summary(attrib_tb)

setwd("C:\\Jaya\\GL\\capstone\\yelp csv")
write.csv(attrib_tb, file = "attribute.csv")

########################################################
#2. extract business table
business1 = dbSendQuery(mydb, "select count(*) from business")
business_da = fetch(business1, n=-1)
business_da
#  count(*)
#1  174567

business2 = dbSendQuery(mydb, "select * from business")
business_tb = fetch(business2, n=-1)
head(business_tb)
str(business_tb)
summary(business_tb)

setwd("C:\\Jaya\\GL\\capstone\\yelp csv")
write.csv(business_tb, file = "business.csv")

########################################################
#3. extract category table
category1 = dbSendQuery(mydb, "select count(*) from category")
category_da = fetch(category1, n=-1)
category_da
#  count(*)
#1  667527

category2 = dbSendQuery(mydb, "select * from category")
category_tb = fetch(category2, n=-1)
head(category_tb)
str(category_tb)
summary(category_tb)

setwd("C:\\Jaya\\GL\\capstone\\yelp csv")
write.csv(category_tb, file = "category.csv")

########################################################
#4. extract checkin table
checkin1 = dbSendQuery(mydb, "select count(*) from checkin")
checkin_da = fetch(checkin1, n=-1)
checkin_da
#  count(*)
#1  3911218

checkin2 = dbSendQuery(mydb, "select * from checkin")
checkin_tb = fetch(checkin2, n=-1)
head(checkin_tb)
str(checkin_tb)
summary(checkin_tb)

setwd("C:\\Jaya\\GL\\capstone\\yelp csv")
write.csv(checkin_tb, file = "checkin.csv")

########################################################
#5. extract elite_years table
elite_years1 = dbSendQuery(mydb, "select count(*) from elite_years")
elite_years_da = fetch(elite_years1, n=-1)
elite_years_da
#  count(*)
#1  187125

elite_years2 = dbSendQuery(mydb, "select * from elite_years")
elite_years_tb = fetch(elite_years2, n=-1)
head(elite_years_tb)
str(elite_years_tb)
summary(elite_years_tb)

setwd("C:\\Jaya\\GL\\capstone\\yelp csv")
write.csv(elite_years_tb, file = "elite_years.csv")

########################################################
#6. extract friend table
friend1 = dbSendQuery(mydb, "select count(*) from friend")
friend_da = fetch(friend1, n=-1)
friend_da
#  count(*)
#1  49626957

friend2 = dbSendQuery(mydb, "select * from friend")
friend_tb = fetch(friend2, n=-1)
head(friend_tb)
str(friend_tb)
summary(friend_tb)

setwd("C:\\Jaya\\GL\\capstone\\yelp csv")
write.csv(friend_tb, file = "friend.csv")

########################################################
#7. extract hours table
hours1 = dbSendQuery(mydb, "select count(*) from hours")
hours_da = fetch(hours1, n=-1)
hours_da
#  count(*)
#1  824595

hours2 = dbSendQuery(mydb, "select * from hours")
hours_tb = fetch(hours2, n=-1)
head(hours_tb)
str(hours_tb)
summary(hours_tb)

setwd("C:\\Jaya\\GL\\capstone\\yelp csv")
write.csv(hours_tb, file = "hours.csv")

########################################################
#8. extract photo table
photo1 = dbSendQuery(mydb, "select count(*) from photo")
photo_da = fetch(photo1, n=-1)
photo_da
#  count(*)
#1  206949

photo2 = dbSendQuery(mydb, "select * from photo")
photo_tb = fetch(photo2, n=-1)
head(photo_tb)
str(photo_tb)
summary(photo_tb)

setwd("C:\\Jaya\\GL\\capstone\\yelp csv")
write.csv(photo_tb, file = "photo.csv")

########################################################
#9. extract review table
review1 = dbSendQuery(mydb, "select count(*) from review")
review_da = fetch(review1, n=-1)
review_da
#  count(*)
#1  5261669

review2 = dbSendQuery(mydb, "select * from review")
review_tb = fetch(review2, n=-1)
head(review_tb)
str(review_tb)
summary(review_tb)

setwd("C:\\Jaya\\GL\\capstone\\yelp csv")
write.csv(review_tb, file = "review.csv")

########################################################
#10. extract tip table
tip1 = dbSendQuery(mydb, "select count(*) from tip")
tip_da = fetch(tip1, n=-1)
tip_da
#  count(*)
#1  1098325

tip2 = dbSendQuery(mydb, "select * from tip")
tip_tb = fetch(tip2, n=-1)
head(tip_tb)
str(tip_tb)
summary(tip_tb)

setwd("C:\\Jaya\\GL\\capstone\\yelp csv")
write.csv(tip_tb, file = "tip.csv")

########################################################
#11. extract user table
user1 = dbSendQuery(mydb, "select count(*) from user")
user_da = fetch(user1, n=-1)
user_da
#  count(*)
#1  1326101

user2 = dbSendQuery(mydb, "select * from user")
user_tb = fetch(user2, n=-1)
head(user_tb)
str(user_tb)
summary(user_tb)

setwd("C:\\Jaya\\GL\\capstone\\yelp csv")
write.csv(user_tb, file = "user.csv")
