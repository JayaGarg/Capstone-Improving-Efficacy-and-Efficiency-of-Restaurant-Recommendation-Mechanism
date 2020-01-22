# Clear all variables in workspace
rm(list = ls())
#load packages
#R package for collaborative filtering.
library(recommenderlab) 
library(ggplot2)
library(reshape2)
library(beepr)
library(knitr)

#not really needed here, just for improving speed by allowing parallel threads on multicore systems
library(foreach)
#install.packages("doMC", repos = "http://R-forge.R-project.org")
library(doMC)
registerDoMC(cores=4)
########
# Load Data
########
setwd("C:\\Jaya\\GL\\capstone\\data new")
master_business <- read.csv("Master_Data_File.csv", header = TRUE, sep = ",")
master_review_2014 <- read.csv("review_dateswise_2014.csv", header = TRUE, sep = ",")
master_user_14 <- read.csv("master_user_14.csv", header = TRUE, sep = ",")

head(master_business)
head(master_review_2014)
head(master_user_14)

rating <- merge(master_review_2014[,-1],master_business[,c('Business_Id','Name','Category')], by.x='business_id',by.y='Business_Id')
rating <- merge(rating,master_user_14[,c(2,3)], by.x='user_id',by.y='id')
#1272555 obs

head(rating)
#rename the columns
colnames(rating)[colnames(rating)=="Name"] <- "restaurant"
colnames(rating)[colnames(rating)=="id"] <- "review_id"
colnames(rating)[colnames(rating)=="name"] <- "user"
str(rating)
beep(sound = 8,expr = NULL)

#rearrange the column
rating <- rating[,c("restaurant", "business_id", "user", "user_id","stars","Category","date","text","review_id","useful", "funny", "cool")]
#1272555 obs

# Some user rated the same restaurant more than one times. Remove the duplicated data. 
rating_ndup <- rating[!duplicated(rating[c('user_id','business_id')]),]
#325604 obs

#keep only relevant cols
rating_t <- rating_ndup[,1:6]
summary(rating_t)
head(rating_t)

# convert ratings data to realRatingMatrix for implement of recommenderlab package
length(unique(rating_t[,"user_id"])) #[1] 143195
length(unique(rating_t[,"business_id"])) #12592

#build the user-item matrix
udf <- data.frame(user_No= seq(1:length(unique(rating_t[,"user_id"]))),user_id= unique(rating_t[,"user_id"]))
idf <- data.frame(restaurant_No= seq(1:length(unique(rating_t[,"business_id"]))),business_id=unique(rating_t[,"business_id"]))

ratings <- merge(rating_t,udf,by.x='user_id',by.y='user_id')
ratings <- merge(ratings,idf,by.x='business_id',by.y='business_id')

#rearrange the column
ratings <- ratings[,c("restaurant", "business_id", "restaurant_No","Category", "stars", "user", "user_id", "user_No")]
#325604 obs
class(ratings)
ratings$user_No <- as.numeric(ratings$user_No)
ratings$restaurant_No <- as.numeric(ratings$restaurant_No)
str(ratings)
ratings_bkup <- ratings
ratings <- ratings[,c(8,3,5)]
ratings <- ratings[order(ratings$user_No),]
ratings_bkup <- ratings_bkup[order(ratings_bkup$user_No),]
#ratings table contains ids of users and restaurants and ratings.

user_review_frequency = data.frame(table(ratings$user_No))
user_review_frequency <- user_review_frequency[order(-user_review_frequency$Freq),]

rating_mx <- sparseMatrix(i =  ratings$user_No, j =  ratings$restaurant_No, x = ratings$stars,
                          dims = c(length(unique(ratings$user_No)), length(unique(ratings$restaurant_No))),
                          dimnames = list(paste("U", 1:length(unique(ratings$user_No)), sep = ""),
                                          paste("R", 1:length(unique(ratings$restaurant_No)), sep = "")))

class(rating_mx)
dim(rating_mx)
#143195  12592

#converting dcGMatrix to realRatingMatrix for applyting recommenderlab
mx <- new("realRatingMatrix", data = rating_mx)
mx
#143195 x 12592 rating matrix of class 'realRatingMatrix' with 325604 ratings.

# Keeping only users with more than 5 ratings
mx_r <- mx[rowCounts(mx) > 10,]
mx_r
#9765 x 12592 rating matrix of class 'realRatingMatrix' with 115839 ratings.
mx_r <- mx_r[,colCounts(mx_r) > 5]
dim(mx_r)
#11116  6500

#Heatmap:

image(mx_r, main = "Yelp restarurant reviews Data")
image(normalize(mx_r), main = "Yelp restarurant reviews Data")

#plot histograms
#Here histogram gives us the frequency of the given ratings
hist(getRatings(mx_r), main="Distribution of ratings", breaks=100, xlim=c(0, 5))
#From the histogram we can say that most of the movies are rated between 3 and 5

# Let us look at the normalized ratings histogram

hist(getRatings(normalize(mx_r)), breaks=6)

# From the normalized rating we can find that there are more zero rating

hist(colMeans(mx_r)) # average rating per business
hist(rowCounts(mx_r)) # number of ratings per user

summary(ratings[, 'stars'])

#distribution of ratings
rating_frq <- as.data.frame(table(ratings$stars))

ggplot(rating_frq,aes(Var1,Freq)) +   
  geom_bar(aes(fill = Var1), position = "dodge", stat="identity",fill="palegreen")+ labs(x = "Stars")

#calculate average reviews for each restaurant
business_mean <- data.frame(restaurant = idf$restaurant, average_stars=colMeans(mx_r))

par(mfrow=c(2,2))

ggplot(user,aes(review_count)) +
  geom_histogram(binwidth = 0.05,col='red',fill="plum") + coord_cartesian(ylim=c(0,12000)) + labs(x = "User Review COunt")+geom_vline(xintercept = mean(user$review_count),col='blue',size=1)


as(mx_r, "list")
head(as(mx_r, "list"))
head(as(mx_r,"data.frame")) 
#view the matrix
getRatingMatrix(mx_r)[1:10,70:80]

par(mfrow=c(1,1))
#User-user collaborative Filtering
#similarity matrix with cosine distance
similarity_users <- similarity(mx_r[1:50, ], method =  "cosine", which = "users") 
image(as.matrix(similarity_users), main = "User similarity")
as.matrix(similarity_users)

##similarity matrix with euclidean distance
similarity_users <- similarity(mx_r[1:50, ], method =  "euclidean", which = "users") 
image(as.matrix(similarity_users), main = "User similarity")
as.matrix(similarity_users)

############################################################
#Building a Recommender model
#method="UBCF" is user based collaborative filtering, "IBFC" is item based collaborative filtering

rec_ubcf_cos=Recommender(mx_r,method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=30))
rec_ubcf_jac=Recommender(mx_r,method="UBCF", param=list(normalize = "Z-score",method="Jaccard",nn=30))
rec_ubcf_pea=Recommender(mx_r,method="UBCF", param=list(normalize = "Z-score",method="pearson",nn=30))
#rec_ibcf_cos=Recommender(mx_r,method="IBCF", param=list(normalize = "Z-score",method="Cosine"))

print(rec_ubcf_cos)
names(getModel(rec_ubcf_cos))
getModel(rec_ubcf_cos)$nn

#predict ratings for 1st user in dataset
top10_pred_rate <- predict(rec_ubcf_cos,mx_r[1],type = "ratings")

top10_pred_rate <- as(top10_pred_rate, "list") #convert recommenderlab object to readable list
top10_pred_rate

#Obtain top 10 recommendations for 1st user in dataset
top10_pred_list <- predict(rec_ubcf_cos,mx_r[1],n=10)

top10_pred_list <- as(top10_pred_list, "list") #convert recommenderlab object to readable list
top10_pred_list
#$`U18`
#[1] "R501"  "R547"  "R659"  "R2887" "R4483" "R4572" "R7429" "R557"  "R775"  "R232"

#map them to Restaurant names
library(dplyr)
#We convert the list to a dataframe and change the column name to Business ID
top_10_df=data.frame(top10_pred_list)
colnames(top_10_df)="BusinessId"
top_10_df <- gsub("R","",top_10_df$BusinessId)
top_10_df=data.frame(top_10_df)
colnames(top_10_df)="restaurant_No"

str(top_10_df)
str(ratings)

#Since restaurant_No is of type integer in ratings data created, we typecast BusinessId in our recommendations as well
top_10_df$restaurant_No <- as.numeric(levels(top_10_df$restaurant_No))[top_10_df$restaurant_No]

#Merge the restaurant ids with names 
names <- left_join(top_10_df, ratings_bkup, by="restaurant_No")
names <- names[,c(1,2)]

names_unique <- unique.data.frame(names)
colnames(names_unique)[colnames(names_unique) == "restaurant"]="Name"
names_unique

str(master_business)
#Merge with master_business to get other details 
details <- left_join(names_unique, master_business, by="Name")
details

#details of user1 for whom recommendations are made
user1 <- rownames(as(mx_r[1],"matrix"))
user1

#We convert the list to a dataframe and change the column name to user_No
user1 <- data.frame(user1)
user1 <- gsub("U","",user1$user1)
user1 <- data.frame(user1)
colnames(user1)="user_No"

#Since user_No is of type integer in ratings data created, we typecast userNo in our recommendations as well
user1$user_No <- as.numeric(levels(user1$user_No))[user1$user_No]
user1_det <- left_join(user1, ratings_bkup, by="user_No")
user1_det[,c(1,2,5,6,7)]
write.csv(details, file = "recommendations for user1.csv")
write.csv(user1_det, file = "user1 details.csv")
########################################################################
# Creation of the model - U(ser) B(ased) C(ollaborative) F(iltering)
Rec.model.cos <- Recommender(mx_r, method = "UBCF", 
                         param=list(normalize = "Z-score", method="Cosine", nn=25))

#Making predictions :ratings
prediction.cos.rate <- predict(Rec.model.cos, mx_r[1:25, ], type="ratings")
as(prediction.cos.rate, "matrix")[,1:5]

#           R1       R2       R3       R4       R5
#U19  5.000000 5.000000 5.000000 5.000000 5.000000
#U26  2.696970 2.696970 2.696970 2.696970 2.696970
#U28  3.444444 3.444444 3.444444 3.444444 3.444444
#U30  4.333333 4.333333 4.333333 4.333333 4.333333
#U34  3.600000 3.600000 3.654994 3.600000 3.600000
#U38  3.125000 3.125000 3.125000 3.125000 3.125000
#U39  3.303030 3.303030 3.303030 3.303030 3.303030
#U51  3.428571 3.411271 3.428571 3.428571 3.428571
#U54  3.500000 3.500000 3.500000 3.500000 3.500000
#U73  2.833333 2.833333 2.833333 2.833333 2.833333
#U74  4.250000 4.250000 4.250000 4.250000       NA
#U82  4.000000 4.000000 4.000000 4.000000 4.000000
#U84  3.555556 3.555556 3.555556 3.555556 3.555556
#U104 4.333333 4.322339 4.333333 4.333333 4.333333
#U125 3.666667 3.666667 3.666667 3.666667 3.666667
#U143 4.250000 4.250000 4.250000 4.250000 4.250000
#U144 4.500000 4.500000 4.500000 4.500000 4.500000
#U147 2.250000 2.250000 2.250000 2.250000 2.250000
#U172 3.833333 3.833333 3.833333 3.833333 3.833333
#U205 3.421053 3.421053 3.421053 3.421053 3.421053
#U211 2.625000 2.625000 2.625000 2.625000 2.625000
#U231 2.833333 2.833333 2.833333 2.833333 2.833333
#U240 3.571429 3.571429 3.571429 3.571429 3.571429
#U259 4.150644 4.157143 4.157143 4.157143 4.157143
#U270 4.230769 4.230769 4.230769 4.230769 4.230769

#Making predictions :top n
prediction.cos.list <- predict(Rec.model.cos, mx_r[1:25, ], n=10)
as(prediction.cos.list, "list")
#################################################################
#we have to consider the location while designing a restaurant recommendation system. 
#get city info from business data
head(master_business)
city <- master_business[,c('Business.Id', 'Name','Category','City','State','Stars')]
city <- city %>% group_by(Business.Id) %>% dplyr::summarise(Name = first(Name), City = first(City), State = first(State), Stars = first(Stars))
colnames(city) <- c('business_id','restaurant','city','state','Stars')
idf_city <- merge(idf,city, by='business_id')

idf_city$city <- as.character(idf_city$city)
idf_city$state <- as.character(idf_city$state)
idf_city <- idf_city[order(idf_city$restaurant_No),]

#get 50 restaurants for User 20 from recommender system
(p_top50 <- predict(Rec.model.cos, mx_r[1],type="topNList",n=50))
## Recommendations as 'topNList' with n = 50 for 1 users.
# filter the restaurant for User 20 based on location
pred_restaurant <-  data.frame(as(p_top50, "list"))
pred_restaurant
colnames(pred_restaurant) <- "U18"
pred_restaurant[] <- lapply(pred_restaurant, as.character)
pred_restaurant$restaurant_No <- gsub("R","",pred_restaurant$U18)
pred_restaurant$restaurant_No <- as.numeric(pred_restaurant$restaurant_No)


pred_restaurant <- left_join(pred_restaurant,idf_city, by='restaurant_No' )
pred_restaurant$city <- as.character(pred_restaurant$city)
pred_restaurant$state <- as.character(pred_restaurant$state)

# For example, if user 18 want to get recommendation for restaurants in Las vegas, we can find out from the top50 list
Lasvegas <- filter(pred_restaurant,city == "Las Vegas")
head(Lasvegas[,-3], n=10)
#################################################################
#Evaluation object for RMSE checking.
set.seed(1)
#k=5 meaning a 5-fold cross validation. given=5 meaning a Given-5 protocol items for learning
eval_sets <- evaluationScheme(mx_r, method="cross-validation", given=5, k=2, goodRating = 4)

eval_sets
#Evaluation scheme with 5 items given
#Method: 'cross-validation' with 2 run(s).
#Good ratings: >=4.000000
#Data set: 9765 x 12592 rating matrix of class 'realRatingMatrix' with 115839 ratings.

#We can count how many items we have in each set:
size_sets <- sapply(eval_sets@runsTrain, length)
size_sets
#Create a UBCF recommender, using cosine similarity and 25 nearest neighbours.
RMSE.model.cos <- Recommender(getData(eval_sets, "train"), method = "UBCF",
                              param=list(normalize = "Z-score", method="Cosine", nn=25))

print(RMSE.model.cos)
#Recommender of type 'UBCF' for 'realRatingMatrix' learned using 4882 users.

### create predictions for the test users using known ratings
RMSE.prediction <- predict(RMSE.model.cos, getData(eval_sets, "known"),n=10, type="ratings")
class(RMSE.prediction)

qplot(rowCounts(RMSE.prediction), geom="histogram", binwidth = 100 ) +  
  ggtitle("Distribution of restaurants per user")


##### evaluate recommendations on "unknown" ratings
#rmse_ubcf <- calcPredictionAccuracy(prediction, getData(e, "unknown"))[1]
RMSE_ubcf_accuracy <- calcPredictionAccuracy(RMSE.prediction, getData(eval_sets, "unknown"),byUser = TRUE)
head(RMSE_ubcf_accuracy)
as(RMSE_ubcf_accuracy, "matrix")
RMSE_ubcf_accuracy[[1]]

#RMSE      MSE      MAE 
#1.140568 1.300896 0.882988

qplot(RMSE_ubcf_accuracy[, "RMSE"]) + geom_histogram(binwidth = 0.1) +
  ggtitle("Distribution of the RMSE by user")

RMSE_ubcf_accuracyk <- calcPredictionAccuracy(RMSE.prediction, getData(eval_sets, "unknown"),byUser = FALSE)
RMSE_ubcf_accuracyk

###########################################

prediction_n <- predict(RMSE.model.cos, getData(eval_sets, "known"), n=10, type="ratings")

rmse_ubcf_n <- calcPredictionAccuracy(prediction_n, getData(eval_sets, "unknown"), byUser= FALSE)
rmse_ubcf_n

#TP 
#0.09725517
message("TP Rate: ", round(rmse_ubcf_n[1]/10*100,2),"%")

evaluation_results <- evaluate(eval_sets, method="UBCF", n=c(1,3,5,10,20,100))
class(evaluation_results)
eval_results <- getConfusionMatrix(evaluation_results)[[1]]
eval_results

#           TP         FP       FN       TN  precision      recall         TPR          FPR
#1  0.01494287  0.8265456 4.922063 29042.24 0.01775766 0.003342158 0.003342158 2.845908e-05
#3  0.03838265  2.4860826 4.898623 29040.58 0.01520427 0.007691044 0.007691044 8.559919e-05
#5  0.05889247  4.1485497 4.878113 29038.91 0.01399721 0.013297302 0.013297302 1.428403e-04
#10 0.09668913  8.3181951 4.840316 29034.74 0.01149025 0.020861302 0.020861302 2.864072e-04
#15 0.12891884 12.4934076 4.808087 29030.57 0.01021356 0.027105433 0.027105433 4.301658e-04
#20 0.16056255 16.6692060 4.776443 29026.39 0.00954039 0.033715768 0.033715768 5.739447e-04
###############################################################################
columns_to_sum <- c("TP", "FP", "FN", "TN")
indices_summed <- Reduce("+", getConfusionMatrix(evaluation_results))[, columns_to_sum]
head(indices_summed)

#avg(evaluation_results)
plot(evaluation_results, annotate = TRUE, main = "ROC curve")
plot(evaluation_results, "prec/rec", annotate = TRUE, main = "Precision-recall")
################################################################################
##Compare Several Algorithms
###########################
algorithms <- list(
  RANDOM = list(name = "RANDOM", param = NULL),
  POPULAR = list(name = "POPULAR", param = NULL),
  UBCF = list(name = "UBCF", param = list(nn=50)),
  UBCF_cos = list(name = "UBCF", param = list(method = "cosine")),
  UBCF_cor = list(name = "UBCF", param = list(method = "pearson")),
  IBCF = list(name = "IBCF", param = list()),
  ALS = list(name = "ALS", param = NULL),
  SVD = list(name = "SVD", param = NULL)
)

# Predict top-N recommendation lists
ev <- evaluate(eval_sets, algorithms, type="topNList", n=c(1, 5, seq(10, 100, 10)))
class(ev)
class(ev[[1]])
sapply(ev, class) == "evaluationResults"

UBCF_CM <- getConfusionMatrix(ev[["UBCF"]])
UBCF_CM

avg_matrices <- lapply(ev, avg)
head(avg_matrices$UBCF[, 5:8])
avg_matrices
recommenderlab::avg(ev)
head(getConfusionMatrix(ev)[[1]])

par(mfrow = c(1,1))
plot(ev)
p1 <- plot(ev, annotate=TRUE)

plot(ev, "ROC",annotate = 1, legend = "topleft")
title("ROC curve")
plot(ev, "prec/rec", annotate = 1, legend = "topright")
title("Precision-recall")
##############################################################################
# Predict missing ratings
gc()
memory.limit(size=900000)
ev_ratings2 <- evaluate(eval_sets, algorithms, type="ratings")
recommenderlab::avg(ev_ratings2)
plot(ev_ratings2)

UBCF_CM <- getConfusionMatrix(ev_ratings2[["UBCF"]])
UBCF_CM[1]
UBCF_CM <- as.data.frame(UBCF_CM[1])

error = rbind(
  as.data.frame(getConfusionMatrix(ev_ratings2[["RANDOM"]])[1]),
  as.data.frame(getConfusionMatrix(ev_ratings2[["POPULAR"]])[1]),
  as.data.frame(getConfusionMatrix(ev_ratings2[["UBCF"]])[1]),
  as.data.frame(getConfusionMatrix(ev_ratings2[["ALS"]])[1]),
  as.data.frame(getConfusionMatrix(ev_ratings2[["SVD"]])[1])
)

rownames(error) <- c("RANDOM","POPULAR", "UBCF","ALS", "SVD")
error
avg_matrices <- lapply(ev_ratings2, avg)
head(avg_matrices$UBCF)
avg_matrices

par(mfrow = c(1,1))
plot(ev_ratings2)
plot(ev_ratings2, ylim = c(0,5))

plot(ev_ratings2, annotate = 1,legend = "topleft")
title("RMSE")
plot(ev_ratings2, "prec/rec", annotate = 1, legend = "topright")
title("Precision-recall")
################################
#Dynamic Recommendation
library(dplyr)
########
# Load Data
########

setwd("C:\\Jaya\\GL\\capstone\\data")
master_review_2013 <- read.csv("review_dateswise_2013.csv", header = TRUE, sep = ",")
master_review_2014 <- read.csv("review_dateswise_2014.csv", header = TRUE, sep = ",")

head(master_review_2013)
head(master_review_2014)

compare_df13 <- master_review_2013[,c(3,5)]
compare_df13 <- compare_df13[order(compare_df13$business_id),]
compare_df13_group <- compare_df13 %>% group_by(business_id) %>% dplyr::summarise(stars_13 = mean(stars))
#25254 obs
head(compare_df13_group)
compare_df14 <- master_review_2014[,c(3,5)]
compare_df14 <- compare_df14[order(compare_df14$business_id),]
compare_df14_group <- compare_df14 %>% group_by(business_id) %>% dplyr::summarise(stars_14 = mean(stars))
#29053 obs

head(compare_df14_group,n=10)
#####################
merged_df <- merge(compare_df13_group, compare_df14_group, by = 'business_id')
#count= 21923

head(merged_df)
merged_df <- mutate(merged_df, comparison = round((stars_14 - stars_13),2))
min(merged_df$comparison)
#21923     4

write.csv(merged_df, "merged_df.csv")
##########################
merged_df <- merged_df[order(merged_df$comparison),]
trend_negative <- split(merged_df,merged_df$comparison < 0.00)
trend_positive = split(merged_df,merged_df$comparison >= 0.00)[['TRUE']]

#Building a Recommender model
#method="UBCF" is user based collaborative filtering, "IBFC" is item based collaborative filtering

rec_ubcf_cos=Recommender(mx_r,method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=30))

#Obtain top 50 recommendations for 1st user in dataset
top50 <- predict(rec_ubcf_cos,mx_r[1],n=50)

top50 <- as(top50, "list") #convert recommenderlab object to readable list
top50
#$`U18`
#[1] "R501"  "R547"  "R659"  "R2887" "R4483" "R4572" "R7429" "R557"  "R775"  "R232"  "R5366"
#[12] "R5422" "R7746" "R5708" "R1111" "R1262" "R2338" "R6026" "R568"  "R835"  "R3485" "R4810"
#[23] "R7205" "R1"    "R2"    "R3"    "R4"    "R5"    "R6"    "R7"    "R8"    "R9"    "R10"  
#[34] "R11"   "R12"   "R13"   "R14"   "R15"   "R16"   "R18"   "R19"   "R20"   "R29"   "R30"  
#[45] "R31"   "R32"   "R33"   "R34"   "R35"   "R36"  

top_50_df=data.frame(top50)
top_50_df <- merge(idf_city,top_50_df, by = "restaurant_No")
top50_dyn_trend <- merge(top_50_df, trend_positive, by = 'business_id')
top50_dyn_trend <- top50_dyn_trend[order(-top50_dyn_trend$comparison),]

col1<- mapply(anyNA,top50_dyn_trend) # apply function anyNA() on all columns to check if comparison column has any NA's
col1

Lasvegas_dy <- filter(top50_dyn_trend,city == "Las Vegas")
writeLines("The chosen user liked: ")
kable(activeUser[,c(-2,-3,-6)],format = "rst",row.names = FALSE)

writeLines("You may also like these Restaurants based on dynamic rating at LV:")
kable(Lasvegas_dy[,c(3:6)],format = "rst",row.names = FALSE)

Toronto_dy <- filter(top50_trend,city == "Toronto")
writeLines("The chosen user liked: ")
kable(activeUser[,c(-2,-3,-6)],format = "rst",row.names = FALSE)

writeLines("You may also like these Restaurants:")
kable(Toronto_dy[,c(3:6)],format = "rst",row.names = FALSE)

#####################################################################
#map them to Restaurant names
library(dplyr)
#We convert the list to a dataframe and change the column name to Business ID
top_50_df=data.frame(top50)
colnames(top_50_df)="BusinessId"
top_50_df <- gsub("R","",top_50_df$BusinessId)
top_50_df=data.frame(top_50_df)
colnames(top_50_df)="restaurant_No"

str(top_50_df)
str(ratings)

#Since restaurant_No is of type integer in ratings data created, we typecast BusinessId in our recommendations as well
top_50_df$restaurant_No <- as.numeric(levels(top_50_df$restaurant_No))[top_50_df$restaurant_No]

#Merge the restaurant ids with names 
names <- left_join(top_50_df, ratings_bkup, by="restaurant_No")
names <- names[,c(1,2)]

names_unique <- unique.data.frame(names)
colnames(names_unique)[colnames(names_unique) == "restaurant"]="Name"
names_unique

str(master_business)
#Merge with master_business to get other details 
details <- left_join(names_unique, master_business, by="Name")
details

#details of user1 for whom recommendations are made
user1 <- rownames(as(mx_r[1],"matrix"))
user1

#We convert the list to a dataframe and change the column name to user_No
user1 <- data.frame(user1)
user1 <- gsub("U","",user1$user1)
user1 <- data.frame(user1)
colnames(user1)="user_No"

#Since user_No is of type integer in ratings data created, we typecast userNo in our recommendations as well
user1$user_No <- as.numeric(levels(user1$user_No))[user1$user_No]
user1_det <- left_join(user1, ratings_bkup, by="user_No")
user1_det[,c(1,2,5,6,7)]


