


top50_trend <- merge(top50, trend_positive, by = 'business_id')
col1<- mapply(anyNA,top50_trend) # apply function anyNA() on all columns to check if comparison column has any NA's
col1
top50_trend <- top50_trend[order(-top50_trend$comparison),]
Lasvegas_dy <- filter(top50_trend,city == "Las Vegas")
writeLines("The chosen user liked: ")
kable(activeUser[,c(-2,-3,-6)],format = "rst",row.names = FALSE)

writeLines("You may also like these Restaurants based on dynamic rating at LV:")
kable(Lasvegas_dy[,c(3:6)],format = "rst",row.names = FALSE)

Toronto_dy <- filter(top50_trend,city == "Toronto")
writeLines("The chosen user liked: ")
kable(activeUser[,c(-2,-3,-6)],format = "rst",row.names = FALSE)

writeLines("You may also like these Restaurants:")
kable(Toronto_dy[,c(3:6)],format = "rst",row.names = FALSE)
##########################




#############################################################################
master_bus <- master_business %>% filter(master_business$Business.Id %in% merged_df$business_id)
##############################################
head(master_review_2014)
#pick master_bus df created above with business ids where trend is calculated
head(master_bus)
head(master_user_14)

rating_tr <- merge(master_review_2014[,-1],master_bus[,c('Business.Id','Name','Category')], by.x='business_id',by.y='Business.Id')
rating_tr <- merge(rating_tr,master_user_14[,c(2,3)], by.x='user_id',by.y='id')
head(rating_tr)
#rename the columns
colnames(rating_tr)[colnames(rating_tr)=="Name"] <- "restaurant"
colnames(rating_tr)[colnames(rating_tr)=="id"] <- "review_id"
colnames(rating_tr)[colnames(rating_tr)=="name"] <- "user"

#rearrange the column
rating_tr <- rating_tr[,c("restaurant", "business_id", "user", "user_id","stars","Category","date","text","review_id","useful", "funny", "cool")]
#1127831 obs

# Some user rated the same restaurant more than one times. Remove the duplicated data. 
rating_ndup_tr <- rating_tr[!duplicated(rating_tr[c('user','restaurant')]),]
#266246 obs

#keep only relevant cols
head(rating_ndup_tr)
rating_trend <- rating_ndup_tr[,1:6]
summary(rating_trend)

#View the data
library(knitr)
head(rating_trend)
kable(head(rating_trend,n=5),format = "rst")

# convert ratings data to realRatingMatrix for implement of recommenderlab package
length(unique(rating_trend[,"user_id"])) #[1] 124590
length(unique(rating_trend[,"business_id"])) #11327

#build the user-item matrix
udf_tr <- data.frame(user_No= seq(1:length(unique(rating_trend[,"user_id"]))),user_id= unique(rating_trend[,"user_id"]))
idf_tr <- data.frame(restaurant_No= seq(1:length(unique(rating_trend[,"business_id"]))),business_id=unique(rating_trend[,"business_id"]))

ratings_tr <- merge(rating_trend,udf_tr,by.x='user_id',by.y='user_id')
ratings_tr <- merge(ratings_tr,idf_tr,by.x='business_id',by.y='business_id')
#266246 obs

#rearrange the column
ratings_tr <- ratings_tr[,c("restaurant", "business_id", "restaurant_No","Category", "stars", "user", "user_id", "user_No")]
ratings_tr$user_No <- as.numeric(ratings_tr$user_No)
ratings_tr$restaurant_No <- as.numeric(ratings_tr$restaurant_No)
str(ratings_tr)
ratings_tr_bkup <- ratings_tr
ratings_tr <- ratings_tr[,c(8,3,5)]
ratings_tr <- ratings_tr[order(ratings_tr$user_No),]
ratings_tr_bkup <- ratings_tr_bkup[order(ratings_tr_bkup$user_No),]
#ratings table contains ids of users and restaurants and ratings.

rating_tr_mx <- sparseMatrix(i =  ratings_tr$user_No, j =  ratings_tr$restaurant_No, x = ratings_tr$stars,
                          dims = c(length(unique(ratings_tr$user_No)), length(unique(ratings_tr$restaurant_No))),
                          dimnames = list(paste("U", 1:length(unique(ratings_tr$user_No)), sep = ""),
                                          paste("R", 1:length(unique(ratings_tr$restaurant_No)), sep = "")))

class(rating_tr_mx)
dim(rating_tr_mx)
#124590  11327

#converting dcGMatrix to realRatingMatrix for applyting recommenderlab
mx_tr <- new("realRatingMatrix", data = rating_tr_mx)
mx_tr

# Keeping only users with more than 5 ratings
mx_tr2 <- mx_tr[rowCounts(mx_tr) > 5,]
mx_tr2
#7426 x 11327 rating matrix of class 'realRatingMatrix' with 84949 ratings.

#Creation and Evaluation of the Recommender model
set.seed(123)
#k=5 meaning a 5-fold cross validation. given=5 meaning a Given-5 protocol items for learning
eval_setstr <- evaluationScheme(mx_tr2, method="cross-validation", given=5, k=2, goodRating = 4)

eval_setstr

#We can count how many items we have in each set:
size_setstr <- sapply(eval_setstr@runsTrain, length)
size_setstr
#Create a UBCF recommender, using cosine similarity and 25 nearest neighbours.
ubcf.model.cos.tr <- Recommender(getData(eval_setstr, "train"), method = "UBCF",
                              param=list(normalize = "Z-score", method="Cosine", nn=25))

print(ubcf.model.cos.tr)
#Recommender of type 'UBCF' for 'realRatingMatrix' learned using 3713 users.

### create predictions for the test users using known ratings
UBCF.prediction.tr <- predict(ubcf.model.cos.tr, getData(eval_setstr, "known"),n=50, type="ratings")
class(UBCF.prediction.tr)

##### evaluate recommendations on "unknown" ratings
#rmse_ubcf <- calcPredictionAccuracy(prediction, getData(e, "unknown"))[1]
UBCF_accuracy_tr <- calcPredictionAccuracy(UBCF.prediction.tr, getData(eval_setstr, "unknown"),byUser = TRUE)
head(UBCF_accuracy_tr)
as(UBCF_accuracy_tr, "matrix")
UBCF_accuracy_tr[[1]]

UBCF_accuracyk_tr <- calcPredictionAccuracy(UBCF.prediction.tr, getData(eval_setstr, "unknown"),byUser = FALSE)
UBCF_accuracyk_tr
############################

##############################
#we have to consider the location while designing a restaurant recommendation system. 
#get city info from business data
head(master_business)
city <- master_business[,c('Business.Id', 'Name','City','State')]
city <- city[!duplicated(city$Name),]
colnames(city) <- c('business_id','restaurant','city','state')
idf_city <- left_join(idf,city, by='business_id')

idf_city$restaurant_No <- paste("R", 1:12592, sep = "")
idf_city$city <- as.character(idf_city$city)
idf_city$state <- as.character(idf_city$state)

#get 50 restaurants for User 20 from recommender system
(p_top50 <- predict(Rec.model.cos, mx_r[1],type="topNList",n=50))
## Recommendations as 'topNList' with n = 50 for 1 users.
# filter the restaurant for User 20 based on location
pred_restaurant <-  data.frame(as(p_top50, "list"))
pred_restaurant
colnames(pred_restaurant) <- "U18"
pred_restaurant[] <- lapply(pred_restaurant, as.character)
pred_restaurant$restaurant_No <- pred_restaurant$U18

pred_restaurant <- left_join(pred_restaurant,idf_city, by='restaurant_No' )
pred_restaurant$city <- as.character(pred_restaurant$city)
pred_restaurant$state <- as.character(pred_restaurant$state)

# For example, if user 240 want to get recommendation for restaurants in Las vegas, we can find out from the top100 list
Lasvegas <- filter(pred_restaurant,city == "Las Vegas")
head(Lasvegas, n=5)
#################################################################
