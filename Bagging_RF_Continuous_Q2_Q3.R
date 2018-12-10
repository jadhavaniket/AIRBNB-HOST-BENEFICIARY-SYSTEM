###### Bagging an dRandom Forest Classification for Question 2 - Continuous Variable ##########

library(ISLR) #to manipulate dataset
library(MASS) #loading datasets in R
library(randomForest)

#retrieve data
setwd("/Users/nik/Documents/Nik/MIM/INST737/Project/Decision Trees")
newdataQ1 <- read.csv("listing_latest.csv")

#subsetting data
#remove variavles as factors with more than 53 levels for RF
myvarsQ1 <- names(newdataQ1) %in% c("first_review", "last_review", "calendar_updated", "extra_people", "amenities", "smart_location", "neighbourhood_cleansed", "street", "host_verifications", "host_url", "host_id", "picture_url", "listing_url")
newdata <- newdataQ1[!myvarsQ1]

#Choose a set of variables to be considered
myvars1 <- c("review_scores_rating", "price", "accommodates", "review_scores_location", "beds", "bedrooms", "bathrooms", "zipcode_freq")
listing <- newdata[myvars1]
View(listing)

#Explore data
nrow(listing)
summary(listing)
View(listing)

#Define variable types
listing$room_type<- as.factor(listing$room_type)
#listing$neighbourhood_group_cleansed<- as.factor(listing$neighbourhood_group_cleansed)
listing$price<- as.numeric(listing$price)
listing$accommodates<- as.numeric(listing$accommodates)

#Cleaning data for missing values
str(listing)
complete.cases(listing)
clean.listing <- listing[complete.cases(listing), ]
clean.listing<- na.omit(listing)
clean.listing$price

#generate random sample FOR TRAINING DATASET
set.seed(123)
train <- sample(1:nrow(clean.listing),nrow(clean.listing)/2)

####### Question 2 ############

#bagging with the complete sample set and 100 trees
bag.listing <- randomForest(clean.listing$price~., data=clean.listing, subset=train, ntree=100, importance=TRUE)
bag.listing

#bagging with the complete sample set and 200 trees
bag.listing <- randomForest(clean.listing$price~., data=clean.listing, subset=train, ntree=200, importance=TRUE)
bag.listing

#bagging with the complete sample set and 300 trees
bag.listing <- randomForest(clean.listing$price~., data=clean.listing, subset=train, ntree=300, importance=TRUE)
bag.listing

#bagging with the complete sample set and 500 trees
bag.listing <- randomForest(clean.listing$price~., data=clean.listing, subset=train, ntree=500, importance=TRUE)
bag.listing

#bagging with the complete sample set and 800 trees
bag.listing <- randomForest(clean.listing$price~., data=clean.listing, subset=train, ntree=800, importance=TRUE)
bag.listing

#Find the optimal number of variables selected at each split
mtry <- tuneRF(clean.listing[-1],clean.listing$review_scores_rating, ntreeTry=500, stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

print(mtry)
print(best.m)

#Random Forest number of trees you want to consider for each tree at a split
set.seed(71)
rf <-randomForest(clean.listing$review_scores_rating~.,data=clean.listing, mtry=best.m, subset=train, importance=TRUE,ntree=500)
print(rf)

#Predict testing dataset 
pred <- predict(bag.listing, newdata=clean.listing[-train,])

mean((pred-clean.listing[-train,1])^2)

plot(pred,clean.listing[-train,1])
abline(c(0,1), col=2)



########## Question 3  ###################


#bagging with the complete sample set and 200 trees
bag.listing <- randomForest(clean.listing$review_scores_location~., data=clean.listing, subset=train, ntree=200, importance=TRUE)
bag.listing

#bagging with the complete sample set and 300 trees
bag.listing <- randomForest(clean.listing$review_scores_location~., data=clean.listing, subset=train, ntree=300, importance=TRUE)
bag.listing

#bagging with the complete sample set and 100 trees
bag.listing <- randomForest(clean.listing$review_scores_location~., data=clean.listing, subset=train, ntree= 500, importance=TRUE)
bag.listing

#bagging with the complete sample set and 800 trees
bag.listing <- randomForest(clean.listing$review_scores_location~., data=clean.listing, subset=train, ntree=800, importance=TRUE)
bag.listing

#bagging with the complete sample set and 1000 trees
bag.listing <- randomForest(clean.listing$review_scores_location~., data=clean.listing, subset=train, ntree=1000, importance=TRUE)
bag.listing

#bagging with the complete sample set and 2000 trees
bag.listing <- randomForest(clean.listing$review_scores_location~., data=clean.listing, subset=train, ntree=2000, importance=TRUE)
bag.listing

#Find the optimal number of variables selected at each split
mtry <- tuneRF(clean.listing[-1],clean.listing$review_scores_rating, ntreeTry=2000, stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

print(mtry)
print(best.m)

#Random Forest number of trees you want to consider for each tree at a split
set.seed(71)
rf <-randomForest(clean.listing$review_scores_rating~.,data=clean.listing, mtry=best.m, subset=train, importance=TRUE,ntree=1000)
print(rf)

#Evaluate the performance of the random forest for classification
plot(pred,clean.listing[-train,1])
abline(c(0,1), col=2)
