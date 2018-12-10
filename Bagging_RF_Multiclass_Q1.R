#Bagging and Random Forest Classification for Question 1 - Multiclass variable. 

setwd("/Users/nik/Documents/Nik/MIM/INST737/Project/Decision Trees")
newdataQ1 <- read.csv("listing_latest.csv")

myvarsQ1 <- names(newdataQ1) %in% c("first_review", "last_review", "calendar_updated", "extra_people", "amenities", "smart_location", "neighbourhood_cleansed", "street", "host_verifications", "host_url", "host_id", "picture_url", "listing_url")
newdata <- newdataQ1[!myvarsQ1]

myvars1 <- c("review_scores_rating", "neighbourhood_group_cleansed", "price","room_type", "accommodates", "cancellation_policy", "instant_bookable", "host_is_superhost", "host_response_time", "review_scores_location", "beds", "bedrooms", "bathrooms", "zipcode_freq")
listing <- newdata[myvars1]
View(listing)


#Explore data
nrow(listing)
summary(listing)
View(listing)

#Define variable types
listing$room_type<- as.factor(listing$room_type)
listing$neighbourhood_group_cleansed<- as.factor(listing$neighbourhood_group_cleansed)
listing$price<- as.numeric(listing$price)
listing$accommodates<- as.numeric(listing$accommodates)

listing$review_scores_rating <-cut(listing$review_scores_rating, c(20.000,90.000,95.000,100.000),right="FALSE")
listing$review_scores_rating<- as.factor(listing$review_scores_rating)

summary(listing$review_scores_rating)

str(listing)
complete.cases(listing)
clean.listing <- listing[complete.cases(listing), ]
clean.listing<- na.omit(listing)

nrow(clean.listing)

#another way to generate random sample FOR TRAINING DATASET
listing_rand <- clean.listing[order(runif(17558)),]
nrow(listing_rand)
nrow(newdataQ1)

#Create a 80%-20% split
train <- listing_rand[1:12290,]
test <- listing_rand[12290:17558,]


#Install and load packages required for random forest
install.packages("party")
install.packages("randomForest")
install.packages("ROCR")
library(randomForest)
library(ROCR)

set.seed(81) 
rf <-randomForest(train$review_scores_rating~.,data=train, ntree=200) 
print(rf)

rf <-randomForest(train$review_scores_rating~.,data=train, ntree=400) 
print(rf)

rf <-randomForest(train$review_scores_rating~.,data=train, ntree=500) 
print(rf)

rf <-randomForest(train$review_scores_rating~.,data=traing, ntree=600) 
print(rf)

View(train)

mtry <- tuneRF(train[-1],train$review_scores_rating, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

print(mtry)
print(best.m)


rf <-randomForest(train$review_scores_rating~.,data=train, mtry=best.m, importance=TRUE,ntree=500)
print(rf)


#Evaluate variable importance
importance(rf)
varImpPlot(rf)


# Plot partial dependence of each predictor
par(mfrow = c(3, 5), mar = c(2, 2, 2, 2), pty = "s");
for (i in 1:(ncol(clean.listing) - 1))
{
  partialPlot(rf, train, names(train)[i], xlab = names(train)[i],main = NULL);
}


#Predict testing dataset 
pred <- predict(rf, newdata=clean.listing[-train,])

mean((pred-clean.listing[-train,1])^2)


#Evaluate the performance of the random forest for classification.
pred2=predict(rf, type="prob")


#performance in terms of true and false positive rates
#Plot the ROC curve
plot(pred2,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
