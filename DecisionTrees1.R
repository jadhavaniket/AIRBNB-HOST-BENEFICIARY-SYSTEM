setwd("/Users/nik/Documents/Nik/MIM/INST737/Project/Decision Trees")
listing <- read.csv("listing_latest.csv")
View(listing)


#subsetting data
myvarsQ1 <- c("review_scores_rating", "neighbourhood_group_cleansed", "price","room_type", "accommodates", "cancellation_policy", "instant_bookable", "host_is_superhost", "host_response_time")
newdataQ1 <- listing[myvarsQ1]
View(newdataQ1)


#convert Rating into a binary variable
summary(review_scores_rating)
newdataQ1$review_scores_rating <- as.numeric(newdataQ1$review_scores_rating)

newdataQ1$review_scores_rating <-cut(newdataQ1$review_scores_rating, c(20.000,90.000,95.000,100.000),right="FALSE")
#newdataQ1$review_scores_Goodrating[newdataQ1$review_scores_rating >= 20 & newdataQ1$review_scores_rating < 90 ] <- "bad"
#newdataQ1$review_scores_Goodrating[newdataQ1$review_scores_rating >= 90 & newdataQ1$review_scores_rating < 95] <- "med"
#newdataQ1$review_scores_Goodrating[newdataQ1$review_scores_rating >= 95 & newdataQ1$review_scores_rating <= 100] <- "good"


#freq of values 
table(newdataQ1$room_type)
table(newdataQ1$review_scores_rating)

#randomize the same if the sample is not randomly ordered
set.seed(12435) #12345 -any number

#generates random number between 0 and 1
#runif() 

#Indicates sorted position of a number
#order()
#take order of 1000 random numbers of rows and all columns
listing_rand <- newdataQ1[order(runif(29918)),]

nrow(listing_rand)
nrow(newdataQ1)

#Making sure data is the same even after randomizing
summary(listing$price)
summary(listing_rand$price)

#Create a 80%-20% split
listing_train <- listing_rand[1:23934,]
listing_test <- listing_rand[23935:29918,]

#checking proportion of the distribution of values that we have
#checking proportion of average rating that have been rated good, mediocre and bad
#train set
prop.table(table(listing_train$review_scores_rating))

#test set
prop.table(table(listing_test$review_scores_rating))

library(C50)

listing_train[1,1]

listing_train$review_scores_rating <- factor(listing_train$review_scores_rating)


#create model - C5.0(trainData(except column 17 i.e default label, Label))
listing_model <- C5.0(listing_train[-1], listing_train$review_scores_rating)
listing_model
summary(listing_model)

#predict whether individual will default or not on the loan using the model computed
#construct the confusion matrix
listing_pred <- predict(listing_model, listing_test)

library(gmodels)
#display the information in a nice table -crosstable
#
CrossTable(listing_test$review_scores_rating, listing_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn=c('actual Good Rating', 'predicted Good rating'))

#improving model performance by boosting
#compute (run)10 different decision trees
listing_boost10 <- C5.0(listing_train[-1], listing_train$review_scores_rating, trials = 3)
listing_boost10 
summary(listing_boost10)

#now that we have confusion table for training set and we know the accuracy against the training set
#we will try to compare it with accuracy against the testing set.

#predict  for testing set
listing_pred_boost10 <- predict(listing_boost10 , listing_test)
CrossTable(listing_test$review_scores_rating, listing_pred_boost10, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn=c('actual Good Rating', 'predicted Good Rating'))


####################### Question 2 #########################


myvarsQ2 <- c("price", "neighbourhood_group_cleansed", "room_type", "accommodates", "host_response_time", "bedrooms","bathrooms", "beds", "review_scores_rating")
newdataQ2 <- listing[myvarsQ2]
View(newdataQ2)

newdataQ2$price <-cut(newdataQ2$price, c(10.000,70.000,104.000,140.000,169.000,10000.000),right="FALSE")



#convert Price into a category variable
#newdataQ1$price <- ifelse(newdataQ1$review_scores_rating >= 90, "good", "bad")


#newdataQ2$price[newdataQ2$price < 70] <- "bad"
#newdataQ2$price[newdataQ2$price > 70 & newdataQ2$price <= 95] <- "med"
#newdataQ2$price[newdataQ2$price > 95] <- "good"

#factor(newdataQ2$price)
newdataQ2$price <- factor(newdataQ2$price)

#freq of values 
table(newdataQ2$room_type)
table(newdataQ2$price)
View(newdataQ2)

#randomize the same if the sample is not randomly ordered
set.seed(12233425) #12345 -any number

#generates random number between 0 and 1
#runif() 

#Indicates sorted position of a number
#order()
#take order of 1000 random numbers of rows and all columns
listing_randQ2 <- newdataQ2[order(runif(29918)),]

nrow(listing_randQ2)
nrow(newdataQ2)
View(newdataQ2)
#Making sure data is the same even after randomizing
summary(listing$price)
summary(listing_randQ2$price)

#Create a 80%-20% split
listing_trainQ2 <- listing_randQ2[1:23934,]
listing_testQ2 <- listing_randQ2[23935:29918,]

#checking proportion of the distribution of values that we have
#checking proportion of loans that have been defaulted and proportion of loans that is not defaulted
#train set
prop.table(table(listing_trainQ2$price))

#test set
prop.table(table(listing_testQ2$price))

library(C50)

listing_trainQ2[1,1]

listing_trainQ2$price <- factor(listing_trainQ2$price)


#create model - C5.0(trainData(except column 17 i.e default label, Label))
listing_modelQ2 <- C5.0(listing_trainQ2[-1], listing_trainQ2$price)
listing_modelQ2
summary(listing_modelQ2)

listing_testQ2[1,1]


#predict whether individual will default or not on the loan using the model computed
#construct the confusion matrix
listing_predQ2 <- predict(listing_modelQ2, listing_testQ2)

library(gmodels)
#display the information in a nice table -crosstable
#
CrossTable(listing_testQ2$price, listing_predQ2, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn=c('actual Optimal Price', 'predicted Optimal Price'))

#improving model performance by boosting
#compute (run)10 different decision trees
listing_boost10_q2 <- C5.0(listing_trainQ2[-1], listing_trainQ2$price, trials = 7)
listing_boost10_q2 
summary(listing_boost10_q2)

#now that we have confusion table for training set and we know the accuracy against the training set
#we will try to compare it with accuracy against the testing set.

#predict  for testing set
listing_predQ2_boost10 <- predict(listing_boost10_q2 , listing_testQ2)
CrossTable(listing_testQ2$price, listing_predQ2_boost10, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn=c('actual Optimal Price', 'predicted Optimal Price'))



########### Question 3 ######################


myvars3 <- c("review_scores_location", "reviews_per_month", "price","accommodates", "beds", "room_type", "bedrooms","bathrooms")
newdata3 <- listing[myvars3]
newdata3$unit_price <- newdata3$price/newdata3$accommodates
myvarsQ3 <- c("review_scores_location", "reviews_per_month", "unit_price", "price","accommodates", "beds", "room_type", "bedrooms","bathrooms")
newdataQ3 <- newdata3[myvarsQ3]
View(newdata3)

summary(newdataQ3$review_scores_location)
newdataQ3$review_scores_location <-cut(newdataQ3$review_scores_location, c(2.000,9.000,10.000),right="FALSE")

#factor(newdataQ3$review_scores_location)
newdataQ3$review_scores_location <- factor(newdataQ3$review_scores_location)

#freq of values 
table(newdataQ3$zipcode_freq)
table(newdataQ3$review_scores_location)

#randomize the same if the sample is not randomly ordered
set.seed(1242325) #12345 -any number

#generates random number between 0 and 1
#runif() 

#Indicates sorted position of a number
#order()
#take order of 1000 random numbers of rows and all columns
listing_randQ3 <- newdataQ3[order(runif(29918)),]

nrow(listing_randQ3)
nrow(newdataQ3)

#Making sure data is the same even after randomizing
summary(newdataQ3$price)
summary(listing_randQ3$price)

#Create a 80%-20% split
listing_trainQ3 <- listing_randQ3[1:23934,]
listing_testQ3 <- listing_randQ3[23935:29918,]

#checking proportion of the distribution of values that we have
#checking proportion of loans that have been defaulted and proportion of loans that is not defaulted
#train set
prop.table(table(listing_trainQ3$review_scores_location))

#test set
prop.table(table(listing_testQ3$review_scores_location))

library(C50)

listing_trainQ3[1,1]

listing_trainQ3$review_scores_location <- factor(listing_trainQ3$review_scores_location)


#create model - C5.0(trainData(except column 17 i.e default label, Label))
listing_modelQ3 <- C5.0(listing_trainQ3[-1], listing_trainQ3$review_scores_location)
listing_modelQ3
summary(listing_modelQ3)

listing_testQ3[1,1]


#predict whether individual will default or not on the loan using the model computed
#construct the confusion matrix
listing_predQ3 <- predict(listing_modelQ3, listing_testQ3)

library(gmodels)
#display the information in a nice table -crosstable
#
CrossTable(listing_testQ3$review_scores_location, listing_predQ3, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn=c('actual Optimal review_scores_location', 'predicted Optimal review_scores_location'))

#improving model performance by boosting
#compute (run)10 different decision trees
listing_boost10_q3 <- C5.0(listing_trainQ3[-1], listing_trainQ3$review_scores_location, trials = 11)
listing_boost10_q3 
summary(listing_boost10_q3)

#now that we have confusion table for training set and we know the accuracy against the training set
#we will try to compare it with accuracy against the testing set.

#predict  for testing set
listing_predQ3_boost10 <- predict(listing_boost10_q3 , listing_testQ3)
CrossTable(listing_testQ3$review_scores_location, listing_predQ3_boost10, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn=c('actual Optimal review_scores_location', 'predicted Optimal review_scores_location'))


