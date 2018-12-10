listings <- read.csv(file.choose(),header = TRUE, stringsAsFactors = FALSE)
library(aod)
library(ggplot2)
library(caret)

##QUESTION 1: Predicting factors that contribute towards a good rating

#creating a new column to convert continuous variable to binary using median
listings$review_scores_Goodrating <- ifelse(listings$review_scores_rating >= 95, 1, 0)
listings$review_scores_Goodrating <- factor(listings$review_scores_Goodrating)
listings$review_scores_Goodrating
model <- glm( listings$review_scores_Goodrating~listings$neighbourhood_group_cleansed+listings$room_type+listings$price+listings$accommodates+listings$cancellation_policy,data = listings,family = "binomial")
model
summary(model)
varImp(model,scale=FALSE)
#odd ratios
exp(coef(model))

#Prediction 
# Divide as training and testing
#20% test 80% train
testdx	<-	which(1:nrow(listings)%%4==0)
listings_train<-listings[-testdx,]
listings_test<-listings[testdx,]


#Creating a dataframe with the required columns and a column with the predicted probabilities
newdata1 <- with(listings,data.frame(listings$neighbourhood_group_cleansed,listings$room_type,listings$price,listings$accommodates,listings$review_scores_Goodrating,listings$cancellation_policy))
newdata1$ratingP <- predict(model,newdata=newdata1,type="response")


#Plot
ggplot(newdata1,aes(x= listings.price, y= ratingP),xlab = "Price of the listing",ylab ="Rating of the listing") + geom_line(aes(color=ratingP),size=1)

##To be done

##QUESTION 2: PREDICT OPTIMAL PRICING FOR A GOOD RATING

#creating a new column to convert continuous variable to binary using median
#exploratory analysis
mean(listings$price)
median(price)
range(price)
hist(price)

listings$price_factor <- ifelse(price >= median(price), 1, 0)
View(listings)
listings$price_factor <- factor(listings$price_factor)
listings$price_factor
model1 <- glm(price_factor ~ bathrooms + bedrooms + beds + neighbourhood_group_cleansed + room_type, data = listings, family = "binomial")
model1
summary(model1)
varImp(model1,scale=FALSE)
#odd ratios
exp(coef(model1))

#Prediction 
# Divide as training and testing
#20% test 80% train
testdx	<-	which(1:nrow(listings)%%4==0)
listings_train<-listings[-testdx,]
listings_test<-listings[testdx,]


#Creating a dataframe with the required columns and a column with the predicted probabilities
newdata1 <- with(listings,data.frame(neighbourhood_group_cleansed, bathrooms, bedrooms, beds,room_type,price_factor))
newdata1$PriceP <- predict(model1,newdata=newdata1,type="response")
View(newdata1)


## QUESTION 3: PREDICTING THE OPTIMAL LOCATION FOR A NEW LISTING
#creating a new column to convert continuous variable to binary using median
#exploratory analysis
mean(listings$price)
median(price)
range(price)
hist(price)

listings$price_factor <- ifelse(price >= median(price), 1, 0)
View(listings)
listings$price_factor <- factor(listings$price_factor)
listings$price_factor
model1 <- glm(price_factor ~ bathrooms + bedrooms + beds + neighbourhood_group_cleansed + room_type, data = listings, family = "binomial")
model1
summary(model1)
varImp(model1,scale=FALSE)
#odd ratios
exp(coef(model1))

#Prediction 
# Divide as training and testing
#20% test 80% train
testdx	<-	which(1:nrow(listings)%%4==0)
listings_train<-listings[-testdx,]
listings_test<-listings[testdx,]


#Creating a dataframe with the required columns and a column with the predicted probabilities
newdata1 <- with(listings,data.frame(neighbourhood_group_cleansed, bathrooms, bedrooms, beds,room_type,price_factor))
newdata1$PriceP <- predict(model1,newdata=newdata1,type="response")
View(newdata1)


