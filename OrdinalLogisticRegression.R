install.packages("foreign")
install.packages("nnet")
install.packages("reshape2")
install.packages("MASS")
library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)
library(MASS)


##reading the data
listings <- read.csv(file.choose(),header = TRUE, stringsAsFactors = FALSE)
attach(listings)

##----------------------------------
##Question 1: 
##rating <- location,cancellation policy, room type, accomodation available, price

#Convert rating to facors
listings$rating_factor <-factor(with(listings,ifelse((listings$review_scores_rating<20),1,
                                                    ifelse((listings$review_scores_rating<90),2,
                                                           ifelse((listings$review_scores_rating<95),3,
                                                                  ifelse((listings$review_scores_rating<100),4,4))))))
                                                                         



#testing and training
test	<-	which(1:nrow(listings)%%4==0)
new_train_1 <-listings[-test,]
new_test_1 <-listings[test,]

#model
logit_model_1 <- polr(rating_factor ~ neighbourhood_group_cleansed+ cancellation_policy+ room_type+ accommodates+ price,data = listings,Hess = TRUE)
summary(logit_model_1)

#create a table for the summary data and add p-value
logit_table_1 <- coef(summary(logit_model_1))
p1 <- pnorm(abs(logit_table_1[, "t value"]), lower.tail = FALSE) * 2
logit_table_1 <- cbind(logit_table_1, "p-value" = p1)

# Odd Ratios and CI
(ci <- confint(logit_model_1))
confint.default(logit_model_1)

# odds ratios
exp(coef(logit_model_1))
exp(cbind(OR = coef(logit_model_1), ci))


#prediction model
predict_model_1 <-predict(logit_model_1,newdata=new_test_1,type="class")
cor(as.numeric((predict_model_1)), as.numeric((new_test_1$rating_factor)), use="complete")


'-------------------------------------------------'
##Question 2
##Price <- bathrooms+beds+square_feet
###COnvert Price to factors
listings$price_factor <-factor(with(listings,ifelse((listings$price<100),1,
                                                ifelse((listings$price<200),2,
                                                       ifelse((listings$price<300),3,
                                                             ifelse((listings$price<400),4,
                                                                    ifelse((listings$price<500),5,
                                                                           ifelse((listings$price>500),6,6))))))))


#testing and training
test	<-	which(1:nrow(listings)%%4==0)
new_train_2 <-listings[-test,]
new_test_2 <-listings[test,]

#Model
logit_model_2 <- polr(price_factor ~ bathrooms+beds+square_feet+zipcode_freq,data = listings,Hess = TRUE)
summary(logit_model_2)

#create a table for the summary data and add p-value
logit_table_2 <- coef(summary(logit_model_2))
p <- pnorm(abs(logit_table_2[, "t value"]), lower.tail = FALSE) * 2
logit_table_2 <- cbind(logit_table_2, "p-value" = p)

# odds ratios
exp(coef(logit_model_2))

# Odd Ratios and CI
(ci <- confint(logit_model_2))
confint.default(logit_model_2)
exp(cbind(OR = coef(logit_model_2), ci))

#prediction model
predict_model_2 <-predict(logit_model_2,newdata=new_test,type="class")
cor(as.numeric(predict_model_2), as.numeric(new_test_2$price_factor), use="complete")


'-----------------------------------------------'
###Question 3:
##location_rating <- location+ reviews_per_month+ crime rate

#Convert location to factors
listings$review_scores_location_factor <-factor(with(listings,ifelse((listings$review_scores_location<2),1,
                                     ifelse((listings$review_scores_location<3),2,
                                            ifelse((listings$review_scores_location<4),3,
                                                   ifelse((listings$review_scores_location<5),4,
                                                          ifelse((listings$review_scores_location<6),5,
                                                                 ifelse((listings$review_scores_location<7),6,
                                                                        ifelse((listings$review_scores_location<8),7,
                                                                               ifelse((listings$review_scores_location<9),8,
                                                                                      ifelse((listings$review_scores_location<10),9,10)))))))))))



#testing and training
test	<-	which(1:nrow(listings)%%4==0)
new_train_3 <-listings[-test,]
new_test_3 <-listings[test,]

#model
logit_model_3 <- polr(review_scores_location_factor ~ neighbourhood_group_cleansed+ reviews_per_month+ zipcode_freq,data = listings,Hess = TRUE)
summary(logit_model_3)

#create a table for the summary data and add p-value
logit_table_3 <- coef(summary(logit_model_3))
p1 <- pnorm(abs(logit_table_3[, "t value"]), lower.tail = FALSE) * 2
logit_table_3 <- cbind(logit_table_3, "p-value" = p1)
logit_table_3

# Odd Ratios and CI
(ci <- confint(logit_model_3))
confint.default(logit_model_3)

# odds ratios
exp(coef(logit_model_3))
exp(cbind(OR = coef(logit_model_3), ci))


#prediction model
predict_model_3 <-predict(logit_model_3,newdata=new_test,type="class")
cor(as.numeric((predict_model_3)), as.numeric((new_test$neighbourhood_group_cleansed)), use="complete")




