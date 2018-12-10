library(gmodels)
library(e1071)
library(caret)



##reading the data
listings <- read.csv(file.choose(),header = TRUE, stringsAsFactors = FALSE)
attach(listings)

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
rating_classifier <- naiveBayes(rating_factor ~ neighbourhood_group_cleansed+ cancellation_policy+ room_type+ accommodates+price,data=new_train_1,laplace = 0,na.action=na.pass) 
rating_pred<-predict(rating_classifier,new_test_1)


m <- CrossTable(rating_pred,new_test_1$rating_factor,prop.chisq=FALSE,prot.t=FALSE,prop.r=FALSE,dnn=c("predicted","actual"))


##model with laplace estimator
rating_classifier_laplace <- naiveBayes(rating_factor ~ neighbourhood_group_cleansed+ cancellation_policy+ room_type+ accommodates+price,data=new_train_1,laplace = 1,na.action=na.pass) 
rating_pred<-predict(rating_classifier_laplace,new_test_1)

m <- CrossTable(rating_pred,new_test_1$rating_factor,prop.chisq=FALSE,prot.t=FALSE,prop.r=FALSE,dnn=c("predicted","actual"))


##Question 2
##Price <- bathrooms+bedrooms+beds+square_feet

###COnvert Price to factors
listings$price_factor <-factor(with(listings,ifelse((listings$price<100),1,
                                                    ifelse((listings$price<200),2,
                                                           ifelse((listings$price<300),3,
                                                                  ifelse((listings$price<400),4,
                                                                         ifelse((listings$price<500),5,
                                                                                ifelse((listings$price>500),6,6))))))))


#training and testing
test	<-	which(1:nrow(listings)%%4==0)
new_train_2 <-listings[-test,]
new_test_2 <-listings[test,]

#model
price_classifier <- naiveBayes(price_factor ~ bathrooms+ beds+ square_feet+ zipcode_freq,data=new_train_2,laplace = 0,na.action=na.pass) 
price_pred<-predict(price_classifier,new_test_2)


m <- CrossTable(price_pred,new_test_2$price_factor,prop.chisq=FALSE,prot.t=FALSE,prop.r=FALSE,dnn=c("predicted","actual"))
length(price_pred)
precision <- diag(m)/rowSums(m)

##model with laplace estimator
price_classifier_laplace <- naiveBayes(price_factor ~ bathrooms+ beds+ square_feet+ zipcode_freq,data=new_train_2,laplace = 1,na.action=na.pass) 
price_pred<-predict(price_classifier_laplace,new_test_2)

m <- CrossTable(price_pred,new_test_2$price_factor,prop.chisq=FALSE,prot.t=FALSE,prop.r=FALSE,dnn=c("predicted","actual"))


##Question 3

###Question 3:
##location <- location+ reviews_per_month+ crime rate

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
location_classifier <- naiveBayes(review_scores_location_factor ~neighbourhood_group_cleansed+ reviews_per_month+ zipcode_freq,data=new_train_3,laplace = 0,na.action=na.pass) 
location_pred<-predict(location_classifier,new_test_3)

m <- CrossTable(location_pred,new_test_3$review_scores_location_factor,prop.chisq=FALSE,prot.t=FALSE,prop.r=FALSE,dnn=c("predicted","actual"))


##model with laplace estimator
location_classifier_laplace <- naiveBayes(review_scores_location_factor ~ neighbourhood_group_cleansed+ reviews_per_month+ zipcode_freq,data=new_train_3,laplace = 1,na.action=na.pass) 
location_pred<-predict(location_classifier_laplace,new_test_3)

m <- CrossTable(location_pred,new_test_3$review_scores_location_factor,prop.chisq=FALSE,prot.t=FALSE,prop.r=FALSE,dnn=c("predicted","actual"))



