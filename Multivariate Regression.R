
#-------------------------------------------------------------------#
# Model 1 bathrooms+bedrooms+beds+square_feet
newMV1<-temp
newsMV1<-temp[complete.cases(temp[,59]),]
newsMV1<-temp[complete.cases(temp[,60]),]
newsMV1<-temp[complete.cases(temp[,57]),]
newsMV1<-temp[complete.cases(temp[,56]),]
newsMV1<-temp[complete.cases(temp[,55]),]


#testing and training 
testMV1	<-	which(1:nrow(newMV1)%%4==0)
newMV1_train<-newMV1[-testMV1,]
newMV1_test<-newMV1[testMV1,]

model1<-lm(price~bathrooms+bedrooms+beds+square_feet, data=newMV1_train)
prediction1<-predict(model1,newdata=newMV1_test)

str(newMV1)
(newMV1[,55])
#Analysis of Variance
anova(model1)

#Model parameters
names(model1)
model1$coefficients
model1$residuals
model1$qr
summary(model1, corr=T)

# table with fitted values and residuals...check this part
revMV1_train<-data.frame(fitted.value=fitted(model1),residual=resid(model1))


revMV1_train<-data.frame(newMV1_train)
z<-fitted(model1)
z

# Other useful functions
confint(model1, level=0.95) # CIs for model parameters
vcov(model1) # covariance matrix for model parameters
influence(model1) # regression diagnostics 

# optional 4 graphs/page
layout(matrix(c(1,2,3,4),2,2))  
plot(model1)

library(hydroGOF)
#Prediction between real and predicted value
cor(prediction1,newMV1_test$price, use="complete")

#RMSE
rmse(prediction1,newMV1_test$price)


#-------------------------------------------------------------------#
# Model 2 we add review rating to the model

newMV2<-temp
newsMV2<-temp[complete.cases(temp[,60]),]
newsMV2<-temp[complete.cases(temp[,57]),]
newsMV2<-temp[complete.cases(temp[,56]),]
newsMV2<-temp[complete.cases(temp[,55]),]
newsMV2<-temp[complete.cases(temp[,70]),]

#testing and training 
testMV2	<-	which(1:nrow(newMV2)%%4==0)
newMV2_train<-newMV2[-testMV2,]
newMV2_test<-newMV2[testMV2,]

model2<-lm(price~bathrooms+bedrooms+beds+square_feet+review_scores_rating, data=newMV2_train)
prediction2<-predict(model2,newdata=newMV2_test)

str(newMV2)
(newMV2[,55])
#Analysis of Variance
anova(model2)

#Model parameters
names(model2)
model2$coefficients
model2$residuals
model2$qr
summary(model2, corr=T)

# table with fitted values and residuals...check this part
revMV2_train<-data.frame(fitted.value=fitted(model2),residual=resid(model2))


revMV2_train<-data.frame(newMV2_train)
z<-fitted(model2)
z

# Other useful functions
confint(model2, level=0.95) # CIs for model parameters
vcov(model2) # covariance matrix for model parameters
influence(model2) # regression diagnostics 

# optional 4 graphs/page
layout(matrix(c(1,2,3,4),2,2))  
plot(model2)

library(hydroGOF)
#Prediction between real and predicted value
cor(prediction2,newMV2_test$price, use="complete")

#RMSE
rmse(prediction2,newMV2_test$price)



#-------------------------------------------------------------------#

# Model 3 Interaction variables

newMV3<-temp
newsMV3<-temp[complete.cases(temp[,60]),]
newsMV3<-temp[complete.cases(temp[,57]),]
newsMV3<-temp[complete.cases(temp[,56]),]
newsMV3<-temp[complete.cases(temp[,55]),]


#testing and training 
testMV3	<-	which(1:nrow(newMV3)%%4==0)
newMV3_train<-newMV3[-testMV3,]
newMV3_test<-newMV3[testMV3,]

model3<-lm(price~bathrooms*bedrooms+beds+square_feet, data=newMV1_train)
prediction3<-predict(model3,newdata=newMV3_test)

str(newMV3)

#Analysis of Variance
anova(model3)

#Model parameters
names(model3)
model3$coefficients
model3$residuals
model3$qr
summary(model3, corr=T)

# table with fitted values and residuals...check this part
revMV3_train<-data.frame(fitted.value=fitted(model3),residual=resid(model3))


revMV1_train<-data.frame(newMV1_train)


# Other useful functions
confint(model3, level=0.95) # CIs for model parameters
vcov(model3) # covariance matrix for model parameters
influence(model3) # regression diagnostics 

# optional 4 graphs/page
layout(matrix(c(1,2,3,4),2,2))  
plot(model3)

library(hydroGOF)
#Prediction between real and predicted value
cor(prediction3,newMV3_test$price, use="complete")

#RMSE
rmse(prediction3,newMV3_test$price)


#-------------------------------------------------------------------#

#model 4 adding crime rate
temp1<-new311
View(newMV4)
newMV4<-temp1
newMV4<-temp1[complete.cases(temp[,60]),]
newMV4<-temp1[complete.cases(temp[,57]),]
newMV4<-temp1[complete.cases(temp[,56]),]
newMV4<-temp1[complete.cases(temp[,55]),]
newMV4<-temp1[complete.cases(temp[,70]),]

newMV4<-temp1[complete.cases(temp1[,97]),]

newMV4<-newMV4[newMV4$crimerate_frequency>1000,]
newMV4<-newMV4[newMV4$price<2000,]
colnames(newMV4)[97] <- "crimerate_frequency"

#testing and training 
testMV4	<-	which(1:nrow(newMV4)%%4==0)
newMV4_train<-newMV4[-testMV4,]
newMV4_test<-newMV4[testMV4,]

model4<-lm(price~bathrooms+bedrooms+beds+square_feet+review_scores_rating+crimerate_frequency, data=newMV4_train)
prediction4<-predict(model4,newdata=newMV4_test)

str(newMV4)
(newMV2[,55])
#Analysis of Variance
anova(model4)

#Model parameters
names(model4)
model4$coefficients
model4$residuals
model4$qr
summary(model4, corr=T)

# table with fitted values and residuals...check this part
revMV4_train<-data.frame(fitted.value=fitted(model4),residual=resid(model4))




# Other useful functions
confint(model4, level=0.95) # CIs for model parameters
vcov(model4) # covariance matrix for model parameters
influence(model4) # regression diagnostics 

# optional 4 graphs/page
layout(matrix(c(1,2,3,4),2,2))  
plot(model4)

library(hydroGOF)
#Prediction between real and predicted value
cor(prediction4,newMV4_test$price, use="complete")

#RMSE
rmse(prediction2,newMV2_test$price)

#-------------------------------------------------------------------#

#model 5 adding reviews per month

View(newMV5)

newMV5<-temp1
newMV5<-temp1[complete.cases(temp[,60]),]
newMV5<-temp1[complete.cases(temp[,57]),]
newMV5<-temp1[complete.cases(temp[,56]),]
newMV5<-temp1[complete.cases(temp[,55]),]
newMV5<-temp1[complete.cases(temp[,70]),]

newMV5<-temp1[complete.cases(temp1[,97]),]

newMV5<-newMV5[newMV5$crimerate_frequency>1000,]
newMV5<-newMV5[newMV5$price<2000,]
colnames(newMV5)[97] <- "crimerate_frequency"

#testing and training 
testMV5	<-	which(1:nrow(newMV5)%%4==0)
newMV5_train<-newMV4[-testMV5,]
newMV5_test<-newMV4[testMV5,]

model5<-lm(price~bathrooms+bedrooms+beds+square_feet+review_scores_rating+crimerate_frequency+reviews_per_month, data=newMV5_train)
prediction5<-predict(model5,newdata=newMV5_test)

str(newMV4)
(newMV2[,55])
#Analysis of Variance
anova(model5)

#Model parameters
names(model5)
model5$coefficients
model5$residuals
model5$qr
summary(model5, corr=T)

# table with fitted values and residuals...check this part
revMV5_train<-data.frame(fitted.value=fitted(model4),residual=resid(model4))




# Other useful functions
confint(model5, level=0.95) # CIs for model parameters
vcov(model5) # covariance matrix for model parameters
influence(model5) # regression diagnostics 

# optional 4 graphs/page
layout(matrix(c(1,2,3,4),2,2))  
plot(model5)

library(hydroGOF)
#Prediction between real and predicted value
cor(prediction5,newMV5_test$price, use="complete")

#RMSE
rmse(prediction5,newMV5_test$price)
