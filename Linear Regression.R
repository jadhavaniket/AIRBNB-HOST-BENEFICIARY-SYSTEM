
install.packages("hydroGOF")
library(ggplot2)
library(hydroGOF)



#-------------------------------------------------------------------------------------------------------------#

#Model 1 Predicting the price for the square feet
#Remove nullvalues from Square_feet, remove valuesless than 100 and greater than 3000
  

#cleaning data
newsquare<-temp[complete.cases(temp[,60]),]
newsquare=newsquare[newsquare$square_feet>100 & newsquare$square_feet<5000,]

#intial plots
attach(newsquare)

plot(square_feet,price,xlab="sqft",
     ylab="price",pch=16)


fit.lm <- lm(price ~ square_feet)
fit.lm
abline(fit.lm,col="red",lwd=4)


#squarearea.fit<-function(x) fit.lm$coefficient[1]+fit.lm$coefficient[2]*x
#x.pred<-c(500,600,700,800)
#squarearea.fit(x.pred)


  # Divide as training and testing
  #20% test 80% train
testdx	<-	which(1:nrow(newsquare)%%4==0)
newsquare_train<-newsquare[-testdx,]
newsquare_test<-newsquare[testdx,]

#train the model
model<-lm(price~square_feet,data=newsquare_train)
prediction<-predict(model,newdata=newsquare_test)

# Correlation
cor(prediction,newsquare_test$price, use='complete')

#root mean square error
rmse(prediction,newsquare_test$price)


# General plot
plot(newsquare$square_feet, newsquare$price)


# Plot a line on data
abline(model, col="red",lwd=3)

#Model parameters
names(model)
model$coefficients
model$residuals
model$qr
outlierTest(model)
summary(model)



#other variables of model
confint(model, level=0.95) 
fitted(model) # predicted values
residuals(model) # residuals
anova(model) # anova table 
vcov(model) # covariance matrix for model parameters 
influence(model) # regression diagnostics



# optional 4 graphs/page
layout(matrix(c(1,2,3,4),2,2))  
plot(model)

# Confidence and Prediction bands
predframe<-data.frame(newsquare_test$square_feet)
colnames(predframe)=c("square_feet")
View(predframe)
pp<-predict(model,newdata=predframe,interval="prediction")
str(pp)

pc<-predict(model,newdata=predframe,interval="confidence")
plot(newsquare_test$square_feet,newsquare_test$price)
View(newsquare_test)
length(newsquare_test$square_feet)
length(newsquare_test$price)
matlines(predframe,pc)
matlines(predframe,pp)

#-----------------------------------------------------------------------------------------------#
#Model 2 Price  and Beds

View(temp)
#cleaning data
newbed<-temp[complete.cases(temp[,57]),]
newbed=newbed[newbed$beds>0 & newbed$price<2000 & newbed$price>50,]

newbed$beds<-as.numeric(as.character(newbed$beds))

# Divide as training and testing
#20% test 80% train
testbed	<-	which(1:nrow(newbed)%%4==0)
newbed_train<-newbed[-testbed,]
newsbed_test<-newbed[testbed,]


#train the model
modelbed<-lm(price~beds,data=newbed_train)
predictionbed<-predict(modelbed,newdata=newsbed_test)

# Correlation
cor(predictionbed,newsbed_test$price, use='complete')


#root mean square error
rmse(predictionbed,newsbed_test$price)

anova(modelbed) # anova table


# General plot
plot(newbed$beds, newbed$price)


# Plot a line on data
abline(modelbed, col="red",lwd=3)


#Model parameters
names(modelbed)
modelbed$coefficients
modelbed$residuals
modelbed$qr
outlierTest(modelbed)
summary(modelbed)



#other variables of model
confint(modelbed, level=0.95) 
fitted(modelbed) # predicted values
residuals(modelbed) # residuals
anova(modelbed) # anova table 
vcov(modelbed) # covariance matrix for model parameters 
influence(modelbed) # regression diagnostics



# optional 4 graphs/page
layout(matrix(c(1,2,3,4),2,2))  
plot(modelbed)

# Confidence and Prediction bands
predframe2<-data.frame(newsbed_test$beds)
colnames(predframe2)=c("beds")
View(predframe2)
pp2<-predict(modelbed,newdata=predframe2,interval="prediction")
str(pp2)

pc2<-predict(modelbed,newdata=predframe2,interval="confidence")
plot(newsbed_test$beds,newsbed_test$price)
View(newsbed_test)
length(newsbed_test$beds)
length(newsbed_test$price)
matlines(predframe2,pc2)
matlines(predframe2,pp2)




#-------------------------------------------------------------------------------------------------------------#

#Model 3 Price and accomodates

View(temp)
#cleaning data
View(newacc)
str(newacc)
newacc<-temp
newacc<-temp[complete.cases(temp[,54]),]
newacc=newacc[newacc$price<2000 & newacc$price>50,]

newacc$accommodates<-as.numeric(as.character(newacc$accommodates))

# Divide as training and testing
#20% test 80% train
testacc	<-	which(1:nrow(newacc)%%4==0)
newacc_train<-newacc[-testacc,]
newacc_test<-newacc[testacc,]

#train the model
modelacc<-lm(price~accommodates,data=newacc_train)
predictionacc<-predict(modelacc,newdata=newacc_test)

# Correlation
cor(predictionacc,newacc_test$price, use='complete')


#root mean square error
rmse(predictionacc,newacc_test$price)


# General plot
plot(newacc$accommodates, newacc$price)


# Plot a line on data
abline(modelacc, col="red",lwd=3)


#Model parameters
names(modelacc)
modelacc$coefficients
modelacc$residuals
modelacc$qr
outlierTest(modelacc)
summary(modelacc)



#other variables of model
confint(modelacc, level=0.95) 
fitted(modelacc) # predicted values
residuals(modelacc) # residuals
anova(modelacc) # anova table 
vcov(modelacc) # covariance matrix for model parameters 
influence(modelacc) # regression diagnostics



# optional 4 graphs/page
layout(matrix(c(1,2,3,4),2,2))  
plot(modelacc)

# Confidence and Prediction bands
predframeacc<-data.frame(newacc_test$accommodates)
colnames(predframeacc)=c("accommodates")
View(predframeacc)
ppacc<-predict(modelacc,newdata=predframeacc,interval="prediction")
str(ppa30)

pcacc<-predict(modelacc,newdata=predframeacc,interval="confidence")
plot(newacc_test$accommodates,newacc_test$price)
View(newacc_test)
length(newacc_test$accommodates)
length(newacc_test$price)
matlines(predframeacc,pcacc)
matlines(predframeacc,ppacc)


#-------------------------------------------------------------------------------------------------------------#

#Model 4 Price  and availability for 30 days
newa30<-temp
newa30<-temp[complete.cases(temp[,71]),]
newa30<-newa30[newa30$price>100 & newa30$price<2000 & newa30$availability_30>0,]

 
# Divide as training and testing
#20% test 80% train
testa30	<-	which(1:nrow(newa30)%%4==0)
newa30_train<-newa30[-testa30,]
newa30_test<-newa30[testa30,]

#train the model
modela30<-lm(price~availability_30,data=newa30_train)
predictiona30<-predict(modela30,newdata=newa30_test)

# Correlation
cor(predictiona30,newa30_test$price, use='complete')


#root mean square error
rmse(predictiona30,newa30_test$price)


# General plot
plot(newa30$availability_30, newa30$price)


# Plot a line on data
abline(modela30, col="red",lwd=3)


#Model parameters
names(modela30)
modela30$coefficients
modela30$residuals
modela30$qr
outlierTest(modela30)
summary(modela30)



#other variables of model
confint(modela30, level=0.95) 
fitted(modela30) # predicted values
residuals(modela30) # residuals
anova(modela30) # anova table 
vcov(modela30) # covariance matrix for model parameters 
influence(modela30) # regression diagnostics



# optional 4 graphs/page
layout(matrix(c(1,2,3,4),2,2))  
plot(modela30)

# Confidence and Prediction bands
predframea30<-data.frame(newa30_test$availability_30)
colnames(predframea30)=c("availability_30")
View(predframea30)
ppa30<-predict(modela30,newdata=predframea30,interval="prediction")
str(ppa30)

pca30<-predict(modela30,newdata=predframea30,interval="confidence")
plot(newa30_test$availability_30,newa30_test$price)
View(newsbed_test)
length(newa30_test$beds)
length(newa30_test$price)
matlines(predframea30,pca30)
matlines(predframea30,ppa30)


#-------------------------------------------------------------------------------------------------------------#

#Model 5  311 calls and price
temp1<-read.csv("311.csv",stringsAsFactors = FALSE)
View(new311)
new311<-temp1
new311<-temp1[complete.cases(temp1[,97]),]
colnames(new311)[97] <- "crimerate_frequency"

new311<-new311[new311$crimerate_frequency>1000 & new311$price<2000,]


# Divide as training and testing
#20% test 80% train
test311	<-	which(1:nrow(new311)%%4==0)
new311_train<-new311[-test311,]
new311_test<-new311[test311,]

#train the model

model311<-lm(price~crimerate_frequency,data=new311_train)
prediction311<-predict(model311,newdata=new311_test)

# Correlation
cor(prediction311,new311_test$price, use='complete')


#root mean square error
rmse(prediction311,new311_test$price)


# General plot
plot(new311$crimerate_frequency, new311$price)


# Plot a line on data
abline(model311, col="red",lwd=3)


#Model parameters
names(model311)
model311$coefficients
model311$residuals
model311$qr
outlierTest(model311)
summary(model311)



#other variables of model
confint(model311, level=0.95) 
fitted(model311) # predicted values
residuals(model311) # residuals
anova(model311) # anova table 
vcov(model311) # covariance matrix for model parameters 
influence(model311) # regression diagnostics



# optional 4 graphs/page
layout(matrix(c(1,2,3,4),2,2))  
plot(model311)

# Confidence and Prediction bands
predframe311<-data.frame(new311_test$crimerate_frequency)
colnames(predframe311)=c("crimerate_frequency")
View(predframe311)
pp311<-predict(model311,newdata=predframe311,interval="prediction")
str(ppa30)

pc311<-predict(model311,newdata=predframe311,interval="confidence")
plot(new311_test$crimerate_frequency,new311_test$price)
View(new311_test)
length(new311_test$crimerate_frequency)
length(new311_test$price)
matlines(predframe311,pc311)
matlines(predframe311,pp311)

#-------------------------------------------------------------------------------------------------------------#

#Model 6  Avg number of reviews and price

# Data Cleaning
newrev<-temp

newrev<-temp[complete.cases(temp[,93]),]
newrev<-newrev[newrev$price<2000,]

# Divide as training and testing
#20% test 80% train
testrev	<-	which(1:nrow(newrev)%%4==0)
newrev_train<-newrev[-testrev,]
newrev_test<-newrev[testrev,]

#train the model

modelrev<-lm(price~reviews_per_month,data=newrev_train)
predictionrev<-predict(modelrev,newdata=newrev_test)

# Correlation
cor(predictionrev,newrev_test$price, use='complete')


#root mean square error
rmse(predictionrev,newrev_test$price)


# General plot
plot(newrev$reviews_per_month, newrev$price)


# Plot a line on data
abline(modelrev, col="red",lwd=3)


#Model parameters
names(modelrev)
modelrev$coefficients
modelrev$residuals
modelrev$qr
outlierTest(modelrev)
summary(modelrev)



#other variables of model
confint(modelrev, level=0.95) 
fitted(modelrev) # predicted values
residuals(modelrev) # residuals
anova(modelrev) # anova table 
vcov(modelrev) # covariance matrix for model parameters 
influence(modelrev) # regression diagnostics



# optional 4 graphs/page
layout(matrix(c(1,2,3,4),2,2))  
plot(modelrev)

# Confidence and Prediction bands
predframerev<-data.frame(newrev_test$reviews_per_month)
colnames(predframerev)=c("reviews_per_month")
View(predframerev)
pprev<-predict(modelrev,newdata=predframerev,interval="prediction")
str(pprev)

pcrev<-predict(modelrev,newdata=predframerev,interval="confidence")
plot(newrev_test$reviews_per_month,newrev_test$price)
View(newrev_test)
length(newrev_test$reviews_per_month)
length(newrev_test$price)
matlines(predframerev,pcrev)
matlines(predframerev,pprev)

#-------------------------------------------------------------------------------------------------------------#

#Model 7  Property listing age in DAYS and price


temp$newd<- as.Date(temp$host_since) #add new column to convert date from charachter format to date foramt
View(temp)
temp$age_of_listing <- difftime(Sys.Date(),temp$newd,units='days') # gives age of listing in DAYS by substracting current date and lisintg date
new_ageoflisting<-temp
new_ageoflisting<-temp[complete.cases(temp[,95]),]
new_ageoflisting$age_of_listing = as.numeric(new_ageoflisting$age_of_listing)
new_ageoflisting<-new_ageoflisting[new_ageoflisting$price<2000 & new_ageoflisting$age_of_listing>100 & new_ageoflisting$age_of_listing<2500,]

# Divide as training and testing
#20% test 80% train
test_ageoflisting	<-	which(1:nrow(new_ageoflisting)%%4==0)
new_ageoflisting_train<-new_ageoflisting[-test_ageoflisting,]
new_ageoflisting_test<-new_ageoflisting[testrev,]

any(is.na(new_ageoflisting_test))

#train the model

model_ageoflisting<-lm(price~age_of_listing,data=new_ageoflisting_train)
prediction_ageoflisting<-predict(model_ageoflisting,newdata=new_ageoflisting_test)

# Correlation
cor(prediction_ageoflisting,new_ageoflisting_test$price, use='complete')


#root mean square error
rmse(prediction_ageoflisting,new_ageoflisting_test$price)


# General plot
plot(new_ageoflisting$age_of_listing, new_ageoflisting$price)


# Plot a line on data
abline(model_ageoflisting, col="red",lwd=3)


#Model parameters
names(model_ageoflisting)
model_ageoflisting$coefficients
model_ageoflisting$residuals
model_ageoflisting$qr
outlierTest(model_ageoflisting)
summary(model_ageoflisting)



#other variables of model
confint(model_ageoflisting, level=0.95) 
fitted(model_ageoflisting) # predicted values
residuals(model_ageoflisting) # residuals
anova(model_ageoflisting) # anova table 
vcov(model_ageoflisting) # covariance matrix for model parameters 
influence(model_ageoflisting) # regression diagnostics



# optional 4 graphs/page
layout(matrix(c(1,2,3,4),2,2))  
plot(model_ageoflisting)

# Confidence and Prediction bands
predframe_ageoflisting<-data.frame(new_ageoflisting_test$age_of_listing)
predframe_ageoflisting<-na.omit(predframe_ageoflisting)
any(is.na(predframe_ageoflisting))
colnames(predframe_ageoflisting)=c("age_of_listing")
View(predframe_ageaoflisting)
pp_ageoflisting<-predict(model_ageoflisting,newdata=predframe_ageoflisting,interval="prediction")
str(pprev)



pc_ageoflisting<-predict(model_ageoflisting,newdata=predframe_ageoflisting,interval="confidence")
plot(new_ageoflisting_test$age_of_listing,new_ageoflisting_test$price)
View(new_ageoflisting_test)
length(new_ageoflisting_test$age_of_listing)
length(new_ageoflisting_test$price)
matlines(predframe_ageoflisting,pc_ageoflisting)
matlines(predframe_ageoflisting,pp_ageoflisting)


View(temp)
#-------------------------------------------------------------------------------------------------------------#

#Model 8  price and reviews

newR<-temp
newR<-temp[complete.cases(temp[,79]),]
newR<-newR[newR$price<2000,]
# Divide as training and testing
#20% test 80% train
testR	<-	which(1:nrow(newR)%%4==0)
newR_train<-newrev[-testR,]
newR_test<-newrev[testR,]

#train the model

modelR<-lm(price~review_scores_rating,data=newR_train)
predictionR<-predict(modelR,newdata=newR_test)

# Correlation
cor(predictionR,newR_test$price, use='complete')


#root mean square error
rmse(predictionR,newR_test$price)


# General plot
plot(newR$review_scores_rating, newR$price)


# Plot a line on data
abline(modelR, col="red",lwd=3)


#Model parameters
names(modelR)
modelR$coefficients
modelR$residuals
modelR$qr
outlierTest(modelR)
summary(modelR)



#other variables of model
confint(modelR, level=0.95) 
fitted(modelR) # predicted values
residuals(modelR) # residuals
anova(modelR) # anova table 
vcov(modelR) # covariance matrix for model parameters 
influence(modelR) # regression diagnostics



# optional 4 graphs/page
layout(matrix(c(1,2,3,4),2,2))  
plot(modelR)

# Confidence and Prediction bands
predframeR<-data.frame(newR_test$review_scores_rating)
colnames(predframeR)=c("review_scores_rating")
View(predframeR)
ppR<-predict(modelR,newdata=predframeR,interval="prediction")
str(pprev)

pcR<-predict(modelR,newdata=predframeR,interval="confidence")
plot(newR_test$review_scores_rating,newR_test$price)
View(newR_test)
length(newR_test$review_scores_rating)
length(newR_test$price)
matlines(predframeR,pcR)
matlines(predframeR,ppR)




