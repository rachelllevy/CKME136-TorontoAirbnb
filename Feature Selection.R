#Install Packages

install.packages('RCurl')
install.packages('MASS')
install.packages('leaps')
library(RCurl) # getURL 
library(MASS) # stepwise regression
library(leaps) # all subsets regression

#Remove char variables

char_variables <- c("Name","Description","Host Verifications","Amenities","Geolocation","Features","Host Neighbourhood","Zipcode","Neighbourhood Cleansed","Property Type","Room Type","Bed Type","Calendar Updated","Cancellation Policy")
cleaned_nochar <- cleaned_data_2[ , !(names(cleaned_data_2) %in% char_variables)]


#Create training and test set - 80% and 20% 

rn_train <- sample(nrow(cleaned_nochar), floor(nrow(cleaned_nochar)*0.8))
train <- cleaned_nochar[rn_train,]
test <- cleaned_nochar[-rn_train,]

#Build a multiple linear regression model to predict the 'price' variable. Will train the model on the training set and do the prediction on the test set.

model_mlr <- lm(Price~., data= train)
prediction <- predict(model_mlr, interval="prediction", newdata = test)
model_mlr

#Plot errors on a histogram. 

errors <- prediction[,"fit"] - test$Price
hist(errors)

#Compute the root mean square error and find the percentage of cases with less than 25% error.

rmse <- sqrt(sum((prediction[,"fit"] - test$Price)^2)/nrow(test))
rmse
#68.54017
rel_change <- 1 - ((test$Price - abs(errors)) / test$Price)

pred25 <- table(rel_change<0.25)["TRUE"] / nrow(test)
table(rel_change)
paste("RMSE:", rmse)
paste("PRED(25):", pred25)
#"PRED(25): 0.433306055646481"

#Forward Selection

full <- lm(Price~.,data=cleaned_nochar)
null <- lm(Price~1,data=cleaned_nochar)
stepF <- stepAIC(null, scope=list(lower=null, upper=full), direction= "forward", trace=TRUE)
summary(stepF)

##Forward Step: AIC=102432.3
#Price ~ Accommodates + `Cleaning Fee` + Zipcode + `Calendar Updated` + 
  #`Room Type` + `Availability 30` + `Property Type` + `Number of Reviews` + 
  #`Review Scores Rating` + Latitude + Longitude + `Extra People` + 
  #`Cancellation Policy` + `Host Listings Count`


stepB <- stepAIC(full, direction= "backward", trace=TRUE)
summary(stepB)

#Backward Step:  AIC=102432.3
#Price ~ `Host Listings Count` + Zipcode + Latitude + Longitude + 
#`Property Type` + `Room Type` + Accommodates + `Cleaning Fee` + 
#`Extra People` + `Calendar Updated` + `Availability 30` + 
#`Number of Reviews` + `Review Scores Rating` + `Cancellation Policy`



