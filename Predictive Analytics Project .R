##################################
#Project II: Predictive Analytics#
##################################

#Import Cleaned Data#
read.table('/Users/skyepetersen/Desktop/Intro to Bus Analytics/fixed_dataset.txt')
cleaned_data<-read.table('/Users/skyepetersen/Desktop/Intro to Bus Analytics/fixed_dataset.txt')
View(cleaned_data)
library(ggplot2)
library(plyr)
library(tseries)

#Transforming the Data#
cleaned_data$Elec2<-cleaned_data$Elec^2 #Quadratic transformation (2nd order)
cleaned_data$ln_Elec<-log(cleaned_data$Elec) 

#####Section 1: Partitioning the data#####
#fraction of sample to be used for training
p<-.7 #use 70% of data to train/build model

#number of observations (rows) in the dataframe
obs_count<-dim(cleaned_data)[1]
obs_count

#Number of observations to be selected for the training partition
#the floor() function rounds down to the nearest integer
training_size <- floor(p * obs_count) 
training_size 
#set the seed to make your partition reproducible
set.seed(1234)
#create a vector with the shuffled row numbers of the original dataset
train_ind <- sample(obs_count, size = training_size)

Training <- cleaned_data[train_ind, ] #pulls random rows for training
Testing <- cleaned_data[-train_ind, ] #pulls random rows for testing

dim(Training) #checking the dimensions of the partitioned data 
dim(Testing)

#####Section2: Building Linear Regressions for each Independent Variable#####
#ModelTest1#
#testing fit of Elec vs. CO2 
MODELTest1<-lm(CO2 ~ Elec, cleaned_data)
summary(MODELTest1)

#ModelTest2#
#testing fit of Coal vs. CO2 
MODELTest2<-lm(CO2 ~ Coal, cleaned_data)
summary(MODELTest2)

#ModelTest3#
#testing fit of Oil vs. CO2 
MODELTest3<-lm(CO2 ~ Oil, cleaned_data)
summary(MODELTest3)

#ModelTest4#
#A Multiple linear regression model comparing 4 variables to CO2 
MODELTest4<-lm(CO2 ~ Gas, cleaned_data)
summary(MODELTest4) 

######Section2,3,4: Linear Models for testing and Training#####  
##### Model 1 #####
M1 <- lm(CO2 ~ Elec, Training)  
summary(M1) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_1_IN <- predict(M1, Training) #generate predictions on the (in-sample) training data
View(PRED_1_IN)
View(M1$fitted.values) #these are the same as the fitted values ##might not need this 

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_1_OUT <- predict(M1, Testing) #generate predictions on the (out-of-sample) testing data
View(PRED_1_OUT)

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_1_IN<-sqrt(sum((PRED_1_IN-Training$CO2)^2)/length(PRED_1_IN))  #computes in-sample error
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-Testing$CO2)^2)/length(PRED_1_OUT)) #computes out-of-sample 

RMSE_1_IN #IN-SAMPLE ERROR
RMSE_1_OUT #OUT-OF-SAMPLE ERROR

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS
x_grid <- seq(0,7000,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M1, list(Elec=x_grid))
plot(Training$CO2 ~ Training$Elec, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$CO2 ~ Testing$Elec, col='red', pch=3)

##### Model 2 #####
#incorporating non-linear (polynomial) transformation of Electricity 
M2 <- lm(CO2 ~ Elec + Elec2, Training) 
summary(M2) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_2_IN <- predict(M2, Training) #generate predictions on the (in-sample) training data
View(PRED_2_IN)
View(M2$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_2_OUT <- predict(M2, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_2_IN<-sqrt(sum((PRED_2_IN-Training$CO2)^2)/length(PRED_2_IN))  #computes in-sample error
RMSE_2_OUT<-sqrt(sum((PRED_2_OUT-Testing$CO2)^2)/length(PRED_2_OUT)) #computes out-of-sample

RMSE_2_IN #IN-SAMPLE ERROR
RMSE_2_OUT #OUT-OF-SAMPLE ERROR

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS
x_grid <- seq(0,7000,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M2, list(Elec=x_grid, Elec2=x_grid^2))
plot(Training$CO2 ~ Training$Elec, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$CO2 ~ Testing$Elec, col='red', pch=3)

##### Model 3 #####
M3<-lm(CO2 ~ ln_Elec, Training)
summary(M3)

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_3_IN <- predict(M3, Training) #generate predictions on the (in-sample) training data
View(PRED_3_IN)
View(M3$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_3_OUT <- predict(M3, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_3_IN<-sqrt(sum((PRED_3_IN-Training$CO2)^2)/length(PRED_3_IN))  #computes in-sample error
RMSE_3_OUT<-sqrt(sum((PRED_3_OUT-Testing$CO2)^2)/length(PRED_3_OUT)) #computes out-of-sample

RMSE_3_IN #IN-SAMPLE ERROR
RMSE_3_OUT #OUT-OF-SAMPLE ERROR

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS
x_grid <- seq(0,7000,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M3, list(ln_Elec=log(x_grid)))
plot(Training$CO2 ~ Training$Elec, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$CO2 ~ Testing$Elec, col='red', pch=3)

##### Model 4 ######
M4 <- lm(CO2 ~ Elec + Coal, Training)
summary(M4)

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_4_IN <- predict(M4, Training) #generate predictions on the (in-sample) training data
View(PRED_4_IN)
View(M4$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_4_OUT <- predict(M4, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_4_IN<-sqrt(sum((PRED_4_IN-Training$CO2)^2)/length(PRED_4_IN))  #computes in-sample error
RMSE_4_OUT<-sqrt(sum((PRED_4_OUT-Testing$CO2)^2)/length(PRED_4_OUT)) #computes out-of-sample

RMSE_4_IN #IN-SAMPLE ERROR
RMSE_4_OUT #OUT-OF-SAMPLE ERROR

#####Testing For Normality#####
hist(M1$residuals)
hist(M2$residuals)
hist(M3$residuals)
hist(M4$residuals)

jarque.bera.test(M1$residuals)
jarque.bera.test(M2$residuals) 
jarque.bera.test(M3$residuals)
jarque.bera.test(M4$residuals)

######Comparison of in-sample model performance by RMSE#####
RMSE_1_IN #MODEL WITH ONLY LINEAR TERM
RMSE_2_IN #MODEL WITH LINEAR AND QUADRATIC TERM
RMSE_3_IN #LOGARITHMIC MODEL
RMSE_4_IN #MULTIPLE REGRESSION MODEL

#####Comparison of out-of-sample model performance by RMSE#####
RMSE_1_OUT #MODEL WITH ONLY LINEAR TERM
RMSE_2_OUT #MODEL WITH LINEAR AND QUADRATIC TERM
RMSE_3_OUT #LOGARITHMIC MODEL
RMSE_4_OUT #MULTIPLE REGRESSION MODEL

#####PLOTTING THE REGRESSION MODELS AGAINST ONE ANOTHER#####
x_grid <- seq(0,7000,.1) #CREATES GRID OF X-AXIS VALUES
plot(Training$CO2 ~ Training$Elec, col='blue')
predictions_1 <- predict(M1, list(Elec=x_grid))
predictions_2 <- predict(M2, list(Elec=x_grid, Elec2=x_grid^2))
predictions_3 <- predict(M3, list(ln_Elec=log(x_grid))) 
lines(x_grid, predictions_1, col='darkgreen', lwd=3) #PLOTS M1
lines(x_grid, predictions_2, col='green', lwd=3) #PLOTS M2
lines(x_grid, predictions_3, col='orange', lwd=3) #PLOTS M3
points(Testing$CO2 ~ Testing$Elec, col='red', pch=3) 
