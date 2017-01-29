library(caret)
library(AppliedPredictiveModeling)
library(dplyr)
library(pls)
library(rpart)
library("VIM")
library(plyr)
library(randomForest)

# Data Capture
training <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"))
testing <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"))

# Data Cleaning
# Let's have a look at the data by creating a plot of missing data
aggr(training)

# We see that many of the variables are empty or NA. Let's identify these variables and remove them from the training set.
na_count <-sapply(training, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

# Using the na_count I decided to remove the variables listed below.
# I've also decided to remove X and user name variables.

names = c('avg_yaw_belt','max_roll_belt','max_picth_belt','min_roll_belt','min_pitch_belt','amplitude_roll_belt','amplitude_pitch_belt','var_total_accel_belt',
         'avg_roll_belt','stddev_roll_belt','var_roll_belt','avg_pitch_belt','stddev_pitch_belt','var_pitch_belt','stddev_yaw_belt','var_yaw_belt',
         'var_accel_arm','avg_roll_arm','stddev_roll_arm','var_roll_arm','avg_pitch_arm','stddev_pitch_arm','var_pitch_arm','avg_yaw_arm','stddev_yaw_arm','var_yaw_arm',
         'max_roll_arm','max_picth_arm','max_yaw_arm','min_roll_arm','min_pitch_arm','min_yaw_arm','amplitude_roll_arm','amplitude_pitch_arm','amplitude_yaw_arm',
         'max_roll_dumbbell','max_picth_dumbbell','min_roll_dumbbell','min_pitch_dumbbell','amplitude_roll_dumbbell','amplitude_pitch_dumbbell','var_accel_dumbbell',
         'avg_roll_dumbbell','stddev_roll_dumbbell','var_roll_dumbbell','avg_pitch_dumbbell','stddev_pitch_dumbbell','var_pitch_dumbbell','avg_yaw_dumbbell','stddev_yaw_dumbbell',
         'var_yaw_dumbbell','max_roll_forearm','max_picth_forearm','min_roll_forearm','min_pitch_forearm','amplitude_roll_forearm','amplitude_pitch_forearm','var_accel_forearm',
         'avg_roll_forearm','stddev_roll_forearm','var_roll_forearm','avg_pitch_forearm','stddev_pitch_forearm', 'var_pitch_forearm','avg_yaw_forearm','stddev_yaw_forearm',
         'var_yaw_forearm','skewness_pitch_forearm','skewness_yaw_forearm','max_yaw_forearm','min_yaw_forearm','amplitude_yaw_forearm','kurtosis_roll_forearm',
         'kurtosis_picth_forearm','kurtosis_yaw_forearm','skewness_roll_forearm','skewness_yaw_dumbbell','max_yaw_dumbbell','min_yaw_dumbbell','amplitude_yaw_dumbbell',
         'kurtosis_picth_dumbbell','kurtosis_yaw_dumbbell','skewness_roll_dumbbell','skewness_pitch_dumbbell','kurtosis_roll_dumbbell','kurtosis_roll_arm','kurtosis_picth_arm',
         'kurtosis_yaw_arm','skewness_pitch_arm','amplitude_yaw_belt','kurtosis_yaw_belt','skewness_roll_belt','skewness_roll_belt.1','skewness_yaw_belt',
         'max_yaw_belt','min_yaw_belt','kurtosis_roll_belt','kurtosis_picth_belt','X','user_name')

training <- training %>% select(-one_of(names))

#
count(training, 'classe')

aggr(training)

testing <- testing %>% select(-one_of(names))

# Apply Model
# We will fit a classification tree using the Recursive Partitioning and Regression Trees model 

model <- train(classe ~., method="rpart", data = training)
prediction <- predict(model,newdata = testing)
prediction

#Submitted the results listed above and earned 9/20 points (45%)

install.packages('randomForest')
library(randomForest)

# explore and remove variables with more than 53 factors because random forest can not handle categorical predictors with more than 53 categories.
training <- training %>% select(-one_of(c('skewness_roll_arm','skewness_yaw_arm')))
testing <- testing %>% select(-one_of(c('skewness_roll_arm','skewness_yaw_arm','problem_id')))

rf_model <- randomForest(as.factor(classe) ~.,data = training,importance=TRUE,ntree=2000)

# I tried to predic classification but I was receiving an error that the type of predictors in new data do not match that of the training data.
# This is because some of the variables had different types and because we have different levels of factors for the categorical variables.
# Here below I make necessary transformations 
testing$magnet_dumbbell_z<-as.numeric(testing$magnet_dumbbell_z)
testing$magnet_forearm_y<-as.numeric(testing$magnet_forearm_y)
testing$magnet_forearm_z<-as.numeric(testing$magnet_forearm_z)
levels(testing$cvtd_timestamp) <- levels(training$cvtd_timestamp)
levels(testing$new_window) <- levels(training$new_window)

# Predict on training data
rf_prediction <- predict(rf_model,newdata = testing)
rf_prediction

#Submitted the results listed above and earned 20/20 points!