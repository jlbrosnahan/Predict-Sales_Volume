# The purpose of this project is to predict sales of four different product types and assess the impact service reviews and customer reviews have on sales.
# Target variable: 'Volume' for the product types: PC, Laptops, Netbooks, and Smartphones

# loading packages
library(tidyverse)
library(caret)
library(ggplot2)
library(corrplot)
library(openxlsx)
library(h2o)

# Importing data
existing <- read.csv(file.path('C:/Users/jlbro/OneDrive/C3T3', 'existing.csv'), 
                     stringsAsFactors = TRUE)

# Checking structure
str(existing)
# Everything is numeric or integer except ProductType

# Because regression algorithms can easily misinterpret categorical variables in which there are 
# more than 2 values, we will dummify categorical data for regression modeling to binarize the values.
# Then turn into a dataframe
existingDummy <- dummyVars(' ~ .', data = existing)
existing2 <- data.frame(predict(existingDummy, newdata = existing))

# Check structure again
str(existing2)

# Check summary for descriptive and NAs
summary(existing2)

# Reveals 15 NA's for 'BestSellersRank' 

# Deleting BestSellersRank, only variable with NAs
existing2$BestSellersRank <- NULL

# Correlation matrix of all variables
corrData <- cor(existing2)

# Exporting correlation to excel
write.xlsx(corrData, file = "corrData.xlsx", row.names=TRUE)
write.xlsx(existing2, file = 'existing2.xlsx')

# Viewing correlation heatmap, as you can see, it's unreadable with so many variables
corrplot(corrData)

# Removing 5 Star since perfect correlation of 1 to target variable, risks overfitting
# Also removing low correlated variables
existing3 <- subset(existing2, select = -c(1:4, 8:9, 11:12, 15, 24:27))

str(existing3)

# Transmute new column that is average of all Star Reviews to try as form of PCA
existing4 <- existing3 %>% 
  rowwise() %>% mutate(AvgStarReviews = (mean(c(x4StarReviews, x3StarReviews, x2StarReviews, x1StarReviews))))


######################################################################################################
### EDA

# viewing correlation heatmap
corrData3 <- cor(existing3)
corrplot(corrData3)

# Enhancing the correlation heatmap
color <- colorRampPalette(c('#BB4444','#EE9988','#FFFFFF','#77AADD','#4477AA'))
corrplot(corrData3, method = 'shade', shade.col = NA, tl.col = 'black', 
         type = 'upper', tl.srt = 45)

# Histogram of Volume, reveals couple outliers
ggplot(data = existing3, mapping = aes(x = Volume)) +
  geom_histogram()

# Plotting Sales Volume by Product Type
ggplot(data = existing, aes(x = ProductType, y = Volume, fill = ProductType)) +
  geom_bar(stat = 'identity') + 
  guides(fill=FALSE) +
  coord_flip()

# Plotting the impact 5 Star Reviews have on Sales Volume
ggplot(data=existing, aes(x=x5StarReviews, y=Volume)) + 
  geom_point(aes(color=ProductType, size=2)) +
  theme_bw() +
  scale_x_continuous(trans = 'log2') + 
  scale_y_continuous(trans = 'log2') +
  geom_line() +
  facet_wrap(~ProductType) + 
  xlab('Number of 5 Star Reviews') +
  ylab('Sales Volume') +
  ggtitle('Effect of 5 Star Reviews on Sales Volume')

# Now plotting impact of 4 Star Reviews on Sales Volume
ggplot(data=existing, aes(x=x4StarReviews, y=Volume)) + 
  geom_point(aes(color=ProductType, size=2)) +
  theme_bw() +
  scale_x_continuous(trans = 'log2') + 
  scale_y_continuous(trans = 'log2') +
  geom_line() +
  facet_wrap(~ProductType) + 
  xlab('Number of 4 Star Reviews') +
  ylab('Sales Volume') +
  ggtitle('Effect of 4 Star Reviews on Sales Volume')

# Now plotting impact of Positive Service Reviews on Sales Volume
ggplot(data=existing, aes(x=PositiveServiceReview, y=Volume)) + 
  geom_point(aes(color=ProductType)) +
  theme_bw() +
  scale_x_continuous(trans = 'log2') + 
  scale_y_continuous(trans = 'log2') +
  geom_line() +
  facet_wrap(~ProductType) + 
  xlab('Number of Positive Service Reviews') +
  ylab('Sales Volume') +
  ggtitle('Positive Service Review Impact on Sales Volume')
  
############################################################################################################
# Modeling 
# Set seed
set.seed(123)

# CreateDataPartition() 75% and 25%
index1 <- createDataPartition(existing3$Volume, p=0.75, list = FALSE)
train1 <- existing3[ index1,]
test1 <- existing3[-index1,]

# Removing 2 outlier rows #18 and #48 from test set
test1_rem_out <- test1[!rownames(test1) %in% c('18', '48'), ]

# Checking structure of train1
str(train1)

# Setting cross validation
control1 <- trainControl(method = 'repeatedcv',
                         number = 10,
                         repeats = 1)

##############################################################################################
### Random forest model and tuning

# set seed
set.seed(123)

# Creating dataframe for manual tuning
rfGrid <- expand.grid(mtry = c(2,3,4,5,6,7,8))

rf1 <- train(Volume ~ x4StarReviews + PositiveServiceReview + x2StarReviews + x3StarReviews + 
               x1StarReviews + NegativeServiceReview + Recommendproduct + ShippingWeight + Price,
             data = train1,
             method = 'rf',
             trControl = control1,
             tuneGrid = rfGrid)

rf1

# Variable importance using ggplot
ggplot(varImp(rf1, scale=FALSE)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  ggtitle('Variable Importance of Random Forest 1 on Sales Volume')

# Predicting rf on test1
rf1Preds <- predict(rf1, newdata = test1_rem_out)

summary(rf1Preds)
plot(rf1Preds)

# postResample to test if it will do well on new data or if overfitting
postResample(rf1Preds, test1_rem_out$Volume)

# CV RMSE=788, R2=.908
# PostResample RMSE=190, R2=.945

##############################################################################################
# set seed
set.seed(123)

# Random Forest using feature selection

rf2 <- train(Volume ~ x4StarReviews + PositiveServiceReview + x2StarReviews,
             data = train1,
             method = 'rf',
             trControl = control1)

rf2

# Variable importance using ggplot
ggplot(varImp(rf2, scale=FALSE)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  ggtitle('Variable Importance of Random Forest 2 on Sales Volume')

# Plotting the residuals against the actual values for Volume
# Graph shows outlier
resid_rf2 <- residuals(rf2)
plot(train1$Volume, resid_rf2, 
     xlab = 'Sales Volume', 
     ylab = 'Residuals', 
     main ='Predicted Sales Volume Residuals Plot',
     abline(0,0))

# Predicting rf2 on test1
rf2Preds <- predict(rf2, newdata = test1_rem_out)

summary(rf2Preds)
plot(rf2Preds)

# postResample to test if it will do well on new data or if overfitting
postResample(rf2Preds, test1_rem_out$Volume)

# CV RMSE = 745, R2=.928
# PostResample RMSE=153, R2=.972
# Top model!

##############################################################################################
# set seed
set.seed(123)

# Random Forest using feature selection

rf3 <- train(Volume ~ x4StarReviews + PositiveServiceReview + x3StarReviews,
             data = train1,
             method = 'rf',
             trControl = control1)

rf3

# Variable importance using ggplot
ggplot(varImp(rf3, scale=FALSE)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  ggtitle('Variable Importance of Random Forest 3 on Sales Volume')

# Predicting rf3 on test1
rf3Preds <- predict(rf3, newdata = test1_rem_out)

summary(rf3Preds)

# postResample to test if it will do well on new data or if overfitting
postResample(rf3Preds, test1_rem_out$Volume)

# CV RMSe=648, R2=.934
# PostResample RMSE=167, R2=.954

##############################################################################################
# set seed
set.seed(123)

# Random Forest using feature selection
rf4 <- train(Volume ~ x4StarReviews + PositiveServiceReview + x3StarReviews + x2StarReviews + 
               x1StarReviews + NegativeServiceReview,
             data = train1,
             method = 'rf',
             trControl = control1)

rf4

# Variable importance using ggplot
ggplot(varImp(rf4, scale=FALSE)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  ggtitle('Variable Importance of Random Forest 4 on Sales Volume')

# Plotting the residuals against the actual values for Volume
# Graph shows outlier
resid_rf4 <- residuals(rf4)
plot(train1$Volume, resid_rf4, xlab = 'Sales Volume', ylab = 'Residuals', 
     main='Predicted Sales Volume Residuals Plot',
     abline(0,0))

# Predicting rf4 on test1
rf4Preds <- predict(rf4, newdata = test1_rem_out)
summary(rf4Preds)

# postResample to test if it will do well on new data or if overfitting
postResample(rf4Preds, test1_rem_out$Volume)

# CV RMSE=783, R2=.909
# RMSE=177, R2=.952

##############################################################################################
# Support Vector Machines -- RBF Kernel

# Set seed
set.seed(123)

# Creating dataframe for manual tuning
rbfGrid <- expand.grid(sigma = c(.01, .015, .2),
                       C = c(10, 100, 1000))

rbf1 <- train(Volume ~ x4StarReviews + x3StarReviews + PositiveServiceReview,
              data = train1,
              method = 'svmRadial',
              trControl = control1,
              tuneGrid = rbfGrid,
              preProc = c('center','scale'))

rbf1

# Predicting rbf on test1
rbf1Preds <- predict(rbf1, newdata = test1_rem_out)
summary(rbf1Preds)

# postResample to test if it will do well on new data or if overfitting
postResample(rbf1Preds, test1_rem_out$Volume)

# CV RMSE=879, R2=.919
# PostResample RMSE=264, R2=.815

##############################################################################################
# Support Vector Machines -- RBF Kernel feature selection

# Set seed
set.seed(123)

# Creating dataframe for manual tuning
rbfGrid <- expand.grid(sigma = c(.01, .015, .2),
                       C = c(10, 100, 1000))

rbf2 <- train(Volume ~ x4StarReviews + PositiveServiceReview + x2StarReviews,
              data = train1,
              method = 'svmRadial',
              trControl = control1,
              tuneGrid = rbfGrid,
              preProc = c('center','scale'))

rbf2

# Predicting rbf on test1
rbf2Preds <- predict(rbf2, newdata = test1_rem_out)

summary(rbf2Preds)

# postResample to test if it will do well on new data or if overfitting
postResample(rbf2Preds, test1_rem_out$Volume)

# CV RMSE=657, R2=.909
# PostResample RMSE=420, R2=.704 
# Negatives

##############################################################################################
# Support Vector Machines -- Linear

# Set seed
set.seed(123)

# Creating dataframe for manual tuning
linearGrid <- expand.grid(C = c(1, 10, 100, 1000))

linear1 <- train(Volume ~ x4StarReviews + x3StarReviews + PositiveServiceReview,
                 data = train1,
                 method = 'svmLinear',
                 trControl = control1,
                 tuneGrid = linearGrid,
                 preProc = c('center','scale'))

linear1

# Predicting rbf on test1
linearPreds <- predict(linear1, newdata = test1_rem_out)

summary(linearPreds)

# postResample to test if it will do well on new data or if overfitting
lin_PR <- postResample(linearPreds, test1_rem_out$Volume)

# CV RMSE=843, R2=.858
# PR RMSE=462, R2=.583
# Negative predictions, move on

##############################################################################################
# SVM -- Linear

# Changing features

# Set seed
set.seed(123)

# Creating dataframe for manual tuning
linearGrid <- expand.grid(C = c(1, 10, 100, 1000))

linear2 <- train(Volume ~ x4StarReviews + x3StarReviews + PositiveServiceReview + 
                   NegativeServiceReview + Price,
                 data = train1,
                 method = 'svmLinear',
                 trControl = control1,
                 tuneGrid = linearGrid,
                 preProc = c('center','scale'))

linear2

# Predicting rbf on test1
linear2Preds <- predict(linear2, newdata = test1_rem_out)
summary(linear2Preds)

# postResample to test if it will do well on new data or if overfitting
postResample(linear2Preds, test1_rem_out$Volume)

# RMSE=1120, R2=56.9
# Negative predictions, move on

##############################################################################################
# Support Vector Machines -- Polynomial

# Set seed
set.seed(123)

# Creating dataframe for manual tuning
polyGrid <- expand.grid(degree = c(2,3,4),
                        scale = c(1,2),
                        C = c(.1, 1, 10, 100))

poly1 <- train(Volume ~ x4StarReviews + x3StarReviews + PositiveServiceReview,
               data = train1,
               method = 'svmPoly',
               trControl = control1,
               tuneGrid = polyGrid,
               preProc = c('center','scale'))

poly1

# Predicting rbf on test1
polyPreds <- predict(poly1, newdata = test1_rem_out)
summary(polyPreds)

# postResample to test if it will do well on new data or if overfitting
postResample(polyPreds, test1_rem_out$Volume)

# RMSE=688, R2=60.2
# Negative predictions, move on

##############################################################################################
# SVM -- Polynomial

# Set seed
set.seed(123)

# Creating dataframe for manual tuning
polyGrid <- expand.grid(degree = c(2,3,4),
                        scale = c(1,2),
                        C = c(.1, 1, 10, 100))

poly2 <- train(Volume ~ x4StarReviews + x2StarReviews + PositiveServiceReview + 
                 NegativeServiceReview,
               data = train1,
               method = 'svmPoly',
               trControl = control1,
               tuneGrid = polyGrid,
               preProc = c('center','scale'))

poly2

# Predicting rbf on test1
poly2Preds <- predict(poly2, newdata = test1_rem_out)
summary(poly2Preds)

# postResample to test if it will do well on new data or if overfitting
postResample(poly2Preds, test1_rem_out$Volume)

# RMSE=402, R2=0.57

##############################################################################################
# Gradient Boosting

# Set seed
set.seed(123)

gbm1 <- train(Volume ~ x4StarReviews + x2StarReviews + PositiveServiceReview,
              data = train1,
              method = 'gbm',
              trControl = control1,
              preProc = c('center','scale'))

gbm1

# Predicting gbm on test1
gbmPreds <- predict(gbm1, newdata = test1_rem_out)
summary(gbmPreds)

# postResample to test if it will do well on new data or if overfitting
postResample(gbmPreds, test1_rem_out$Volume)

## awesome step! provides comparison of predictions to actual within same DF!
compare_gbm1 <- data.frame(test1_rem_out,gbmPreds) 

# CV RMSE=1010, R2=.858
# PostResample RMSE=266, R2=.911 

##############################################################################################
# Gradient Boosting Manual Tuning

# Set seed
set.seed(123)

# Creating dataframe for manual tuning
gbmGrid <- expand.grid(n.trees = c(10,50,100),
                       interaction.depth = c(2,3),
                       shrinkage = c(.1, .15, .2),
                       n.minobsinnode = c(5,10,15))

gbm2 <- train(Volume ~ x4StarReviews + x2StarReviews + PositiveServiceReview,
              data = train1,
              method = 'gbm',
              trControl = control1,
              tuneGrid = gbmGrid,
              preProc = c('center','scale'))

gbm2

# Predicting gbm on test1
gbm2Preds <- predict(gbm2, newdata = test1_rem_out)
summary(gbm2Preds)

# postResample to test if it will do well on new data or if overfitting
postResample(gbm2Preds, test1_rem_out$Volume)

# CV RMSE=813, R2=.962 
# PostResample RMSE=415, R2=.706 

#######################################################################################################

# Bayesian Ridge Regression, L1

# Set seed
set.seed(123)

bay1 <- train(Volume ~ x4StarReviews + PositiveServiceReview + x2StarReviews,
              data = train1,
              method = 'blassoAveraged',
              trControl = control1,
              preProc = c('center','scale'))

bay1

# Predicting gbm on test1
bay1Preds <- predict(bay1, newdata = test1_rem_out)
summary(bay1Preds)

# postResample to test if it will do well on new data or if overfitting
postResample(bay1Preds, test1$Volume)

# Negative predictions regardless of feature selection, high RMSE, doesn't work with this task
# CV RMSE=1148, R2=.753

########################################################################################################
set.seed(123)

# After deleting problem outlier rows in test set - 17 observations
Actual_vs_Predicted_NoOutlier <- data.frame(test1_rem_out %>% select(ProductNum, Volume), 
                                            rf1Preds, rf2Preds, rf3Preds, rf4Preds, rbf1Preds, rbf2Preds, 
                                            linearPreds, linear2Preds, polyPreds, poly2Preds, gbmPreds, gbm2Preds)

# exporting to excel
write.xlsx(Actual_vs_Predicted_NoOutlier, file = "Actual_vs_Predicted_NoOutlier.xlsx", row.names=TRUE)

#####################################################################################################
# Now modeling with our feature transformed column, AvgStarReviews to see if feature transformation helps with predictions
# Set seed
set.seed(123)

# CreateDataPartition() 75% and 25%
index2 <- createDataPartition(existing4$Volume, p=0.75, list = FALSE)
train2 <- existing4[ index2,]
test2 <- existing4[-index2,]

# Checking structure of train1
str(train2)

# Setting cross validation
control1 <- trainControl(method = 'repeatedcv',
                         number = 10,
                         repeats = 1)

# set seed
set.seed(123)

# Creating dataframe for manual tuning
rfGrid <- expand.grid(mtry = c(2,3,4,5,6,7,8))

rf_1 <- train(Volume ~ .,
             data = train2,
             method = 'rf',
             trControl = control1,
             tuneGrid = rfGrid)

rf_1
summary(rf_1)

# Variable importance using ggplot
ggplot(varImp(rf_1, scale=FALSE)) +
  ggtitle('Variable Importance of Random Forest_transformed 1 Model')

# Predicting rf on test2
rf_1Preds <- predict(rf_1, newdata = test2)
view(rf_1Preds)

summary(rf_1Preds)

# postResample to test if it will do well on new data or if overfitting
postResample(rf_1Preds, test2$Volume)

# RMSE=1545 and R2=.217, poor, it does not

##############################################################################################
# set seed
set.seed(123)

# Random Forest using feature selection

rf_2 <- train(Volume ~ AvgStarReviews + PositiveServiceReview + NegativeServiceReview,
             data = train2,
             method = 'rf',
             trControl = control1)

rf_2

# Predicting rf2 on test1
rf_2Preds <- predict(rf_2, newdata = test2)

summary(rf_2Preds)

# postResample to test if it will do well on new data or if overfitting
postResample(rf_2Preds, test2$Volume)

# RMSE=1574 and R2=0.19, poor

##############################################################################################

# Using Top Model rf2 algorithm (PostResample R2=.889, RMSE=203) to make predictions on new product data
# Target variable: 'Volume' for PC, Laptops, Netbooks, and Smartphones product types

# importing data
new <- read.csv(file.path('C:/Users/jlbro/OneDrive/C3T3', 'new.csv'), stringsAsFactors = TRUE)

# checking structure
str(new)

# everything is numeric or integer except ProductType

# Because regression algorithms can easily misinterpret categorical variables in which there are more than 2 values, we will dummify categorical data for regression modeling to binarize the values.
# Then turning into a dataframe
newDummy <- dummyVars(' ~ .', data = new)
new2 <- data.frame(predict(newDummy, newdata = new))

# check structure again
str(new2)

# check summary for descriptive and NAs
summary(new2)

# Removing 'BestSellersRank' since not in modeling dataset
new2$BestSellersRank <- NULL

str(new2)

# Making df identical to 
new3 <- subset(new2, select = -c(1:4, 8:9, 11:12, 15, 24:27))

str(new3)

set.seed(123)

# Predicting rbf1 on 'new3' product data
Predicted_Volume <- predict(rf2, newdata = new3)

## Adding our predictions to the 'new' product dataframe
Preds_rf2_df <- data.frame(new3 %>% select(ProductType.Laptop, ProductType.Netbook, ProductType.PC, ProductType.Smartphone, ProductNum, Volume), Predicted_Volume) 

# exporting to excel
write.xlsx(Preds_rf2_df,"TopModel_rf2_Preds.xlsx")

knitr::spin('C3T3-3script.R')
