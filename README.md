## Predict-Sales_Volume
Forecasting sales volume using 3 different regression algorithms in caret and identifying the top model. Will we obtain one that meets the minimum specifications of 85% post resample R2 and minimal RMSE? 
* Please see Sales-Volume.md file above for complete analysis
* Also available on RPubs at https://rpubs.com/brosnahj/predict-sales-volume

## Objective
I have been asked by an Electronics Company to forecast sales of new products being considered for future inventory. Specifically, they would like me to predict sales volume for four different product types: PCs, Laptops, Netbooks, and Smartphones, which will be used to determine which new products will be brought into store inventory. They also want me to analyze the impact Customer Reviews and Service Reviews have on sales volume.

## Goals
The goals are to 1) assess the impact Customer Reviews and Service Reviews have on sales, and 2) build a model that can predict sales volume on new products with at least 85% level of certainty (R2) and minimal error (RMSE). I will build three different algorithms (Random Forest, Support Vector Machines RBF kernel, and Gradient Boosting) and utilize a variety of automatic and manual tuning mechanisms to optimize models.

## Data Description
Data consists of service reviews, customer reviews, historical sales data, and product descriptions for all products currently in inventory. The target variable for this project is sales 'Volume' for the product types: PC, Laptops, Netbooks, and Smartphones.

## Machine Learning
3 different regression algorithms were utilized in caret
* Random Forest with manual, automatic tuning, and feature selection
* Support Vector Machines RBF kernel with manual tuning and feature selection
* Gradient Boosting with manual, automatic, and feature selection

## Skills
* Logarithm to handle imbalanced distribution analysis
* Regression modeling using caret
* Feature selection
* Manual tune grids
* dplyr

## Actionable Insights
* The more Star Reviews a product has, the higher the sales volume. Consider focusing more on existing product types with more 4 and 5 Star reviews.
* The more Positive Service Reviews, the higher the sales volume for most products. Because of this, consider boosting Customer Service training during employee on-boarding and regularly thereafter as part of a Customer- and Employee-centric Company mission. 
* PC #171, Laptop #173, Netbook #180, and Smartphone #650 are projected to outperform other products within each type quite handily, with other products also worthy of bringing on board. Captialize on predicted sales volume to help determine new products for building store inventory.

## Sales predictions
![Visual 1]()
