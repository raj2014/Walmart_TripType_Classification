# Walmart_TripType_Classification
Kaggle DataScience Challenge:

Walmart TripType Classification

https://www.kaggle.com/c/walmart-recruiting-trip-type-classification/

Source Code in R

A single Xgboost model with 1.8k features.

Feature Engineering consisted of basic statistical features(sd,mean,entropy),bins(UPC,FineLine) and 
semisupervised features generated from KMeans on FineLine and UPC fields

Multiclass Logloss Error

Private LB : 0.619

Public LB  : 0.620

Private LB rank : 110

Public LB rank  : 103
