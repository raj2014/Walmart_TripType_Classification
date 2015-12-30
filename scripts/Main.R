# ########################################################################
# Author : Rajasekhar Jetty
# https://www.kaggle.com/c/walmart-recruiting-trip-type-classification/
# ########################################################################

#MainScript
#produces the train and test datasets
#Refer to new_fDefinitions.R for the function definitions
source('scripts/feature_engineering.R')

#generates the features on Department and fineline through Kmeans(Unsupervised Learning from trainData)
#Clustering Departments and fineline items based on their class distributions in the train data.
source('scripts/ClusterFeatures.R')

#Loads the dataset and produces the prediction for test data
source('scripts/xgboost_single.R')