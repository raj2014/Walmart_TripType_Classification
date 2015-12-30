# ########################################################################
# Author : Rajasekhar Jetty
# https://www.kaggle.com/c/walmart-recruiting-trip-type-classification/
# ########################################################################
#Single Xgboost Checks
library(xgboost)
library(data.table)


#Read train and testset from feature vectors
train_matrix<-read.csv(file="featureVectors/train_matrix.csv",header=TRUE)
trainCluster<-read.csv(file="featureVectors/trainCluster.csv",header=TRUE)
deptrainCluster<-read.csv(file="featureVectors/deptrainCluster.csv",header=TRUE)


#joining the cluster features from department and fineline fields
train_matrix<-merge(train_matrix,trainCluster,by="VisitNumber")
train_matrix<-merge(train_matrix,deptrainCluster,by="VisitNumber")


y=train_matrix$TripType
#How to fill y properly : Xgboost requirement ,label must be in [0, num_class)
sortedTripType<-sort(unique(y))
target<-c(1:length(y))
for (i in 1:length(y))
{
  target[i]<-which(sortedTripType==y[i]) 
}
target<-target-1 #label must be in [0, num_class)

param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = 38,
              "eta"=0.05,
              "max.depth"=8,
              "nthread" = -1,
              "min_child_weight"=1,
              "colsample_bytree"=0.9,
              "subsample"=0.9
)

bst = xgboost(params=param, data = as.matrix(train_matrix[,-c(1,2)]), label = target, nrounds=440)

# xgb.cv(params=param,nrounds=150,data=as.matrix(train_matrix[,-c(1,2)]),nfold=5,label=target,
# metrics={'mlogloss'},
# verbose=TRUE,showsd=FALSE,
# maximize=TRUE)


rm(train_matrix)
rm(trainCluster)
rm(deptrainCluster)

#Loading the test set
#Read train and testset from feature vectors

test_matrix<-read.csv(file="featureVectors/test_matrix.csv",header=TRUE)
testCluster<-read.csv(file="featureVectors/testCluster.csv",header=TRUE)
deptestCluster<-read.csv(file="featureVectors/deptestCluster.csv",header=TRUE)

test_matrix<-merge(test_matrix,testCluster,by="VisitNumber")
test_matrix<-merge(test_matrix,deptestCluster,by="VisitNumber")


pred_basic = predict(bst,as.matrix(test_matrix[,-1]))
pred = matrix(pred_basic,38,length(pred_basic)/38)
pred=t(pred)
pred = data.frame(test_matrix[,1],pred)
names_pred<-paste('TripType_',sortedTripType,sep="")
names_pred<-append('VisitNumber',names_pred)
colnames(pred)<-names_pred

pred$VisitNumber<-as.integer(pred$VisitNumber)
write.csv(file="xgboost_single_c.csv",pred,row.names=FALSE)



