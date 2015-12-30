# ########################################################################
# Author : Rajasekhar Jetty
# https://www.kaggle.com/c/walmart-recruiting-trip-type-classification/
# ########################################################################
library(Matrix)
library(data.table)
library(plyr)
library(entropy)
library(stringr)
source('Scripts/new_fDefinitions.R')



#Loading the train and test data
train<-fread("Data/train.csv",colClasses = c(Upc='character'))
test<-fread("Data/test.csv",colClasses = c(Upc='character'))



#Aligning the target label
#Preparing the target variable
y=as.numeric(train$TripType)
#How to fill y properly : Xgboost requirement ,label must be in [0, num_class)
sortedTripType<-sort(unique(y))
target<-c(1:length(y))
for (i in 1:length(y))
{
  target[i]<-which(sortedTripType==y[i]) 
}
#target<-target-1 #label must be in [0, num_class)
train$TripType<-target


#Initialize "NA" in train $ FineLine to 10500
na_index<-which(is.na(train$FinelineNumber))
train$FinelineNumber[na_index]<-10000

na_index<-which(is.na(test$FinelineNumber))
test$FinelineNumber[na_index]<-10000



#Initialize NA's in Upc
na_upc_index<-which(train$Upc=="")
train$Upc[na_upc_index]<-"0000"


na_upc_index<-which(test$Upc=="")
test$Upc[na_upc_index]<-"0000"

#remove "HEALTH AND BEAUTY AIDS" as it is not present in the test
train<-train[train$DepartmentDescription!="HEALTH AND BEAUTY AIDS",]

#for feature engineering
deptlist<-sort(unique(train$DepartmentDescription))

#forming the department neighbourlist
#source('Scripts/deptCorrelation.R')
#print("Completed the smoothing part")


getClassdistribution<-function(x)
{
  tracker<-c(rep(0,38))
  temp_df<-count(x, vars ="TripType")
  for (i in 1:nrow(temp_df))
  {
    tracker[as.integer(temp_df[i,1])]<-temp_df[i,"freq"]
  }
  tracker
}


fineLinedata<-ddply(train,c('FinelineNumber'),
                    function(x) c(totalCount=nrow(x),
                                  classdist=getClassdistribution(x),
                                  uniqueTriptype=length(unique(x$TripType))
                                  
                    )
)  

fineLinedata$prior<-fineLinedata$totalCount/sum(fineLinedata$totalCount)

#Forming the clusters going for 100 clusters
Obj<-kmeans(fineLinedata[,c(3:40)], centers=180, iter.max = 1000, nstart = 100,
       algorithm = c("Lloyd"), trace=FALSE)
fineCluster<-data.frame(fineLinedata$FinelineNumber,Obj$cluster)
fineLinelist<-unique(fineCluster$fineLinedata.FinelineNumber)


testCluster<-ddply(test,c('VisitNumber'),
                    function(x) c(cluster=generatefineCluster(x)
                                  
                    ),.progress="text"
) 
trainCluster<-ddply(train,c('VisitNumber'),
                   function(x) c(cluster=generatefineCluster(x)
                                 
                   ),.progress = "text"
) 


write.csv(file="featureVectors/trainCluster.csv",trainCluster,row.names=FALSE)
write.csv(file="featureVectors/testCluster.csv",testCluster,row.names=FALSE)

rm(trainCluster)
rm(testCluster)



#Forming the cluster for departments
departmentdata<-ddply(train,c('DepartmentDescription'),
                    function(x) c(totalCount=nrow(x),
                                  classdist=getClassdistribution(x),
                                  uniqueTriptype=length(unique(x$TripType))
                                  
                    ),.progress = "text"
)
departmentdata$prior<-departmentdata$totalCount/sum(departmentdata$totalCount)

#performing kmeans
deptObj<-kmeans(departmentdata[,c(3:40)], centers=7, iter.max = 1000, nstart = 100,
            algorithm = c("Lloyd"), trace=FALSE)
deptCluster<-data.frame(departmentdata$DepartmentDescription,deptObj$cluster)
names(deptCluster)<-c("deptID","clusterID")

deptestCluster<-ddply(test,c('VisitNumber'),
                   function(x) c(cluster=generatedeptCluster(x)
                                 
                   ),.progress="text"
) 
deptrainCluster<-ddply(train,c('VisitNumber'),
                    function(x) c(cluster=generatedeptCluster(x)
                                  
                    ),.progress = "text"
)

write.csv(file="featureVectors/deptrainCluster.csv",deptrainCluster,row.names=FALSE)
write.csv(file="featureVectors/deptestCluster.csv",deptestCluster,row.names=FALSE)

rm(deptrainCluster)
rm(deptestCluster)