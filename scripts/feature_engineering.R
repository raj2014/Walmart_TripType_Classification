# ########################################################################
# Author : Rajasekhar Jetty
# https://www.kaggle.com/c/walmart-recruiting-trip-type-classification/
# ########################################################################

library(xgboost)
library(h2o)
library(Matrix)
library(data.table)
library(plyr)
library(entropy)
library(stringr)

source('scripts/new_fDefinitions.R')

#Loading the train and test data
train<-fread("Data/train.csv",colClasses = c(Upc='character'))
test<-fread("Data/test.csv",colClasses = c(Upc='character'))



#Initialize "NA" in train $ FineLine to 10500
na_index<-which(is.na(train$FinelineNumber))
train$FinelineNumber[na_index]<-10100

na_index<-which(is.na(test$FinelineNumber))
test$FinelineNumber[na_index]<-10100



#Initialize NA's in Upc
na_upc_index<-which(train$Upc=="")
train$Upc[na_upc_index]<-"0000"


na_upc_index<-which(test$Upc=="")
test$Upc[na_upc_index]<-"0000"

#remove "HEALTH AND BEAUTY AIDS" as it is not present in the test
train<-train[train$DepartmentDescription!="HEALTH AND BEAUTY AIDS",]

#for feature engineering
deptlist<-sort(unique(train$DepartmentDescription))

###############reading in splitwise manner############################################
uniqueVisitNumbers<-sort(unique(train$VisitNumber))
splits<-split(uniqueVisitNumbers,ceiling(1:length(uniqueVisitNumbers)/10000))
str(splits)


for (i in 1:length(splits))
{
  
  checkdata<-ddply(train[which(train$VisitNumber %in% splits[[i]]),],c('TripType','VisitNumber'),
                   function(x) c(dayType=x$Weekday[1],
                                 scanLength=length(x$ScanCount),
                                 maxScan=max(x$ScanCount),
                                 minScan=min(x$ScanCount),
                                 scanSum=sum(abs(x$ScanCount)),
                                 scanMean=mean(abs(x$ScanCount)),
                                 scanSD=getSD(x$ScanCount),
                                 scanSD_abs=getSD(abs(x$ScanCount)),
                                 #scan features
                                 scan_positive_count=getPositive_count(x),
                                 scan_negative_count=getNegative_count(x),
                                 scan_sum_positives=getSumOfPositives(x),
                                 scan_sum_negatives=getSumOfNegatives(x),
                                 scan_positive_ratio=getPositiveRatio(x),
                                 scan_negative_ratio=getNegativeRatio(x),
                                 #department features
                                 unique_dept_count=getUniqueDepartmentCounts(x),
                                 #department distribution
                                 dept_=insertdeptframe(x), 
                                 entropy_dept=getEntropyDepartment(x),
                                 #FineLine features
                                 uniqe_FineLine_Count=getUniqueFineLineCount(x),
                                 entropy_FineLine=getEntropyFineLine(x),
                                 #1000 bins for FineLineNumber 
                                 bins=generateBins_Fineline(x),
                                 upc_bins=generateBins_Upc(x),
                                 four_length=generatefourLengthCounts(x),
                                 firstChar_counts<-generateCounts_Upc_First(x)
                                 
                   )
  )  
  
  write.csv(file=paste("temp/checkdata_",i,".csv"),checkdata,row.names = FALSE)
  print(paste(i*100/length(splits)," % done"))
}

rm(checkdata)

##Loading it back
for (i in (1:length(splits)))
   {
     temp<-read.csv(paste("temp/checkdata_",i,".csv"))
       if (i==1)
         {
            checkdata<-temp
             
          }
       else
         {
             print (i)
             colnames(temp)<-colnames(checkdata)
            checkdata<-rbind(checkdata,temp)
           }
      
   }

#Completion of loading the training data

#################################################################################################
checkdata$dayType<-as.factor(checkdata$dayType)
numVector<-c(4:1670)
for (i in numVector)
{
  checkdata[,i]<-as.numeric(checkdata[,i])
}



#Preparing the target variable
y=checkdata$TripType
#How to fill y properly : Xgboost requirement ,label must be in [0, num_class)
sortedTripType<-sort(unique(y))
target<-c(1:length(y))
for (i in 1:length(y))
{
  target[i]<-which(sortedTripType==y[i]) 
}
target<-target-1 #label must be in [0, num_class)




train_matrix<-model.matrix(~.-1,data=checkdata)
write.csv(file="featureVectors/train_matrix.csv",train_matrix,row.names=FALSE)

rm(checkdata)
rm(train_matrix)

################################Forming Test Data#####################################

uniqueVisitNumbers<-sort(unique(test$VisitNumber))
splits<-split(uniqueVisitNumbers,ceiling(1:length(uniqueVisitNumbers)/10000))
str(splits)


for (i in 1:length(splits))
{
  
  testdata<-ddply(test[which(test$VisitNumber %in% splits[[i]]),],c('VisitNumber'),
                   function(x) c(dayType=x$Weekday[1],
                                 scanLength=length(x$ScanCount),
                                 maxScan=max(x$ScanCount),
                                 minScan=min(x$ScanCount),
                                 scanSum=sum(abs(x$ScanCount)),
                                 scanMean=mean(abs(x$ScanCount)),
                                 scanSD=getSD(x$ScanCount),
                                 scanSD_abs=getSD(abs(x$ScanCount)),
                                 #scan features
                                 scan_positive_count=getPositive_count(x),
                                 scan_negative_count=getNegative_count(x),
                                 scan_sum_positives=getSumOfPositives(x),
                                 scan_sum_negatives=getSumOfNegatives(x),
                                 scan_positive_ratio=getPositiveRatio(x),
                                 scan_negative_ratio=getNegativeRatio(x),
                                 #department features
                                 unique_dept_count=getUniqueDepartmentCounts(x),
                                 #department distribution
                                 dept_=insertdeptframe(x), 
                                 entropy_dept=getEntropyDepartment(x),
                                 #FineLine features
                                 uniqe_FineLine_Count=getUniqueFineLineCount(x),
                                 entropy_FineLine=getEntropyFineLine(x),
                                 #1000 bins for FineLineNumber 
                                 bins=generateBins_Fineline(x),
                                 upc_bins=generateBins_Upc(x),
                                 four_length=generatefourLengthCounts(x),
                                 firstChar_counts<-generateCounts_Upc_First(x)
                                 
                   )
  )  
  
  write.csv(file=paste("temp/testdata_",i,".csv"),testdata,row.names = FALSE)
  print(paste(i*100/length(splits)," % done"))
}

#rm(testdata)

##Loading it back
for (i in (1:length(splits)))
{
  temp<-read.csv(paste("temp/testdata_",i,".csv"))
  if (i==1)
  {
    testdata<-temp
    
  }
  else
  {
    print (i)
    colnames(temp)<-colnames(testdata)
    testdata<-rbind(testdata,temp)
  }
  
}

#Completed with formation of the test set
###########################################################################################

testdata$dayType<-as.factor(testdata$dayType)
numVector<-c(3:1669)
for (i in numVector)
{
  testdata[,i]<-as.numeric(testdata[,i])
}

test_matrix<-model.matrix(~.-1,data=testdata)

## writing the train and test features out to the master data folder
#write.csv(file="featureVectors/train_matrix.csv",train_matrix,row.names=FALSE)
write.csv(file="featureVectors/test_matrix.csv",test_matrix,row.names=FALSE)

rm(testdata)
rm(test_matrix)





