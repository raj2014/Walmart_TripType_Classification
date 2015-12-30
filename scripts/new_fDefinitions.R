# ########################################################################
# Author : Rajasekhar Jetty
# https://www.kaggle.com/c/walmart-recruiting-trip-type-classification/
# ########################################################################
#3
getPositive_count<-function(x)
{
  length(x$ScanCount[x$ScanCount>0])
}
#4
getNegative_count<-function(x)
{
  length(x$ScanCount[x$ScanCount<0])
}
#5
getSumOfPositives<-function(x)
{
  sum(x$ScanCount[x$ScanCount>0])
}
#6
getSumOfNegatives<-function(x)
{
  abs(sum(x$ScanCount[x$ScanCount<0]))
}
#7
getPositiveRatio<-function (x)
{
  a=sum(x$ScanCount[x$ScanCount>0])
  b=abs(sum(x$ScanCount[x$ScanCount<0]))
  cumSum=a+b
  r=a/cumSum
  return (r)
}
#8
getNegativeRatio<-function (x)
{
  a=sum(x$ScanCount[x$ScanCount>0])
  b=abs(sum(x$ScanCount[x$ScanCount<0]))
  cumSum=a+b
  r=b/cumSum
  return (r)
}

#get number of unique departments involved
getUniqueDepartmentCounts<-function (x)
{
  length(unique(x$DepartmentDescription))
}

#Distributions for departments

insertdeptframe<-function(x)
{
  k<-ddply(x,c('DepartmentDescription'),function (d) c(counts=sum(abs(d$ScanCount))))
  ##include raw counts also as features
  raw_counts<-c(rep(0,68))
  names(raw_counts)<-c(deptlist)
  for (i in 1:nrow(k))
  {
    raw_counts[as.character(k$DepartmentDescription[i])]<-k$counts[i]
  }
  names(raw_counts)<-c(paste("raw_",c(1:68)))
  k$counts<-k$counts/sum(k$counts)
  v<-c(rep(0,71))
  names(v)<-c(deptlist,"dif_peaks","sum_peaks","sd_dist")
  for (i in 1:nrow(k))
  {
    v[as.character(k$DepartmentDescription[i])]<-k$counts[i]
  }
  k<-sort(v,decreasing=TRUE)
  v["dif_peaks"]<-k[1]-k[2]
  v["sum_peaks"]<-k[1]+k[2]
  v["sd_dist"]<-sd(v)
  v<-c(v,raw_counts)
  v
}

getUniqueDepartmentCounts<-function (x)
{
  length(unique(x$DepartmentDescription))
}

getEntropyDepartment<-function(x)
{
  k<-as.data.frame(table(x$DepartmentDescription))
  colnames(k)<-c("DepartmentDescription","Freq")
  entropy(k$Freq)
}


#getting the unique fineline counts
getUniqueFineLineCount<-function(x)
{
  length(unique(x$FinelineNumber))
}

#get the entropy of fineline counts
getEntropyFineLine<-function(x)
{
  entropy(as.data.frame(table(x$FinelineNumber))$Freq)
}

# return standard deviation of the vector 
getSD<-function(x)
{
  if (length(x)<=1)
  {
    return (0)
  }
  
  else
  {
    return (sd(x))
  }
}

#generating bins for fineline :(relative /absolute )
generateBins_Fineline<-function(x)
{
  v<-discretize(x$FinelineNumber,numBins=1000,r=range(0,10200))
  names(v)<-paste("bins",c(1:1000),sep="_")
  #s<-sd(v)
  #v<-c(v,s)
  v<-v/length(x$FinelineNumber)
  v
}

generateFineLine<-function(x)
{
  v<-discretize(x$FinelineNumber,numBins=2000,r=range(0,10000))
  names(v)<-paste("bins",c(1:2000),sep="_")
  #s<-sd(v)
  #v<-c(v,s)
  v<-v/length(x$FinelineNumber)
}



generateBins_Upc<-function(x)
{
  upcode<-x$Upc[which(str_length(x$Upc)>=4)]
  upc_temp<-strtoi(str_sub(upcode,start=1,end=4), base = 0L)
  #estimation 50 per bin
  v<-discretize(upc_temp,numBins=500,r=range(0,10000)) 
  names(v)<-paste("upc_bins",c(1:500),sep="_")
  v<-v/length(upc_temp)
  v
}

#to return the number of 4 digit upc codes
generatefourLengthCounts<-function(x)
{
  indexes_length<-length(which(str_length(x$Upc)<=4))
  indexes_length
}

#other features generation techniques

#generate bins for first letter
generateCounts_Upc_First<-function(x)
{
  
  t<-as.data.frame(table(str_sub(x$Upc,start=1,end=1)))
  colnames(t)<-c("startLetter","count")
  t$startLetter<-strtoi(t$startLetter, base = 0L)
  #normalizing table
  t$count<-t$count/sum(t$count)
  v<-c(rep(0,10))
  names(v)<-paste("start_",c(0:9),sep="_")
  for (i in 1:nrow(t))
  {
    v[t$startLetter[i]+1]<-t$count[i]
  }
  v
}


generatefolds<-function(target,sampleRate,setSeed)
{ set.seed(setSeed)
  trainlist<-list()
  cvlist<-list()
  for(i in unique(target))
  {
    temp<-which(target==i)
    
    train_fold<-sample(temp,sampleRate*length(temp),replace=FALSE)
    cv_fold<-temp[which(!temp %in% train_fold)]
    trainlist[[i+1]]<-train_fold
    cvlist[[i+1]]<-cv_fold
    #print(trainlist)
  }
  
  trainIndex<-unlist(trainlist)
  cvIndex<-unlist(cvlist)
  list(trainIndex,cvIndex)
}


#generate the cluster features for the finelineNumbers

generatefineCluster<-function(x)
{
  prob<-c(rep(0,181))
  uniquefineLine<-unique(x$FinelineNumber)
  for (i in uniquefineLine)
  {
    if (i %in% fineLinelist)
    {
      id<-which(fineCluster$fineLinedata.FinelineNumber==i)
      clusterValue<-fineCluster$Obj.cluster[id]
      prob[clusterValue]<-prob[clusterValue]+1
      
    }
    else
    {
      prob[151]<-prob[151]+1#do nothing
    }
  }  
  prob
}

#generate the cluster features for the department

generatedeptCluster<-function(x)
{
  prob<-c(rep(0,7))
  uniquefineLine<-unique(x$DepartmentDescription)
  for (i in uniquefineLine)
  {
    if (i %in% deptlist)
    {
      id<-which(departmentdata$DepartmentDescription==i)
      clusterValue<-deptCluster$clusterID[id]
      prob[clusterValue]<-prob[clusterValue]+1
      
    }
    else
    {
      #do nothing
    }
  }  
  prob
}

