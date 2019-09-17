hc <-function (orgData, linkage)
{
  if(linkage == "centroid")
  {
    DistMatrix <- as.matrix(dist(orgData)^2)
  }
  else
  {
    DistMatrix <- as.matrix(dist(orgData))
      
  }
  
  # for avoid zero when calculating min value.
  DistMatrix[DistMatrix==0] <- NA

  #For using apply function, change linkage
  method <- changeMethod(linkage)

  #In order to find pairs of a tree, mGroup is made a negative matrix from 1 to nrow(DistMatrix)
  # and then mGroup include a positive number after combine two attributes that are the smallest distance if the group used previously
  mGroup <- -(1:nrow(DistMatrix))

  # put merged data in variable mergedata
  mergeData <- matrix(0, nrow = nrow(DistMatrix) -1, ncol = 2)
  
  mergerows <- matrix(0,nrow = nrow(DistMatrix)-1,ncol = 2)

  # minvalue means height in hc
  minValue <- rep(0,nrow(DistMatrix)-1)

   for (i in seq(1,nrow(DistMatrix)-1))
   {
     # In order to find index minimum value in DistMatrix, it used which function.
      idx <- which(DistMatrix == min(DistMatrix,na.rm = TRUE),arr.ind = TRUE)
     
      minValue[i] <- min(DistMatrix,na.rm = TRUE)
      #browser()
      
        mPair <- mGroup[idx[1,]]
        mergeData[i,] <- mPair[order(mPair)]
        #matrix mergerows shows where merges between attributes
        mergerows[i,] <- idx[1,]
        
        #In order to merge data from bottom to up, it bounds current pair and all previous groups they belong to
        # initially, mGroup compose of negative numbers but if column changed positive, it means that it has been clustered
        gIdx <- c(idx, which(mGroup %in% mGroup[idx[1,mGroup[idx[1,]]>0]]))
        grouping <- c()
        
        if (length(gIdx) >0)
        {
          
          grouping = c(idx[1,],gIdx)
          
        }
        else
        {
          
          grouping = c(idx[1,])
          
        }
        
        # mPair <- order(mPair)
        # mergeData[i,] <- mPair
        
        
        
        
        mGroup[grouping] <- i
        
        
        # To compute distance by each linkage, it used function 'apply'
        rDist <- apply(DistMatrix[idx[1,],],2,method)
      
      
      
    
       
      DistMatrix[min(idx),] <- rDist
      DistMatrix[,min(idx)] <- rDist

   
      # To move next the smallest distance, megered data is excluded so data is NA
      DistMatrix[min(idx),min(idx)] <- NA
      DistMatrix[max(idx),] <- NA
      DistMatrix[,max(idx)] <- NA


   }

  #for making similar as function hclust in r, mergeData is converted vector
  vc <- c()
  vc <- as.vector(abs(mergeData))
  vc <- unique(vc)
  
  # By using structure, this function returns merge data, height, and order
  structure (list(merge = mergeData, height = minValue, order = vc,labels = rownames(orgData),
              method = method),class="hclust")


}

changeMethod <- function (method)
{
  # set initial linkage
  Method <- mean

  if(method == "single")
  {
    Method <- min
  }
  else if(method == "complete")
  {
    Method <- max

  }
  else if(method == "average")
  {
    Method <- mean

  }
  else if(method == "centroid")
  {
    Method <- median
    
  }

  return (Method)
}

#Question 1
set.seed(2)
x <- matrix(rnorm(50*2),ncol=2)
x[1:25,1] <- x[1:25,1]+3
x[1:25,2] <- x[1:25,2]-4

x1=hc(x,"single")
xh1 <- as.dendrogram(x1)
plot(xh1)



#load data
nciData <- read.table('nci.data.txt')
nciData <- t(nciData)
yLabel <- read.table('label.txt')


# single
h1=hc(nciData,"single")
hh1 <- as.dendrogram(h1)
#plot(hh1)

# complete
h2=hc(nciData,"complete")
hh2 <- as.dendrogram(h2)
#plot(hh2)

# average
h3=hc(nciData,"average")
hh3 <- as.dendrogram(h3)
#plot(hh3)

# centroid
h4=hc(nciData,"centroid")
hh4 <- as.dendrogram(h4)
#plot(hh4)


plot(hh1)
plot(hh2)
plot(hh3)
plot(hh4)

#Kmean

set.seed(0515)
scaleData <- scale(nciData)
nciKmean1<- kmeans(scaleData, 3, nstart = 20)

nciKmean2<- kmeans(scaleData, 5, nstart = 20)

nciKmean3<- kmeans(scaleData, 10, nstart = 20)


par(mfrow = c(3,1))
plot(nciData, col=(nciKmean1$cluster+1), main="K-Means Clustering Results with K=3", xlab="", ylab="", pch=20, cex=2)
plot(nciData, col=(nciKmean2$cluster+1), main="K-Means Clustering Results with K=5", xlab="", ylab="", pch=20, cex=2)
plot(nciData, col=(nciKmean3$cluster+1), main="K-Means Clustering Results with K=10", xlab="", ylab="", pch=20, cex=2)

par(mfrow = c(1,1))

#Optional 
nc <- NbClust(x, min.nc = 2, max.nc = 15, method = "kmeans")
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")
