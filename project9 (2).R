Academic
#Ghousia Katlagal
# " I have neither given or received, nor have I tolerated other's use of unauthorized aid. "
install.packages("h2o")  
install.packages("cluster")  
install.packages("fpc")  
#The dataset is too large to run with computer system,so we took 3000 rows.
data9<-athlete_events[1:3000,]

#We just consider four attributes as predictor variables like previous project.
#The Medal attribute is our target variable like previous project.
data9$Height<-as.numeric(data9$Height)
data9$Weight<-as.numeric(data9$Weight)
data9$Age<-as.numeric(data9$Age)
data9$Year<-as.numeric(data9$Year)
data
data9$Medal<-as.factor(as.character(data9$Medal))
data9$Medal<-as.numeric(data9$Medal)
data9[is.na(data9)]<-0
# We bulid a dataset that includes five attributes we picked.
#The Medal attribute is our target variable like previous project.
x=data9[,c(4,5,6,10,15)]

#We need to find the optimal number of clusters.
set.seed(200) # Let's normalize the randomization for the classroom
k.max <- 10 # Defining the maximum of the clusters
wss<- sapply(1:k.max,function(k){kmeans(x[,1:4],k,nstart = 20,iter.max = 20)$tot.withinss})
wss
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")
#The plot of Within cluster sum of squares vs the number of clusters show us an elbow point at 2. 
#So, we can conlude that 2 is the best value for k to be used to create the final model.

############################## k Means ################################

## At first, we remove Medal from the data to cluster. After that, we apply function kmeans() to
## ath9, and store the clustering result in kmeans.result. The cluster number is set to 2 in the
## code below.
ath9<-x
ath9$Medal<- NULL
kmeans.result <- kmeans(ath9, 2)

## The clustering result is then compared with the class label (Medal) to check 
## whether similar objects are grouped together.   
table(x$Medal, kmeans.result$cluster)

## Let's plot the clusters and their centers.
plot(ath9[c("Age", "Height")], col = kmeans.result$cluster)
points(kmeans.result$centers[,c("Age", "Height")], col = 1:3, pch = 8, cex=2)


############################ Hierarchical Clustering ##########################

## We will perform hierarchical clustering with function hclust()

idx <- sample(1:dim(x)[1], 40)
xSample <- x[idx,]
xSample$Medal <- NULL
hc <- hclust(dist(xSample), method="ave")
plot(hc, hang = -1, labels=x$Medal[idx])

## Let's cut the tree into 3 clusters
rect.hclust(hc, k=3)
groups <- cutree(hc, k=3)

############################ Density-based Clustering #########################
## The DBSCAN algorithm provides a density based clustering for numeric data. 

library(fpc)
Ath9 <- x[-5]   ## Remove class
ds <- dbscan(Ath9, eps=0.42, MinPts=5)

## Let's compare the clusters with the original class labels
table(ds$cluster, x$Medal)

#Let's plot
plot(ds, Ath9)

#Let's display the clusters in a scatter plot using the first and fourth columns of the data.
plot(ds, ath9[c(1,4)])

#Let's use plotcluster.
plotcluster(ath9, ds$cluster)

#Let's do some predicting.
#Create a new dataset for labeling
set.seed(435)
idx1 <- sample(1:nrow(x), 10)
newData <- x[idx1,-5]
newData <- newData + matrix(runif(10*4, min=0, max=0.2), nrow=10, ncol=4)

## Label new data
myPred <- predict(ds, ath9, newData)

## Plot the result with new data as asterisks
plot(ath9[c(1,4)], col=1+ds$cluster)
points(newData[c(1,4)], pch="*", col=1+myPred, cex=3)

## Check cluster labels
table(myPred, x$Medal[idx1])

########################### Answer 1)########################

## Question 1: How do the cluster distributions that the different algorithms generate differ from one another?
# Ans: In a dataset the task of k-man algorithm is to categorize the attributes into groups. 
# This is an unsupervised learning algorithm. The algorithm will categorize the items into k groups of similarity. 
# To calculate that similarity, we will use the Euclidean distance as measurement.
# We need to define before itself , we need to perform preprocessing and post processing 
# We categorize each item to its closest mean and we update the mean's coordinates, 
# which are the averages of the items categorized in that mean so far.
# We repeat the process for a given number of iterations and at the end, we have our clusters.
# The problem with K-measn is that it does not handle the noise outliers.

# DB SCAN METHOD:
# Based on a set of data, DBSCAN groups together points that are close to each other based on a distance measurement (usually Euclidean distance) and a minimum number of points. 

# Firstly, it divides the data into n dimension then For each point in the dataset, 
# DBSCAN forms an n dimensional shape around that data point, 
# and then counts how many data points fall within that shape. 
# DBSCAN counts this shape as a cluster.
# Db scan is great with handling the outliers.

# Db Scan is good for small datasets for the bigger data sets
# it will not give the proper output.

# Heirarchial clustering :
# It is an algorithm that groups similar objects into groups called clusters, 
# where each cluster is distinct from each other cluster. 
# It forms nested cluster in the form of tree.
# The hierarchical clustering method often encounters difficulties regarding the selection of merge or spilt points.

####################### Answer 2)######################
# Question 2: Which algorithm produces the best results?
#Ans: We have visualization method for each of the method 
# so we can validate the results of clustering algorthms 
# If we see the results of all three clustering algorithms, 
# K Means, H Clust and DBscan,K-means can not  handle the noisy data, 
# DBScan can deal with noisy data but it fails to determine the number of clusters according to the class attribute. 
# we can say that result we are getting with H Clust are very good comparatively, 
# Because we have choice to decide the height of clustering cut, 
# that's why we are getting good results.

##################### Answer 3 ########################

# Question 3: Given the nature of your dataset and its attribute values, why would that alrgorithm produce the best result.
# Ans: According to our dataset we had taken medal  as the class attribute. 
# Our dataset contains numeric as well as categorical data. 
# Hclust algorithm works good for both categorical and numerical, 
# while K means works good for continues values 
# and DBscan works for categorical. 
# considering our dtaset, H clust is good fit as it a with both sort of datatypes.

#################### Answer 4 #####################

# Question 4: What do the results tell you about the data?  
# What business, political, or medical decisions could management make based on the results of your analysis? 
# Ans: Using clustering algorithsms we can group athlets based on their feature set and accordingly we can 
# work on them and get the best result out of it,
# For business point of view  we can set the different targets for different races for  example, 
# if we could group athlete by performance,
# we can set them simmilar target like race for 100 meter 200m in one cluster  and 1km or long race in other cluster,

# Medical point of view, 
# if we could group athlete by performance, we can give them different practice sessions, coach and diet also.

# Political point of view the country who is going to win, 
# can be predicted by considering the past records, 
# so investors and organizations can support that country financially.