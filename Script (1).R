# > Team member names: 
Ghousia Katlagal

bk = read.csv("C:/Users/Computer/Desktop/datamining/googleplaystore.csv")

str(bk)

# Using lapply converting all columns to numeric
bk1 <- lapply(bk,as.numeric)
bk1 <- data.frame(bk1)
str(bk1)

#####  1      ################
# Class Attribute - > y

#Which class attribute does your dataset have?  
#For what business or political purposes would someone be interested in 
#this class attribute?

bk1$Rating = as.numeric(bk1$Rating)

bk1 = bk1[bk1$Rating <=5,]

bk1 = na.omit(bk1, cols="Rating")

bk1$Rating = ifelse(bk1$Rating<2.5,'LowRating','HighRating')

print(table(bk1$Rating))

#We have Rating of the APP which is the Class of the dataset which we need to predict

############  2     #############
# 
# List the Minimum, Maximum, Mean, Median, Mode, Q1, Q3, and Standard Deviation for two appropriate attributes.  Which 
# attribute has the smaller standard deviation?
#   

bk_preprocess <- bk1[,c(2,4)]
str(bk_preprocess)

## min,max

print(summary(bk_preprocess$Category))
print(summary(bk_preprocess$Reviews))

## mode
print(names(table(bk$Category))[table(bk1$Category)==max(table(bk1$Category))])

print(names(table(bk$Genres))[table(bk1$Genres)==max(table(bk1$Genres))])

## sd

print(sd(bk1$Category))

print(sd(bk1$Reviews))

## Plot ##

################################# 3 ##############################
# For each of the attributes above, make a scatterplot and describe in detail how each of the attributes is correlated to 
# the identified class attribute.

barplot(table(bk1$Rating))

ggplot(aes(x=Rating, y=Reviews, group = Rating, fill = Rating), data = bk1) +
  geom_bar(stat = 'identity', position = 'dodge')

plot(bk1$Reviews , bk1$Genres)


################################# 4 ##############################

## Splitting Dataset ##

# Separate the dataset into 25% training data and 75% test data.  Remove the class attribute values from the test data 
# set. Then prepare the training data to run with two of the classifiers below.

bk_preprocess = bk1[1:3000,]

ind <- sample(2, nrow(bk_preprocess), replace=TRUE, prob=c(0.75, 0.25))
trainDataset <- bk_preprocess[ind==1,]
testDataset <- bk_preprocess[ind==2,]
trainDataset$Rating = as.factor(trainDataset$Rating)


str(trainDataset)

library(mlbench)

library(e1071)

## Naive Bayes Method ##
bk1$Rating = as.factor(bk1$Rating)

nb_model_bk <- naiveBayes(Rating ~.,data = trainDataset)
nb_model_bk
summary(nb_model_bk)
str(nb_model_bk)
nbm_test_predict_bk <- predict(nb_model_bk,subset(testDataset,select = -c(Rating)))
mean(nbm_test_predict_bk==testDataset$Rating)

## Output ##
# 38.64585


## Random Forest ##
library(randomForest)
rf_bk <- randomForest(Rating ~ ., data=trainDataset, ntree=15, proximity=TRUE)
rf_pred = predict(rf_bk,newdata = testDataset)
mean(rf_pred == testDataset$Rating)

#output
#0.9950031
importance(rf_bk)
varImpPlot(rf_bk)

Rand_Pred_bk <- predict(rf_bk, newdata=testDataset)
mean(Rand_Pred_bk==testDataset$Rating)
100*sum(Rand_Pred_bk==testDataset$Rating)/length(testDataset$Rating)

########################## 5 ######################################## 
# 5 Using the test data set you have just generated, remove  the class attribute and run two of the clustering algorithms 
# below. 
## clustering ##
## k means

normalize = function(x){
  return( (x - min(x)) / (max(x) - min(x)) )
}

testDataset_new = normalize(testDataset[,-3])


testDataset_new$y <- NULL

(kmeans.result <- kmeans(testDataset_new,2))

table(testDataset$Rating,kmeans.result$cluster)

testDataset_new$Rating = testDataset$Rating

testDataset_new$y = kmeans.result$cluster
## DBSCAN
##

library(fpc)

dbscan_data <- dbscan(testDataset[,-3], eps=1.5, MinPts=5)

## Let's compare the clusters with the original class labels
table(dbscan_data$cluster, testDataset$Rating)
#plot(dbscan_data, testDataset[,-3])
plotcluster(testDataset[,-3], dbscan_data$cluster)


########################## 6 ######################################## 
# 6 What kinds of business or political decisions could someone make on the basis of your analysis?

#Based on ratings, genres and view and rest of the features we can predict the ratings for song which it will get

