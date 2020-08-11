#Data Project 7: Decision Trees and Random Forest




install.packages("tree") # the builds a decision tree,
install.packages("party")   # The package "party" has the function ctree() which is used to create and analyze decison tree
install.packages("rpart")
install.packages("car")
install.packages("mlbench")
install.packages("mboost")
install.packages("textir")
install.packages("class")
install.packages("e1071")
install.packages("randomForest")
install.packages("h2o")
library(tree)
# Step1 : Building a model by spitting  training (70%) and test (30%).
getwd()
dataset1 <- athlete_events
#Display Data by Str()

str(dataset1)
dataset1
cols.num <- c("Name","Sex","Team","NOC","Games","Season","City","Sport","Event","Medal")
dataset1[cols.num] <- sapply(dataset1[cols.num],as.numeric)# 
dataset1[is.na(dataset1)] <- 0
dataset2<- sample(2, nrow(dataset1), replace=TRUE, prob=c(7.5, 2.5))
trainathelete <- dataset1[dataset2==1,]
testathelete<-dataset1[dataset2==2,]

x1 <- Medal ~.
athelete_tree <- tree(x1, data=trainathelete)

summary(athelete_tree)


#building a tree

print(athelete_tree) # built tree by printing

#plotting atree 
plot(athelete_tree)
text(athelete_tree)
#built tree needs to be tested with the test data .
testresult <- predict(athelete_tree, newdata = testathelete) 
table(testresult, testathelete$Medal)
show(testresult)
################ C TREE (conditional inference) ##################
getwd()
dataset<- athlete_events
str(dataset)
dataset
dataset3 <- dataset[1:100,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)]
str(dataset3)
set.seed(1234)
test<- sample(2, nrow(dataset3), replace=TRUE, prob=c(7.5, 2.5))
trainathelete1 <- dataset3[test==1,]
testathelete1<- dataset3[test==2,]


library(party)
x2 <- NOC ~ Weight + Year
athelete_ctree<- ctree(x2, data = trainathelete1)
##performing prediction by taking train data


table(predict(athelete_ctree), trainathelete1 $Medal)
print(athelete_ctree) # priniting the tree
plot(athelete_ctree) # plotting tree
##performing prediction by taking train data
testPredict1 <- predict(athelete_ctree, newdata = testathelete1)
table(testPredict1,testathelete1$Medal)


#########################random forest###########

dataset_1<-dataset[1:1000,]
dataset_1[is.na(dataset_1)]<-0
ind22 <- sample(2, nrow(dataset_1), replace=TRUE, prob=c(0.7, 0.3))
trainDataRF <- dataset_1[ind22==1,]
testDataRF <- dataset_1[ind22==2,]

library(randomForest)
rf <- randomForest(Medal ~ ., data=trainDataRF, ntree=100, proximity=TRUE)
table(predict(rf), trainDataRF$Medal)
print(rf)
attributes(rf)
## After that, we plot the error rates with various number of trees.
plot(rf)
## The importance of variables can be obtained with functions importance() and varImpPlot()
importance(rf)
varImpPlot(rf)
## Finally, the built random forest is tested on test data, and the result is checked with functions table() 
athletePred <- predict(rf, newdata=testDataRF)
table(athletePred, testDataRF$Medal)

##########################Answer 1##########################

#Random Forest works better because it has high predictive accuracy. 
#In random forest prediction is based on  on input features considered important for classification.
#It works well with missing data and gives better prediction. 
#If we compare the output of tree, c-tree and random forest, 
#we take comparatively less attributes as input in tree and in ctree
#while in random forest, we provide large dataset and get more accurate result. 
#When we compare all the result together Random forest gives the best results.




##########################Answer 2##########################

#Random forest works better because it can deal even with the missing data 
#and produces more accurate result comparing with K-NN and Na?ve Bayes. 
#K-NN is used for numeric dataset, Naivey bayes is used for categorical data set 
#while Random forest can be used for both numeric as well as categorical data set.
#This algorithm can handle high dimensional spaces as well as large number of training examples.


##########################Answer 3##########################

#From the question 2 and Question 3 we can conclude that random forest produces the best and accurate results. 
#For example when our  non-technical manager wants to predict the result of the country China winning a gold medal,
#we predict the result by using Random forest because it gives the accurate result and it works on categorical data as well as on  numeric data 
#and can deal with large data set.
#Non-technical manager is least concerned about the algorithm we are using, 
#he/she more focuses on the result.

#########################Answer 4##############################

#We believe random forest method will be helpful in every field including business, political and medical because random forest gives more accurate predictions.
#For the business point of view, investors or organizations can invest on that athletic who is going to win based on his/her past winning experience and health that we can predict using random forest. 
#For the political point of view the country who is going to win, can be predicted by considering the past records, so investors and organizations can support that country financially 
#Medically, we can predict the stamina and health of athletics by his height, weight and Age.

