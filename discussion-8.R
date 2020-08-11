################ Decision treee #################################
#installing the package tree to implement the decision tree algorithm
install.packages("tree")
# loading all the library files needed
library(tree)
#importing the dataset to identify the decision tree of all the variables 
creditcard <- read.csv("UCI_Credit_Card.csv")
data(creditcard)
creditcard
#The dataset is then divided into two components with the probability of 70 and 30 
#So that the training and testing datasets are created
ind <- sample(2, nrow(creditcard), replace=TRUE, prob=c(0.7,0.3))
ind
traindata <- creditcard[ind==1,]
testdata <- creditcard[ind==2,]
#Then depending on the attributes identified from the training and testing datasets 
#decision tree using the function tree is generated
dec_tree <- tree(LIMIT_BAL~.,data=creditcard)
#summary of the decision tree is determined which defines the variables included
#minimum, maximum values and the quartiles
summary(dec_tree)
#Then finally based on the attribute selection and comparision
#decision tree is plotted for easier understanding
plot(dec_tree)
text(dec_tree)

##############Probability tree #####################################
# installing the party package needed for loadinf the ctree functionality
install.packages("party")
library(party)
set.seed(123)
#ctree is used to create the probability decision tree to define the varaiable LIMIT_BAL
credit_ctree <- ctree(LIMIT_BAL~., data=creditcard)
#the tree obtained is identified manually and is also determined by plotting
print(credit_ctree)
plot(credit_ctree)
table(predict(credit_ctree), traindata$LIMIT_BAL)

############### Random forest Algorithm ################################
## As random forest would be a combination of trees 
#every tree defined is compared with the other to identify the best varaibles needed
install.packages("randomForest")
library(randomForest)
set.seed(1234)
rf <- randomForest(LIMIT_BAL~., data=creditcard, ntree=100, proximity=TRUE)
#This identified the required variables arranged in the order depending on the comparision 
#of around 100 trees
table(predict(rf), traindata$LIMIT_BAL)
print(rf)
plot(rf)
importance(rf)
# This random tree genrated is then tested against the testing dataset to ensure the criticality of the data
rf_pred <- predict(rf, newdata=testdata$LIMIT_BAL)
table(predict(rf), testdata$LIMIT_BAL)
#VAraibles identfied are then plotted 
plot(margin(rf,testdata$LIMIT_BAL))

