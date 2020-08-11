
install.packages('nutshell')
install.packages('lsa')
install.packages('lattice')
install.packages('MASS')
install.packages('OneR')
install.packages('proxy')
library('nutshell')
library('lattice')
library('MASS')
library('OneR')
library('proxy')
library('lsa')
Lab3 <- read.csv("athlete_events.csv")
na.rm = TRUE
#1.	How did you decide which two or three attributes to pick for your analysis?

##After viewing the datset I thought based on Height the weigh get changes so I picked up Height and Weight 

c <- c(Lab3$Height)
d <- c(Lab3$Weight)
res <- lsa::cosine(c,d)
res


#As i got NA in my cosine i found there is no similarities between Height and Weight 

Covariance_result = cov(c,d)
Covariance_result
#2.What connections between the attributes do the results of your analysis reveal?  
 # Result shows They do not have any relation between these attributes
#3.	Were there any surprises?  Did you have to go back and pick different attributes and re-run your exercise?
#After viewing the datset I thought Games and seasons have similarities 
a <- c(Lab3$Games)
b <- c(Lab3$Season)
res <- lsa::cosine(a,b)
res

#Here we found the similarities the cosine value is 0.8951453

# I am finiding covarience between Games and Season
Covariance_result = cov(a,b)
Covariance_result
# the covarience result is > Covariance_result = cov(a,b)
#> Covariance_result
#[1] 0.8275204
# A positive covariance means that the two variables at hand are positively related, and they move in the same direction.

attr1 <- as.numeric(Lab3$Season)
attr2 <- as.numeric(Lab3$Games)
attr1
attr2
#Correlation between Games and Season
cor(attr1,attr2,use="all.obs",method=c("pearson")) 
plot(attr2, attr1, xlab = "Games", ylab = "Season")





#Chi Square between  Games and Season
summary(Lab3$Games)
summary(Lab3$Season)
#Converting Games column to categorical field
Lab3$Gamesbins<- bin(Lab3$Games, nbins = 4, labels = c("low","Average","Above Average", "high"))
bins<-data.frame(Lab3$Gamesbins, Lab3$Games)
str(bins)
summary(bins)
bins_sorted<-bins[order(Lab3$Gamesbins),]
plot(Lab3$Gamesbins, main="Automatically generated bins with equal value thresholds ",xlab="Lab3$Gamesbins",ylab="Lab3$Games")

#Converting Season column to categorical field
Lab3$Seasonbins<- bin(Lab3$Season, nbins = 4, labels = c("low","Average","Above Average", "high"))
bins2<-data.frame(Lab3$Seasonbins, Lab3$Season)
str(bins2)
summary(bins2)
bins_sorted2<-bins[order(Lab3$Seasonbins),]
plot(Lab3$Seasonbins, main="Automatically generated bins with equal value thresholds ",xlab="Lab3$longitudebins",ylab="Lab3$longitude")

# Making use of bins in Chi Square
Chi_Square=table(Lab3$Gamesbins, Lab3$Seasonbins)
Chi_Square
chisq.test(Chi_Square)
# The p value is < 2.2e-16 which is very less it means they are dependent.


#4.After calculating the ChiSquare I got the p value < 2.2e-16 which is very much less than 0.05. So I can say Games and Season are dependent to each other.
#The correlation between Games and Season is 0.1575068 which is a positive value it means the both attributes are directly related. 
#They relation between them we can define as Games and Season is if one attribute is increases then the other will also get increases
#Also the covariance is 0.8275204 whisch is a positive number that  indicate a positive linear relationship between the variables
#With all of the above points  I can say I can able to predict future values of Games and Season attribute 