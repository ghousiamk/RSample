install.packages("coop") 
install.packages("Isa")
library('OneR')
library('coop')

#Cosine Similarity
############################################################
cc<-read.csv("UCI_Credit_Card.csv")
cosine(cc$AGE,cc$default.payment.next.month,use="everything", inverse=FALSE)


#Covariance
############################################################
AGE=cc$AGE
DP=cc$default.payment.next.month
cov(AGE,DP)

#Here, cosine result shows how age and payment next month. Bcause default 
#payment doesnt mean anything to us. On the other hand covariance result 
#shows a positive value of 0.05 which indicates positive relational strength
#between age and payment due.


#Chi Square
############################################################
cc$SEX<-as.factor(cc$default.payment.next.month)
PMBG=(table(cc$SEX,cc$default.payment.next.month))
chisq.test(PMBG)


#Correlation
############################################################
cc$AGE<-as.numeric(as.factor(cc$AGE))
cc$SEX<-as.numeric(as.factor(cc$SEX))

age=cc$AGE
sex=cc$SEX
cor(age,sex,use="pairwise.complete.obs", method = c("pearson"))
