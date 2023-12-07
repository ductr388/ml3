# Lab 3 block 1 of 732A99/TDDE01/732A68 Machine Learning
# Author: jose.m.pena@liu.se
# Made for teaching purposes

library(kernlab)
set.seed(1234567890)

data(spam)
foo <- sample(nrow(spam))
spam <- spam[foo,]
spam[,-58]<-scale(spam[,-58])
tr <- spam[1:3000, ]
va <- spam[3001:3800, ]
trva <- spam[1:3800, ]
te <- spam[3801:4601, ] 

# Divided data according to:
# Train_30:    3000 
# Validation:  800 
# Test:        800

# Have another data division:
# Train_38: 3800
# Test :    800

# Finds the best value of C by using validation data
by <- 0.3
err_va <- NULL
for(i in seq(by,5,by)){
  filter <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=i,scaled=FALSE)
  mailtype <- predict(filter,va[,-58])
  t <- table(mailtype,va[,58])
  err_va <-c(err_va,(t[1,2]+t[2,1])/sum(t))
}


# Model trained on train_30, error on validation <- not good.
filter0 <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter0,va[,-58])
t <- table(mailtype,va[,58])
err0 <- (t[1,2]+t[2,1])/sum(t)
err0


# Model trained on train_30, error on test <- Candidate 
filter1 <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter1,te[,-58])
t <- table(mailtype,te[,58])
err1 <- (t[1,2]+t[2,1])/sum(t)
err1

# Model trained on train_38, error on test <- Candidate? We are however using 800 obs used from validation to find c... 
filter2 <- ksvm(type~.,data=trva,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter2,te[,-58])
t <- table(mailtype,te[,58])
err2 <- (t[1,2]+t[2,1])/sum(t)
err2

# Model trained on all data, error on test <- not good. 
filter3 <- ksvm(type~.,data=spam,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter3,te[,-58])
t <- table(mailtype,te[,58])
err3 <- (t[1,2]+t[2,1])/sum(t)
err3

# Questions

# 1. Which filter do we return to the user ? filter0, filter1, filter2 or filter3? Why?
# Filter1: This is the only filter that we have an accurate generalization error of. 

# 2. What is the estimate of the generalization error of the filter returned to the user? err0, err1, err2 or err3? Why?
# Err0: 0.0675, however this is not accurate as generalization error due to the value of c was found by validation data
# which the error is using.
# Err1: 0.08489388, accurate generalizaton error of the filter. We are using data never seen by the model.
# Err2: 0.082397, not so accurate generalization error. Data used to find hyperparameter c is reused to train the model. => overfitting?
# Err3: 0.02122347, not accurate generaizaton error. Test data is used as training data also. Generalization error is smaller then it is for new data. 

# 3. Implementation of SVM predictions.

sv<-alphaindex(filter3)[[1]]
co<-coef(filter3)[[1]]
inte<- - b(filter3)
k<-NULL
for(i in 1:10){ # We produce predictions for just the first 10 points in the dataset.
  k2<-NULL
  for(j in 1:length(sv)){
    k2<- # Your code here
  }
  k<-c(k, # Your code here)
}

k
predict(filter3,spam[1:10,-58], type = "decision")
