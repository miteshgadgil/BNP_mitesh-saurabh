# BNP Paribas Challenge
# Using Multinomial Logistic Regression
# Prepare Data
mydata <- read.csv("train.csv")
train <- trainData[complete.cases(trainData),]

del = array()
i = 1
for (j in 1:ncol(train)){
  if (class(train[,j]) == "factor"){
    del[i] = j
    i = i + 1
  }
}

X <- train[,-c(1,del)]
X$targetF <- factor(X$target)
X$out <- relevel(X$targetF,ref="1")

newtest = train[,-c(1,2,del)]

# Develop Logistic Regression Model
# install.packages("nnet")
library(nnet)

# Change the number of variables according to test the dependence
LRmodel <- multinom(out~v1+v2+v3+v4+v5+v6+v7 = X)
summary(LRmodel)

# Predict
pred <- predict(LRmodel,newtest)
predprob <- predict(LRmodel,newtest,type="prob")

# Classification Error
cm <- table(predict(LRmodel), X$out)
print(cm)
accuracy <- 1-(sum(diag(cm))/sum(cm))

# 2 tailed Z-test
z <- summary(mymodel)$coefficients/summary(mymodel)$standard.errors
p <- 
