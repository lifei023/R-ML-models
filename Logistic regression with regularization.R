##Question 2
# Library
#install.packages("glmnet")
setwd('D:/Georgian tech/Courses/ISYE8803/Final/Q1')
library(glmnet)
# load data
set.seed(7)
Xtrain=read.csv('Ionosphere.train.csv',stringsAsFactors = FALSE,header = FALSE)
Ytrain=read.csv('labels.train.csv',stringsAsFactors = FALSE,header = FALSE)
Xtest=read.csv('Ionosphere.test.csv',stringsAsFactors = FALSE,header = FALSE)
Ytest=read.csv('labels.test.csv',stringsAsFactors = FALSE,header = FALSE)

summary(Xtrain)
#scale the data
# Xtrain[2:33]=scale(Xtrain[2:33])
# Xtest[2:33]=scale(Xtest[2:33])

################################### part b #############################333333
#combine the input and response 
colnames(Ytrain)[colnames(Ytrain)=="V1"] <- "y"
data1=cbind(Xtrain,Ytrain)
data1$y=ifelse(data1$y=='g',1,0)

#build up the logistic regression model and make predictions on the test set
fit1= glm(y ~ .,family=binomial(link = "logit"),data=data1)
coef(fit1)

#test model 
yhat1=predict(fit1,Xtest,type = "response")
yhat_round1 <- as.integer(yhat1 > 0.5)

# confusion matrix
y_true=ifelse(Ytest$V1=='g',1,0)
t1 <- table(yhat_round1,y_true)
t1

################################### part c ############################ 
x=as.matrix(data1[1:33])
y=as.matrix(data1[34])
xtest=as.matrix(Xtest)

#build up the ridge logistic regresson model
fit2=cv.glmnet(x,y,family = "binomial",nfolds=5,intercept = TRUE,
               standardize = FALSE,type.measure = "class",alpha = 0)
fit2$lambda.min
plot(fit2)

#optimal coefficients
coef(fit2,s="lambda.min")

#test model 
yhat2=predict(fit2,xtest,s = "lambda.min", type = "response")
yhat_round2 <- as.integer(yhat2 > 0.5)

# confusion matrix
t2 <- table(yhat_round2,y_true)
t2


################################### part d ############################ 
x=as.matrix(data1[1:33])
y=as.matrix(data1[34])
xtest=as.matrix(Xtest)

#build up the lasso logistic regresson model
fit3=cv.glmnet(x,y,family = "binomial",nfolds=5,intercept = TRUE,
               standardize = FALSE,type.measure = "class",alpha = 1)
fit3$lambda.min
plot(fit3)

#optimal coefficients
coef(fit3,s='lambda.min')

#test model 
yhat3=predict(fit3,xtest,s = "lambda.min", type = "response")
yhat_round3 <- as.integer(yhat3 > 0.5)

# confusion matrix
t3 <- table(yhat_round4,y_true)
t3


################################### part e ############################ 
gamma=2
b.ridge = matrix(round(coef(fit2,s="lambda.min"),3))[2:34]
w2 = 1/abs(b.ridge)^gamma
# alasso2 = cv.glmnet(X, y, family = "gaussian", alpha = 1, intercept = TRUE, penalty.factor = w2)

fit4=cv.glmnet(x,y,family = "binomial",nfolds=5,intercept = TRUE,
               standardize = FALSE,type.measure = "class",alpha = 1,penalty.factor = w2)

fit4$lambda.min
plot(fit4)

#optimal coefficients
coef(fit4,s='lambda.min')

#test model 
yhat4=predict(fit4,xtest,s = "lambda.min", type = "response")
yhat_round4 <- as.integer(yhat4 > 0.5)

# confusion matrix
t4 <- table(yhat_round4,y_true
t4
