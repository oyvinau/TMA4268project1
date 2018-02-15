library(ggplot2)
library(dplyr)
data = read.table("https://www.math.ntnu.no/emner/TMA4268/2018v/data/SYSBPreg3uid.txt")

modelA = lm( -1/sqrt(SYSBP) ~ ., data = data ) 
sA = summary(modelA)

modelB = lm(SYSBP ~ ., data = data)
sB = summary(modelB)

new = data.frame(SEX=1, AGE=56, CURSMOKE=1, BMI=89/1.75^2, TOTCHOL=200, BPMEDS=0)
y0 <- predict.lm(modelA, new)

tvalues <- qt( c(0.05, 0.95), df = 2593 )

X <- as.matrix(data)
Y = -1/sqrt(X[,1])
X[,1] = 1
x0 <- c(1, 1, 56, 1, 89/1.75^2, 200, 0)

beta <- modelA$coefficients

k <- sqrt( 1 + t(x0) %*% solve(t(X) %*% X, x0) )
H = X %*% solve(t(X) %*% X, t(X))
sigmahat2 <- t(Y) %*% (diag(length(Y)) - H) %*% Y / (n - length(X[1,]))

sigmathat <- sA$sigma
left <- 1 / ( t(x0) %*% beta + sigmahat * k * tvalues[1] )^2
right <- 1 / ( t(x0) %*% beta + sigmahat * k * tvalues[2] )^2
# print(tvalues)
# print(c(left, right))

wine = read.csv("https://www.math.ntnu.no/emner/TMA4268/2018v/data/Comp1Wine.csv",sep=" ")
wine$class = as.factor(wine$class-1)
colnames(wine) = c("y","x1","x2")
n=dim(wine)[1]
set.seed(1729)
ord = sample(1:n) 
test = wine[ord[1:(n/2)],]
train = wine[ord[((n/2)+1):n],]

knn9 = knn(train = train[,-1], test = test[,-1], k = 9, cl = train$y, prob = FALSE)

X = data.matrix(test)
Y <- X[,1]
X[,1] = 1
betahat = as.matrix(fit$coefficients)
dot = X %*% betahat
predglm = as.numeric( exp(dot) / (1 + exp(dot)) )

glmroc=roc(response=test$y,predictor=predglm)
plot(glmroc)
auc(glmroc)
KNN3 = knn(train = train[,-1], test = test[,-1], k = 3, cl = train$y, prob = F)
KNN3probwinning = attributes(knn(train = train[,-1], test = test[,-1], k = 3, cl = train$y, prob = TRUE))$prob
KNN3prob <- ifelse(KNN3 == "0", 1-KNN3probwinning, KNN3probwinning)
KNN3roc=roc(response=test$y,predictor=KNN3prob) 
# plot(KNN3roc)
# auc(KNN3roc)
ltrain=lda(y~x1+x2,data=train)
lpred=predict(object = ltrain, newdata = test)$posterior[,1]
lroc=roc(response=test$y,lpred)
# plot(lroc)
# auc(lroc)




