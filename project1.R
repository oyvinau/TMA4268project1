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

wine_lda <- lda(y ~ x1 + x2, data = train) #, prior = c(pi1hat, 1 - pi1hat))
pred <- predict(wine_lda, data = test)$class
ytest <- test[,1]
t <- table(ytest, pred)
print(t)




