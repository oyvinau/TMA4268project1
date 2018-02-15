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

p = ggplot(train, aes(x = x1, y = x2, color = y)) + geom_point(size = 2.5)

wine_lda <- lda(y ~ x1 + x2, data = train, prior = c(0.4538, 1 - 0.4538))

testgrid = expand.grid(x1 = seq(min(wine[,2]-0.2), max(wine[,2]+0.2), 
                                          by=0.5), x2 = seq(min(wine[,3]-0.2),
                                                                      max(wine[,3]+0.2), by=0.125))

res = predict(object = wine_lda, newdata = testgrid)
wines = res$class
postprobs=res$posterior

wine_lda_df = bind_rows(mutate(testgrid, wines))
wine_lda_df$Species_lda = as.factor(wine_lda_df$wines)

p = p + geom_point(data = wine_lda_df, aes(x = x1, y = x2, colour = wines), size=1.2, pch = 3)
# print(p)

X <- data.matrix(train)
sort(X, y)

S1 <- t(X1) %*% X1
S2 <- t(X2) %*% X2

S <- (S1 + S2) / (n - 2)

Sinv <- solve(S)

pi1hat <- n1 / n; pi2hat <- n2 / n

c <- 0.5 * (t(mu1hat) %*% Sinv %*% mu1hat - t(mu2hat) %*% Sinv %*% mu2hat) + log(pi1hat / pi2hat) 
 
A <- t(mu2hat) %*% Sinv - t(mu1hat) %*% Sinv
# print(A)
# print(c)












