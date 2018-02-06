library(ggplot2)
data = read.table("https://www.math.ntnu.no/emner/TMA4268/2018v/data/SYSBPreg3uid.txt")

modelA=lm(-1/sqrt(SYSBP) ~ ., data = data) 
sA = summary(modelA)

X = data.matrix( data )
p = length(X[1,])
n = length(X[,1])

Y = X[,1]
Y = -1 / sqrt(Y)
X[,1] = 1
betahat = solve(t(X) %*% X, t(X) %*% Y)
Yhat = X %*% betahat
sigmahat = as.numeric( sqrt( t(Y - Yhat) %*% (Y - Yhat) / (n - p - 1) ) )
varbeta = sigmahat * sigmahat * solve(t(X) %*% X)
stderr = sqrt( diag (varbeta) )
tvalues = betahat / stderr

Ybar = sum(Y) / n
TSS = t(Y - Ybar) %*% (Y - Ybar)
RSS = t(Y - Yhat) %*% (Y - Yhat)
RSE = RSS / (n - 2)

Rsq = (TSS - RSS) / TSS

f = ( (TSS - RSS) / p ) / ( RSS / (n - p - 1) )

H = X %*% solve(t(X) %*% X, t(X))
lev = diag(H)

res = (Y - Yhat) / (sigmahat * sqrt(1 - lev))
resvslev = data.frame(lev, res)
# print(lev)

p = ggplot() + geom_point(data = resvslev, aes(x= lev, y = res))
print(p)

modelB = lm(SYSBP ~ ., data = data)
sB = summary(modelB)

# residuls vs fitted
ggplot(modelA, aes(.fitted, .resid)) + geom_point(pch = 21) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_smooth(se = FALSE, col = "red", size = 0.5, method = "loess") + 
  labs(x = "Fitted values", y = "Residuals", title = "Fitted values vs. residuals", subtitle = deparse(modelA$call))

# qq-plot of residuals
ggplot(modelA, aes(sample = .stdresid)) +
  stat_qq(pch = 19) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  labs(x = "Theoretical quantiles", y = "Standardized residuals", title = "Normal Q-Q", subtitle = deparse(modelA$call))

# normality test
library(nortest) 
ad.test(rstudent(modelA))









