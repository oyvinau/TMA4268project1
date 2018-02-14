#Task 3
#a
library(ggplot2)
library(GGally)
library(class)
library(MASS)
library(pROC)
wine=read.csv("https://www.math.ntnu.no/emner/TMA4268/2018v/data/Comp1Wine.csv",sep=" ")
wine$class=as.factor(wine$class-1)
colnames(wine)=c("y","x1","x2")
ggpairs(wine, ggplot2::aes(color=y))

n=dim(wine)[1]
set.seed(4268) #to get the same order if you rerun - but you change this to your favorite number
ord = sample(1:n) #shuffle 
test = wine[ord[1:(n/2)],]
train = wine[ord[((n/2)+1):n],]

fit=glm(y~x1+x2, family="binomial", data=train)
coef(fit)
round(exp(coef(fit)),3)
