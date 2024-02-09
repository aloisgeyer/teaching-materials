library(tree)
library(rpart)
library(rpart.plot)
library(Metrics)

data=read.csv(file="hitters.csv",sep=";")

data=na.omit(data)

fit.lm=lm(Salary~Years+Hits+AtBat,data)
summary(fit.lm)

fit.tree=tree(Salary~Years+Hits+AtBat,data)
summary(fit.tree)
plot(fit.tree)
text(fit.tree,cex=0.75)

fitted.tree=predict(fit.tree,newdata=data[,c("Years","Hits","AtBat")])
rmse(data$Salary,fitted.tree)

fit.rpart=rpart(Salary~Years+Hits+AtBat,data)
summary(fit.rpart)
rpart.plot(fit.rpart)
text(fit.rpart,cex=0.75)

fitted.rpart=predict(fit.rpart,newdata=data[,c("Years","Hits","AtBat")])
rmse(data$Salary,fitted.rpart)