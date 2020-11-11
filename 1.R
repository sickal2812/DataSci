library(readxl)
library(caret)
library(olsrr)
library(dplyr)
library(htestClust)



hos<- read.csv("HousingData.csv")



summary(hos)
hos$CRIM <- replace(hos$CRIM, which(is.na(hos$CRIM)), 3.6) 






hos$ZN <- replace(hos$ZN, which(is.na(hos$ZN)), 0)
hos$CHAS <- replace(hos$CHAS, which(is.na(hos$CHAS)), 0)
hos$INDUS <- replace(hos$INDUS, which(is.na(hos$INDUS)), 11)
hos$AGE <- replace(hos$AGE, which(is.na(hos$AGE)), 68)
hos$LSTAT <- replace(hos$LSTAT, which(is.na(hos$LSTAT)), 12.7)





NA_num <- t(colSums(is.na(hos)))
NA_prop <- t(colSums(is.na(hos))) / nrow(hos)
rbind(NA_num, NA_prop)



index = createDataPartition(hos$MEDV, p = .7, list = F)
train = hos[index, ]
test = hos[-index, ]

plot(medv~lstat, Boston)
pairs(~ medv + ptratio + black + lstat + dis + rm + crim, data = Boston, main = "Boston Data")
fit1=lm(medv~lstat,data=Boston)
summary(fit1)

summary(hos)



model <- lm(MEDV~., data = train)
prelm <- predict(model,newdata = test)
#DO cv성능평가



k <- ols_step_all_possible(model)
plot(k)
ols_step_best_subset(model)



predict(MEDV,model)
summary(model)



#f-test
var.test(hos$MEDV )



#가격 미치는 요인 측정
corx <-round(cor( na.omit(hos)),2)



corx[14,]



sort(abs(corx[14,]))#   TAX  INDUS  PTRATIO  RM  LSTAT 상위 5개 
summary(lm(MEDV~ TAX, data = hos))
summary(lm(MEDV~ INDUS, data = hos))
summary(lm(MEDV~ PTRATIO, data = hos))
summary(lm(MEDV~ RM, data = hos))
summary(lm(MEDV~ LSTAT, data = hos))



require('MASS')
# t-test -> p-value
t.test(hos$MEDV,hos$TAX)
t.test(hos$MEDV,hos$INDUS)
t.test(hos$MEDV,hos$PTRATIO)
t.test(hos$MEDV,hos$RM)
t.test(hos$MEDV,hos$LSTAT)



plot(medv~lstat, Boston)
pairs(~ medv + ptratio + black + lstat + dis + rm + crim, data = Boston, main = "Boston Data")
fit1=lm(medv~lstat,data=Boston)
summary(fit1)
plot(medv~lstat,Boston)
abline(fit1,col="red")
confint(fit1)
data.frame(lstat=c(10,20,30))
par(mfrow=c(2,2))
plot(fit1)
fit2=lm(medv~lstat+age,data=Boston)
summary(fit2)
fit3=lm(medv~.,Boston)
summary(fit3)
par(mfrow=c(2,2))
plot(fit3)
fit4=update(fit3,~.-age-indus)
summary(fit4)
fit6=lm(medv~lstat +I(lstat^2),Boston)
summary(fit6)
attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat, Boston)
points(lstat,fitted(fit6), col="red",pch=20)
predict(fit1,data.frame(lstat=c(10,20,30)),interval="confidence")
