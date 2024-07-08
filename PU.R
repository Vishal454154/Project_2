
library(lubridate)
library(xts)
library(olsrr)
library(ggplot2)
library(dplyr)
library(TSA)
library(tseries)
library(forecast)
library(ggformula)
library(ggcorrplot)
library(gridExtra)
library(reshape2)
library(vars)
library(MASS)
library(leaps)




D=read.csv("C:\\Users\\Admin\\Desktop\\Pune.csv")
dim(D)
D=D[,4:17]
names(D)
attach(D)
D$Date=dmy(D$Date)
str(D)
class(D)

# Variable selection

Model=step(lm(PRECTOTCORR~ALLSKY_SFC_UVA+ALLSKY_SFC_UVB+ALLSKY_SFC_PAR_TOT+T2M+TS+T2MDEW+RH2M+QV2M+PS+WS10M+WS50M),direction="both")
summary(Model)

# Basic EDA
C=cor(D[,2:13])
ggcorrplot(C,type="lower",lab=TRUE)

# The correlation shows multicollinerity we dealt with by deleting some variables.

# BASIC PLOTTING

ggplot(D,aes(x=Date,y=ALLSKY_SFC_PAR_TOT))+stat_spline(color="red",lwd=1)+stat_spline(aes(x=Date,y=TS),D,color="blue",lwd=0.75)+stat_spline(aes(x=Date,y=T2MDEW),D,color="dark green",lwd=0.75)+stat_spline(aes(x=Date,y=PS),D,color="purple",lwd=0.75)+stat_spline(aes(x=Date,y=WS50M),D,color="dark orange",lwd=0.75)+xlab("Date")+ylab("Variables")+ggtitle("Variables VS Date")+theme(plot.title=element_text(size=15,face="bold"))

ggplot(D,aes(x=ALLSKY_SFC_PAR_TOT,y=PRECTOTCORR))+xlab("Photosynthetically Active Radiation in W/m^2")+ylab("PRECIPITATION (mm)")+ggtitle("Photosynthetically Active Radiation in W/m^2 VS PRECIPITATION")+stat_spline(size=2,color="red",lwd=2)+theme(plot.title=element_text(size=15,face="bold"))

ggplot(D,aes(x=TS,y=PRECTOTCORR))+xlab("Surface Temperature in C")+ylab("PRECIPITATION (mm)")+ggtitle("Surface Temperature in C VS PRECIPITATION")+stat_spline(size=2,color="blue")+theme(plot.title=element_text(size=15,face="bold"))

ggplot(D,aes(x=T2MDEW,y=PRECTOTCORR))+xlab("Dew Point at 2 M")+ylab("PRECIPITATION (mm)")+ggtitle("Dewpoint at 2 M VS PRECIPITATION")+stat_spline(size=2,color="dark green")+theme(plot.title=element_text(size=15,face="bold"))

ggplot(D,aes(x=PS,y=PRECTOTCORR))+xlab("Surface Pressure (kPa)")+ylab("PRECIPITATION (mm)")+ggtitle("Surface Pressure in kPa VS PRECIPITATION")+stat_spline(size=2,color="orange")+theme(plot.title=element_text(size=15,face="bold"))

ggplot(D,aes(x=WS50M,y=PRECTOTCORR))+xlab("Wind Speed at 50 M")+ylab("PRECIPITATION (mm)")+ggtitle("Wind Speed at 50 M VS PRECIPITATION")+stat_spline(size=2,color="purple")+theme(plot.title=element_text(size=15,face="bold"))

# Boxplot of Variables
ggplot(data=melt(D[,c(4,6,7,10,12)]),aes(x=variable,y=value))+geom_boxplot(aes(fill=variable))+ggtitle("Boxplot of Variables")+theme(plot.title=element_text(size=15,face="bold"))


S=summary(D[,c(4,6,7,10,12)])
S


## Statistical Analysis

# Objective 1 and 2
# Multiple Linear Regression

# For All Variables
MLRM=lm(PRECTOTCORR~.,data=D)
MLRM
summary(MLRM)
ols_vif_tol(MLRM)
par(mfrow=c(2,2))
plot(MLRM)

# After Removing multicollinearity
MLRM2=lm(PRECTOTCORR~ ALLSKY_SFC_PAR_TOT+TS+T2MDEW+PS+WS50M,data=D)
MLRM2
summary(MLRM2)
ols_vif_tol(MLRM2)
par(mfrow=c(2,2))
plot(MLRM2)

y=(sqrt(PRECTOTCORR))

a=lm(y~ ALLSKY_SFC_PAR_TOT+TS+T2MDEW+PS+WS50M,data=D)
summary(a)
par(mfrow=c(2,2))
plot(a)



### Polynomial Regression

D=read.csv("C:\\Users\\Admin\\Desktop\\poly1.csv")
names(D)
attach(D)

## For 2nd order
NLRM=lm(PRECTOTCORR~poly(ALLSKY_SFC_PAR_TOT,2)+poly(TS,2)+poly(T2MDEW,2)+poly(PS,2)+poly(WS50M,2),data=D)
NLRM
summary(NLRM)
ols_vif_tol(NLRM)
par(mfrow=c(2,2))
plot(NLRM)

## For 3rd order
NLRM2=lm(PRECTOTCORR~poly(ALLSKY_SFC_PAR_TOT,3)+poly(TS,3)+poly(T2MDEW,3)+poly(PS,3)+poly(WS50M,3),data=D)
NLRM2
summary(NLRM2)
ols_vif_tol(NLRM2)
par(mfrow=c(2,2))
plot(NLRM2)


## For 5th order
NLRM5=lm(PRECTOTCORR~poly(ALLSKY_SFC_PAR_TOT,5)+poly(TS,5)+poly(T2MDEW,5)+poly(PS,5)+poly(WS50M,5),data=D)
NLRM5
summary(NLRM5)
ols_vif_tol(NLRM5)
par(mfrow=c(2,2))
plot(NLRM5)


## For transformed data y=sqrt
## For 2nd order
NLRM=lm(y~poly(ALLSKY_SFC_PAR_TOT,2)+poly(TS,2)+poly(T2MDEW,2)+poly(PS,2)+poly(WS50M,2),data=D)
NLRM
summary(NLRM)
ols_vif_tol(NLRM)
par(mfrow=c(2,2))
plot(NLRM)

## For 3rd order
NLRM2=lm(y~poly(ALLSKY_SFC_PAR_TOT,3)+poly(TS,3)+poly(T2MDEW,3)+poly(PS,3)+poly(WS50M,3),data=D)
NLRM2
summary(NLRM2)
ols_vif_tol(NLRM2)
par(mfrow=c(2,2))
plot(NLRM2)

## For 5th order
NLRM5=lm(y~poly(ALLSKY_SFC_PAR_TOT,5)+poly(TS,5)+poly(T2MDEW,5)+poly(PS,5)+poly(WS50M,5),data=D)
NLRM5
summary(NLRM5)
ols_vif_tol(NLRM5)
par(mfrow=c(2,2))
plot(NLRM5)



### ARIMA Fitting

library(lubridate)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(astsa)
library(forecast)
library(readxl)
library(urca)
library(ggfortify)
library(tsutils)
library(e1071)
library(caTools)
library(caret)
library(tseries)
library(forecast)
library(knitr)
library(rmarkdown)
library(fpp)
library(fma)
library(expsmooth)
library(lmtest)


## Forecasting using different methods

D=read.csv("C:\\Users\\Admin\\Desktop\\P1.csv",header=TRUE,row.names=1)
attach(D)
names(D)
d=c(D$X2016,D$X2017,D$X2018,D$X2019,D$X2020,D$X2021,D$X2022)

D=ts(d,start=c(2016,1),frequency=12)
D

plot.ts(D)

boxplot(split(D,cycle(D)),names=month.abb,xlab="Months",ylab="Precipitation in mm/month",col="sky blue")+title("Boxplot of Precipitation(Monthly)")

train=window(D,start=2016,end=c(2020,12))
test=window(D,start=2021,end=c(2022,12))

plot(train,main="Monthly Precipitation (mm)",xlab="Months",xlim=c(2016,2023))
lines(meanf(train,h=24)$mean,col=4)
lines(rwf(train,h=24)$mean,col=2)
lines(rwf(train,drift=TRUE,h=24)$mean,col=3)
lines(snaive(train,h=24)$mean,col=5)

legend("topleft",lty=1,col=c(4,2,3,5),legend=c("Mean Method","Naive Method","Drift Method","Seasonal naive Method"),bty="n")
lines(train,col="red")

accuracy(meanf(train,h=24),test)
accuracy(rwf(train,h=24),test)
accuracy(rwf(train,drift=TRUE,h=24),test)
accuracy(snaive(train,h=24),test)


train.mean = meanf(train,h=48)$mean
train.naive = rwf(train,h=48)$mean
train.drift = rwf(train,drift=TRUE,h=48)$mean
train.seas = snaive(train,h=48)$mean

# plot the test set
plot(test, main="Precipitation", ylab="", xlab="Months")

# plot forecasting for 4 years according to four methods
lines(train.mean, col=4)
lines(train.naive, col=2)
lines(train.drift, col=3)
lines(train.seas, col=5)

# legend
legend("topleft", lty=1, col=c(4,2,3,5),legend=c("Mean method","Naive method","Drift method", "Seasonal naïve method"),bty="n")

trainData = train
testData = test

#  the default value in auto.arima() is test="kpss". 
# A KPSS test has a null hypothesis of stationarity
# In general, all the defaults are set to the values that give the best forecasts on average.

arimaMod = auto.arima(trainData, stepwise=FALSE, approximation=FALSE)
arimaMod
arimaMod.Fr =forecast(arimaMod,h=24)
arimaMod.Fr
# plot of the prediction and of the test set

plot(arimaMod.Fr)
lines(testData, col="red")
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("Actual Values","ARIMA Prediction"))

AR.mean =forecast(arimaMod,h=24)$mean

plot(testData, main="", ylab="Precipitation in mm", xlab="Months", col="red")  
lines(AR.mean,col="blue")
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("Actual Values","ARIMA Prediction"))

accuracy(arimaMod.Fr,testData)

tsdisplay(residuals(arimaMod))


# Ljung box test and assumptions checking
lb = Box.test(residuals(arimaMod), lag = 24, type = "Ljung-Box")
lb

res.fr= residuals(arimaMod.Fr)

par(mfrow=c(1,3))

plot(res.fr, main="Residuals from ARIMA method",ylab="",xlab="Years")

Acf(res.fr, main="ACF of residuals")

u = residuals(arimaMod)

m=mean(u)
std=sqrt(var(u))
hist(u, breaks=20,col="gray",prob=TRUE,xlab="Residuals", main="Histogram of residuals\n with Normal Curve")
curve(dnorm(x,mean=m,sd=std),col="black",lwd=2,add=TRUE)




## Logistic regression

model=glm(TPM~ ALLSKY_SFC_PAR_TOT+TS+T2MDEW+PS+WS50M,data=D,family="binomial")
model
summary(model)

qchisq(0.05,5)

par(mfrow=c(2,2))
plot(model)





## Final prediction

library(lubridate)
library(xts)
library(olsrr)
library(ggplot2)
library(dplyr)
library(TSA)
library(tseries)
library(forecast)
library(ggformula)
library(ggcorrplot)
library(gridExtra)
library(reshape2)
library(vars)
library(MASS)
library(leaps)
library(caret)
library(caTools)
library(tidyverse)
library(ISLR)
library(rpart)
library(rpart.plot)
library(plotly)
library(randomForest)
library(RColorBrewer)


D=read.csv("C:\\Users\\Admin\\Desktop\\Pune.csv")
dim(D)
D=D[,4:17]
names(D)
attach(D)
D$Date=dmy(D$Date)
y=sqrt(D$PRECTOTCORR)
str(D)
class(D)


D=data.frame(ALLSKY_SFC_PAR_TOT,TS,T2MDEW,PS,WS50M,y)

train=subset(D[1:1790,])
test=subset(D[1791:nrow(D),])
dim(train)
dim(test)
names(train)

cols=c("ALLSKY_SFC_PAR_TOT","TS","T2MDEW","PS","WS50M")
pre_proc_val=preProcess(train[,cols],method=c("center","scale"))

train[,cols]=predict(pre_proc_val,train[,cols])
test[,cols]=predict(pre_proc_val,test[,cols])

summary(train)

model=rpart(y~.,data=train,method="anova")
model  ## 
printcp(model)
plotcp(model)

best=model$cptable[which.min(model$cptable[,"xerror"]),"CP"]
best
model=rpart(y~.,data=train,cp=best)
model
rpart.plot(model,type=3,fallen.leaves=TRUE)

pred=predict(model,newdata=test)

RMSE(pred,test$y)

mse=mean((pred-test$y)^2)
mse

MAE(pred,test$y)

eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))  
# Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
}

eval_results(test$y,pred,test)


rf_model=randomForest(y~.,data=train)
rf_model
summary(rf_model)
importance(rf_model)
plot(rf_model)

pred_rf=predict(rf_model,newdata=test)
eval_results(test$y,pred_rf,test)


D1=data.frame(x=Date[1791:nrow(D)],y=test$y,pred_rf)
attach(D1)
D1$x=dmy(D1$x)
str(D1)
class(D1)
names(D1)
my.cols=c("red","blue")
my.names=c("y","pred_rf")
names(my.cols)=my.names


ggplot(D1,aes(x=x,y=y))+stat_spline(color="red")+stat_spline(aes(x=x,y=pred_rf),D1,color="blue")+theme(plot.title=element_text(size=15,face="bold"))+scale_color_manual(values=c("original"="red","forecast"="blue"))+labs(title="Actual VS Predicted Plot of Rainfall")+xlab("Years")+ylab("Preipitation")
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("Actual Values","Random Forest Prediction"))


#### STOCHASTIC OBJECTIVE
n=100
year2016=matrix(c(187,10,10,159),nrow=2,ncol=2,byrow=T)
y2016=year2016/rowSums(year2016)
y2016
result1 = y2016 %^% n
print(result1)


year2017=matrix(c(178,11,11,165),nrow=2,ncol=2,byrow=T)
y2017=year2017/rowSums(year2017)
y2017
n=100
result2 = y2017 %^% n
print(result2)


year2018=matrix(c(175,15,15,160),nrow=2,ncol=2,byrow=T)
y2018=year2018/rowSums(year2018)
y2018
n=100
result3 = y2018 %^% n
print(result3)

year2019=matrix(c(185,8,8,164),nrow=2,ncol=2,byrow=T)
y2019=year2019/rowSums(year2019)
y2019
n=100
result4 = y2019%^% n
print(result4)

year2020=matrix(c(150,10,10,196),nrow=2,ncol=2,byrow=T)
y2020=year2020/rowSums(year2020)
y2020
n=100
result5 = y2020 %^% n
print(result5)

year2021=matrix(c(133,12,12,208),nrow=2,ncol=2,byrow=T)
y2021=year2021/rowSums(year2021)
y2021
n=100
result6 = y2021 %^% n
print(result6)

year2022=matrix(c(159,17,17,172),nrow=2,ncol=2,byrow=T)
y2022=year2022/rowSums(year2022)
y2022
n=100
result7 = y2022 %^% n
print(result7)


result1
result2
result3
result4
result5
result6
result7






