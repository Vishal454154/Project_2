
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




D=read.csv("C:\\Users\\Admin\\Desktop\\Solapur.csv")
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

ggplot(D,aes(x=Date,y=ALLSKY_SFC_PAR_TOT))+stat_spline(color="red")+stat_spline(aes(x=Date,y=TS),D,color="blue")+stat_spline(aes(x=Date,y=T2MDEW),D,color="green")+stat_spline(aes(x=Date,y=PS),D,color="purple")+stat_spline(aes(x=Date,y=WS50M),D,color="orange")+xlab("Date")+ylab("Variables")+ggtitle("Variables VS Date")+theme(plot.title=element_text(size=15,face="bold"))

ggplot(D,aes(x=ALLSKY_SFC_PAR_TOT,y=PRECTOTCORR))+xlab("Photosynthetically Active Radiation in W/m^2")+ylab("PRECIPITATION (mm)")+ggtitle("Photosynthetically Active Radiation in W/m^2 VS PRECIPITATION")+stat_spline(size=1,color="red")+theme(plot.title=element_text(size=15,face="bold"))

ggplot(D,aes(x=TS,y=PRECTOTCORR))+xlab("Surface Temperature in C")+ylab("PRECIPITATION (mm)")+ggtitle("Surface Temperature in C VS PRECIPITATION")+stat_spline(size=1,color="blue")+theme(plot.title=element_text(size=15,face="bold"))

ggplot(D,aes(x=T2MDEW,y=PRECTOTCORR))+xlab("Dew Point at 2 M")+ylab("PRECIPITATION (mm)")+ggtitle("Dewpoint at 2 M VS PRECIPITATION")+stat_spline(size=1,color="green")+theme(plot.title=element_text(size=15,face="bold"))

ggplot(D,aes(x=PS,y=PRECTOTCORR))+xlab("Surface Pressure (kPa)")+ylab("PRECIPITATION (mm)")+ggtitle("Surface Pressure in kPa VS PRECIPITATION")+stat_spline(size=1,color="orange")+theme(plot.title=element_text(size=15,face="bold"))

ggplot(D,aes(x=WS50M,y=PRECTOTCORR))+xlab("Wind Speed at 50 M")+ylab("PRECIPITATION (mm)")+ggtitle("Wind Speed at 50 M VS PRECIPITATION")+stat_spline(size=1,color="purple")+theme(plot.title=element_text(size=15,face="bold"))

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

D=read.csv("C:\\Users\\Admin\\Desktop\\poly.csv")
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


D=read.csv("C:\\Users\\Admin\\Desktop\\S1.csv",header=TRUE,row.names=1)
D
attach(D)
names(D)

d=c(D$X2016,D$X2017,D$X2018,D$X2019,D$X2020)

D=ts(d,start=c(2016,1),frequency=12)
D


kpss.test(D)
auto.arima(D)


trend=ma(D,order=12)

season=decompose(D)
s=season$figure
s

T=D-trend-s
T

kpss.test(T)
plot.ts(T)

T1=diff(T,difference=12)
kpss.test(T1)
plot.ts(T1)

A=auto.arima(T1)
A

D=read.csv("C:\\Users\\Admin\\Desktop\\S1.csv",header=TRUE,row.names=1)
attach(D)

d=c(D$X2021,D$X2022)

D=ts(d,start=c(2021,1),frequency=12)
D

f=forecast(A,h=24)
f
plot.ts(f)

plot(f)

names(f)

x=f$mean
x

xhat=x[3:24]


rmse=sqrt(sum((d-x)^2)/length(d))
rmse


## optional
D=read.csv("C:\\Users\\Admin\\Desktop\\S1.csv",header=TRUE,row.names=1)
D
attach(D)
names(D)

d=c(D$X2016,D$X2017,D$X2018,D$X2019,D$X2020)

D=ts(d,start=c(2016,1),frequency=12)
D


kpss.test(D)
A1=auto.arima(D)
A1
f1=forecast(A1,h=24)
plot.ts(f1)
plot(f1)

accuracy(f1)

C=table()


x=f1$mean
x
D=read.csv("C:\\Users\\Admin\\Desktop\\S1.csv",header=TRUE,row.names=1)
d=c(D$X2021,D$X2022)
rmse=sqrt(sum((d-x)^2)/length(d))
rmse

plot(PRECTOTCORR)




D=ts(D,start=c(2016,1),frequency=12)
D
dim(D)
D

kpss.test(D)



autoplot(D) + ylab("Rainfall (mm2)") + xlab("Datetime") + 
  scale_x_date(date_labels = '%b - %Y', breaks = '1 year', minor_breaks = '2 month') +
  theme_bw() + ggtitle("Solapur Rainfall 2016 - 2022")


Y=PRECTOTCORR
Y=ts(Y,start=c(2016))

plot.ts(Y)
kpss.test(Y)

t=ma(Y,order=1)

season=decompose(Y)

auto.arima(Y)


## Forecasting using different methods

D=read.csv("C:\\Users\\Admin\\Desktop\\S1.csv",header=TRUE,row.names=1)
attach(D)
names(D)
d=c(D$X2016,D$X2017,D$X2018,D$X2019,D$X2020,D$X2021,D$X2022)

D=ts(d,start=c(2016,1),frequency=12)
D

plot.ts(D)

boxplot(split(D,cycle(D)),names=month.abb,col="gold")

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
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("testData","ARIMAPred"))

AR.mean =forecast(arimaMod,h=24)$mean

plot(testData, main="", ylab="Precipitation in mm", xlab="Months", col="darkblue")  
lines(AR.mean,col="red")

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
G=3534.6-1219.5
G
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


D=read.csv("C:\\Users\\Admin\\Desktop\\Solapur.csv")
dim(D)
D=D[,4:17]
names(D)
attach(D)
D$Date=dmy(D$Date)
y=sqrt(sqrt(D$PRECTOTCORR))
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

ggplot(D1,aes(x=x,y=y))+stat_spline(color="red")+stat_spline(aes(x=x,y=pred_rf),D1,color="blue")+theme(plot.title=element_text(size=15,face="bold"))+labs(title="Actual VS Predicted Plot of Rainfall")+xlab("Years")+ylab("Preipitation")



#### STOCHASTIC OBJECTIVE
library(expm)

year2016=matrix(c(171,15,15,165),nrow=2,ncol=2,byrow=TRUE)
y2016=year2016/rowSums(year2016)
y2016
n=100
result = y2016 %^% n
print(result)


year2017=matrix(c(175,14,14,162),nrow=2,ncol=2,byrow=TRUE)
y2017=year2017/rowSums(year2017)
y2017
n=100
result = y2017 %^% n
print(result)


year2018=matrix(c(163,20,20,162),nrow=2,ncol=2,byrow=T)
y2018=year2018/rowSums(year2018)
y2018
n=100
result = y2018 %^% n
print(result)

year2019=matrix(c(165,15,15,170),nrow=2,ncol=2,byrow=T)
y2019=year2019/rowSums(year2019)
y2019
n=100
result = y2019 %^% n
print(result)

year2020=matrix(c(134,16,17,199),nrow=2,ncol=2,byrow=T)
y2020=year2020/rowSums(year2020)
y2020
n=100
result = y2020 %^% n
print(result)

year2021=matrix(c(126,15,14,210),nrow=2,ncol=2,byrow=T)
y2021=year2021/rowSums(year2021)
y2021
n=100
result = y2021 %^% n
print(result)

year2022=matrix(c(147,18,18,182),nrow=2,ncol=2,byrow=T)
y2022=year2022/rowSums(year2022)
y2022
n=100
result = y2022 %^% n
print(result)





















