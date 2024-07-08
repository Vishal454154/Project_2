library(lubridate)
library(xts)
library(olsrr)
library(ggplot2)
library(dplyr)
library(TSA)
library(tseries)
library(forecast)


D=read.csv("C:\\Users\\Admin\\Desktop\\Solapur.csv")
dim(D)
D=D[,4:17]

names(D)
attach(D)
D$Date=dmy(D$Date)
str(D)
class(D)


plot.ts(PRECTOTCORR[1:365])
kpss.test(PRECTOTCORR[1:365])

# BASIC PLOTTING
par(mfrow=c(5,1))
ggplot(D,aes(x=Date,y=ALLSKY_SFC_PAR_TOT))+geom_point()+geom_smooth()
ggplot(D,aes(x=Date,y=TS))+geom_point()
ggplot(D,aes(x=Date,y=T2MDEW))+geom_point()
ggplot(D,aes(x=Date,y=PS))+geom_point()
ggplot(D,aes(x=Date,y=WS50M))+geom_point()


spline.d=(spline(D$Date,D$ALLSKY_SFC_PAR_TOT))
attach(spline.d)
names(spline.d)
ggplot(D)+geom_point(aes(x=D$Date,y=D$ALLSKY_SFC_PAR_TOT))+stat_smooth()



S=summary(D)
S

C=cor(D[2:13])
C

# Multiple Linear Regression

MLRM=lm(PRECTOTCORR~.,data=D)
MLRM
summary(MLRM)
ols_vif_tol(MLRM)
par(mfrow=c(2,2))
plot(MLRM)


MLRM2=lm(PRECTOTCORR~ ALLSKY_SFC_PAR_TOT+TS+T2MDEW+PS+WS50M,data=D)
MLRM2
summary(MLRM2)
ols_vif_tol(MLRM2)
par(mfrow=c(2,2))
plot(MLRM2)

### Polynomial Regression

D=read.csv("C:\\Users\\Admin\\Desktop\\poly.csv")
names(D)
attach(D)
D

NLRM=lm(PRECTOTCORR~poly(ALLSKY_SFC_PAR_TOT,2)+poly(TS,2)+poly(T2MDEW,2)+poly(PS,2)+poly(WS50M,2),data=D)
NLRM
summary(NLRM)
ols_vif_tol(NLRM)
par(mfrow=c(2,2))
plot(NLRM)

NLRM2=lm(PRECTOTCORR~poly(ALLSKY_SFC_PAR_TOT,3)+poly(TS,3)+poly(T2MDEW,3)+poly(PS,3)+poly(WS50M,3),data=D)
NLRM2
summary(NLRM2)
ols_vif_tol(NLRM2)
par(mfrow=c(2,2))
plot(NLRM2)

NLRM5=lm(PRECTOTCORR~poly(ALLSKY_SFC_PAR_TOT,5)+poly(TS,5)+poly(T2MDEW,5)+poly(PS,5)+poly(WS50M,5),data=D)
NLRM5
summary(NLRM5)
ols_vif_tol(NLRM5)
par(mfrow=c(2,2))
plot(NLRM5)

names(D)


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






arimax(D[,1:5],order)









model=glm(TPM~ ALLSKY_SFC_PAR_TOT+TS+T2MDEW+PS+WS50M,data=D,family="binomial")
model
summary(model)
G=3534.6-1219.5
G
qchisq(0.05,5)

par(mfrow=c(2,2))
plot(model)




x=c(TPM)
View(x)

M=as.matrix(table(x[-length(x)],x[-1]))
M
RS=apply(M,1,sum)
RS
TPM=matrix(nrow=2,ncol=2)
for(i in 1:2)
{
for(j in 1:2)
{
TPM[i,j]=M[i,j]/RS[i]
}
}
TPM

D_ts=xts(D[2:13],D$Date)
class(D_ts)
View(D_ts)
names(D_ts)
ts.plot(D_ts$PRECTOTCORR,xlab=row.names(D_ts))

D1=data.frame(TS,T2MDEW,RH2M,PS,WS50M,PRECTOTCORR)
View(D1)


C=cor(D)
C
summary(C)
plot(D)
x=as.matrix(D[2:12])

det(t(x)%*%x)
in
install.packages("vars")




















