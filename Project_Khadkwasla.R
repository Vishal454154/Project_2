library(lubridate)
library(xts)
library(olsrr)


D=read.csv("C:\\Users\\Admin\\Desktop\\Solapur.csv")
dim(D)
D=D[,4:17]

names(D)
attach(D)
D$Date=dmy(D$Date)
str(D)
class(D)

plot(D$Date,D$PRECTOTCORR,type="l",col="red",main="Plot of Rainfall",xlab="Year",ylab="Precipitation in mm/day")

S=summary(D)
S


C=cor(D[2:13])
C


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
