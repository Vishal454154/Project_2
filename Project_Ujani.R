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




















