## This empties the work space.
rm(list=ls())
library(ggplot2)
## change directory
## setwd("/path/to/work_dir")
setwd("~/Travail/DTU/Time_series/Ass2")

################Exercise 1
# Simulate 10 realisations with 200 observations from the process and plot then 
N<-10
n<-200
sd<-0.1
col<-rainbow(N)

par(mfrow=c(1,1))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))
plot(1, type="n", ylab='Xt',xlab='Time', xlim=c(-7, 200), ylim=c(-0.71, 0.75))

Y<-c()
for(i in 1:N) {
  Y<-rbind(Y,arima.sim(model = list(ma=c(1,1,1), order=c(0,0,3)), n = n, sd=sd))
  lines(1:n,Y[i,1:n],type = 'o', pch = 16, col=col[i], lwd=0.5,cex=0.7)
}
legend('topleft',legend = c("1","2","3","4","5","6","7","8","9","10"), 
       col = col, pch = c(16,16,16,16,16,16,16,16,16,16),
       cex=0.7, bty = 'n')


# Estimate the ACF for each realisation and plot those 
acf <- c()

for(i in 1:N) {
  acf <- rbind(acf,acf(Y[i,1:n],plot = FALSE)$acf)
}

par(mfrow=c(1,1))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))
plot(1, type="n", ylab='ACF',xlab='Lag', xlim=c(0, 23), ylim=c(min(acf),max(acf)))

for (i in 2:N) { 
  lines(0:23,acf[i,1:24],type='o',cex=0.6, bty='l', 
        col=col[i],lwd=0.5)
}
legend('topright',legend = c("1","2","3","4","5","6","7","8","9","10"), 
       col = col, pch = c('o','o','o','o','o','o','o','o','o','o'),
       lty = c(1,1,1,1,1,1,1,1,1,1), cex=0.7, bty = 'n')

# Estimate the PACF for each realisation and plot those 
pacf <- c()

for(i in 1:N) {
  pacf <- rbind(pacf,pacf(Y[i,1:n],plot = FALSE)$acf)
}

par(mfrow=c(1,1))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))
plot(1, type="n", ylab='PACF',xlab='Lag', xlim=c(0, 22), ylim=c(min(pacf),max(pacf)))

for (i in 2:N) { 
  lines(0:22,pacf[i,1:23],type ='o',cex=0.6, bty='l', col=col[i],lwd=0.5)
}
legend('topright',legend = c("1","2","3","4","5","6","7","8","9","10"), 
       col = col, pch = c('o','o','o','o','o','o','o','o','o','o'),
       lty = c(1,1,1,1,1,1,1,1,1,1), cex=0.7, bty = 'n')

#Estimate the variance of the realisations
var_hat <- c()

for(i in 1:N) {
  var_hat <- c(var_hat,acf(Y[i,1:n], type='covariance', plot = FALSE)$acf[1])
}

par(mfrow=c(1,1))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))
plot(1, type="n", xlab="Realisation", ylab="Estimation of the variance",
     xlim=c(0, 10), ylim=c(0.00, max(var_hat)))
for (i in 1:N) { 
  points(i,var_hat[i],type = 'p',cex=0.6,pch='O', bty='l', col=col[i],lwd=0.5)
}
lines(1:10,rep(0.04,10),lty=2,col='black')
legend('topleft',legend = c('True value',"1","2","3","4","5","6","7","8","9","10"), 
       col = c('black',col), pch = c(NA,16,16,16,16,16,16,16,16,16,16),
       cex=0.7, lty = c(2,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA), bty = 'n')

#################Exercice 2
#Study of the model
Time<-c(1:15)
Temp<-c(46.6,49.5,60.3,59.2,59.5,61.9,59.7,60.1,57.8,49.7,49.7,50.1,48.6,54.5,62.3)

data<-rbind(Time,Temp)

mean<-55
sd<- 0.5
a1<- 0.5
a2<- -0.3
a4<- 0.9
a5<- -0.45
a6<- 0.27

Model <- function(Y1,Y2,Y12,Y13,Y15,et){
  mean+a1*(Y1-mean)+a2*(Y2-mean)+a4*(Y12-mean)+a5*(Y13-mean)+a6*(Y14-mean)+et
}

#Predictions
Y<-rbind(c(Time,c(16,17)),c(Temp,NA,NA))

for(t in 16:17)
{
  Y1 <- Y[2,Y[1,]==(t-1)]
  Y2 <- Y[2,Y[1,]==(t-2)]
  Y12 <- Y[2,Y[1,]==(t-12)]
  Y13 <- Y[2,Y[1,]==(t-13)]
  Y14 <- Y[2,Y[1,]==(t-14)]
  et <- 0
  Y[2,Y[1,]==t]<- Model(Y1,Y2,Y12,Y13,Y14,et)
}

sd16=sd
sd17=sqrt((1+0.5^2))*sd

t95_16<-qnorm(c(0.025,0.975))*sd16
t95_17<-qnorm(c(0.025,0.975))*sd17

Yint_95<-rbind(Y[2,16]+t95_16,Y[2,17]+t95_17)

#plot of the results
par(mfrow=c(1,1))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))
plot(Time,Temp, xlim=c(1,17),ylim=c(45,65), xaxt="n", type = 'o',lwd=1.5,lty=2,ann=FALSE,bty='l',pch=4,cex=0.6,
     cex.axis = 0.7, mgp=c(3,0.5,0))
# Modification de l'axe des x
xtick<-c(2016.09,'','','',2017.01,'','','',2017.05,'','','',2017.09,'','')
axis(side=1, at=xtick)
mtext(xtick,side=1,at=Time,col='black')
mtext(x=xtick, labels = xtick)
mtext(side = 1,text = 'Year, Month', line = 1.5, cex = 0.78)
mtext(side = 2,text = 'Temp', line = 1.9, cex = 0.78)
points(c(16,17),Y[2,16:17],pch=4,cex=0.6,col='red')
points(c(16,17),Yint_95[,1],pch=16,col='darkorange2')
points(c(16,17),Yint_95[,2],pch=16,col='darkorange2')
legend('bottomright',legend = c("Observations", "Estimations", '95% prediction interval'),
       lty = c(2,NA,NA),col = c("black","red",'darkorange2'), pch = c(4,4,16,16),cex=0.7)

Results=data.frame(Observation = c('Y2017M12','Y2018M01'), 
                  Prediction=c(Y[2,16],Y[2,17]),Sd = c(sd16,sd17), 
                  Lower_bound = c(Yint_95[1],Yint_95[2]),
                  Upper_bound = c(Yint_95[3],Yint_95[4]))
#################Exercice 3

#Defintion of the white noise
mean<-0
sd<-1
N<-300
eps<-rnorm(N, mean = mean, sd=sd)
lag=40
s<-12

#AR(1)
Y<-vector(length = N) 
Y[1]<-eps[1] 
phi1<--0.85
for (t in 2:N) {
  Y[t]<-eps[t]-phi1*Y[t-1] 
}
par(mfrow=c(1,1))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))
plot(x = 1:N, y = Y, type = 'l', cex.axis = 0.7, lwd = 0.7, xlab='Time', ylab='Y')
lines(X,col='red')
par(mfrow=c(1,2))
acf(x = Y, lag = lag, type = 'correlation',ann=FALSE, lwd=2, main ='', xlab='lag'
    , ylab='ACF')
lines(x = c(12,12), y = c(-10,10), type = 'l', lty = 2,lwd=1.4, col='darkorange2')
lines(x = c(24,24), y = c(-10,10), type = 'l', lty = 2,lwd=1.4, col='darkorange2')
lines(x = c(36,36), y = c(-10,10), type = 'l', lty = 2,lwd=1.4, col='darkorange2')
mtext(side = 1,text = 'lag', line = 2, cex = 1.3)
mtext(side = 2,text = 'ACF', line = 2, cex = 1)

pacf(x = Y, lag = lag, main = '', ann=FALSE, lwd=2,xlab='lag',ylab='PACF')
lines(x = c(12,12), y = c(-10,10), type = 'l', lty = 2,lwd=1.5, col='darkorange2')
lines(x = c(24,24), y = c(-10,10), type = 'l', lty = 2,lwd=1.5, col='darkorange2')
lines(x = c(36,36), y = c(-10,10), type = 'l', lty = 2,lwd=1.5, col='darkorange2')
mtext(side = 1,text = 'lag', line = 2, cex = 1.3)
mtext(side = 2,text = 'PACF', line = 2, cex = 1)

#AR(1)s=12
Y<-vector(length = N)
Y[1:s]<-eps[1:s]
psi1<-0.85

for (t in (s+1):N)
{
  Y[t]<-eps[t]-psi1*Y[t-s]
}

par(mfrow=c(1,1))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))
plot(x = 1:N, y = Y, type = 'l', cex.axis = 0.7, lwd = 0.7, xlab='Time', ylab='Y')

par(mfrow=c(1,2))
acf(x = Y, lag = lag, type = 'correlation',ann=FALSE, lwd=2, main ='', xlab='lag'
    , ylab='ACF')
lines(x = c(12,12), y = c(-10,10), type = 'l', lty = 2,lwd=1.4, col='darkorange2')
lines(x = c(24,24), y = c(-10,10), type = 'l', lty = 2,lwd=1.4, col='darkorange2')
lines(x = c(36,36), y = c(-10,10), type = 'l', lty = 2,lwd=1.4, col='darkorange2')
mtext(side = 1,text = 'lag', line = 2, cex = 1.3)
mtext(side = 2,text = 'ACF', line = 2, cex = 1)

pacf(x = Y, lag = lag, main = '', ann=FALSE, lwd=2,xlab='lag',ylab='PACF')
lines(x = c(12,12), y = c(-10,10), type = 'l', lty = 2,lwd=1.5, col='darkorange2')
lines(x = c(24,24), y = c(-10,10), type = 'l', lty = 2,lwd=1.5, col='darkorange2')
lines(x = c(36,36), y = c(-10,10), type = 'l', lty = 2,lwd=1.5, col='darkorange2')
mtext(side = 1,text = 'lag', line = 2, cex = 1.3)
mtext(side = 2,text = 'PACF', line = 2, cex = 1)

#ARMA(1,0)x(0,1)s=12
Y<-vector(length = N)
Y[1:s]<-eps[1:s]
phi1<- -0.8
Theta1<-0.9

for (t in (s+1):N)
{
  Y[t]<- -phi1*Y[t-1] + eps[t] + Theta1*eps[t-s]
}

par(mfrow=c(1,1))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))
plot(x = 1:N, y = Y, type = 'l', cex.axis = 0.7, lwd = 0.7, xlab='Time', ylab='Y')

par(mfrow=c(1,2))
acf(x = Y, lag = lag, type = 'correlation',ann=FALSE, lwd=2, main ='', xlab='lag'
    , ylab='ACF')
lines(x = c(12,12), y = c(-10,10), type = 'l', lty = 2,lwd=1.4, col='darkorange2')
lines(x = c(24,24), y = c(-10,10), type = 'l', lty = 2,lwd=1.4, col='darkorange2')
lines(x = c(36,36), y = c(-10,10), type = 'l', lty = 2,lwd=1.4, col='darkorange2')
mtext(side = 1,text = 'lag', line = 2, cex = 1.3)
mtext(side = 2,text = 'ACF', line = 2, cex = 1)

pacf(x = Y, lag = lag, main = '', ann=FALSE, lwd=2,xlab='lag',ylab='PACF')
lines(x = c(12,12), y = c(-10,10), type = 'l', lty = 2,lwd=1.5, col='darkorange2')
lines(x = c(24,24), y = c(-10,10), type = 'l', lty = 2,lwd=1.5, col='darkorange2')
lines(x = c(36,36), y = c(-10,10), type = 'l', lty = 2,lwd=1.5, col='darkorange2')
mtext(side = 1,text = 'lag', line = 2, cex = 1.3)
mtext(side = 2,text = 'PACF', line = 2, cex = 1)

#ARMA(1,0)x(1,0)s=12
Y<-vector(length = N)
Y[1:(s+1)]<-eps[1:(s+1)]
phi1<-0.7
psi1<-0.8

for (t in 14:N)
{
  Y[t]<- -phi1*Y[t-1]-psi1*Y[t-s]-phi1*psi1*Y[t-s-1]+eps[t]
}

par(mfrow=c(1,1))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))
plot(x = 1:N, y = Y, type = 'l', cex.axis = 0.7, lwd = 0.7, xlab='Time', ylab='Y')

par(mfrow=c(1,2))
acf(x = Y, lag = lag, type = 'correlation',ann=FALSE, lwd=2, main ='', xlab='lag'
    , ylab='ACF')
lines(x = c(12,12), y = c(-10,10), type = 'l', lty = 2,lwd=1.4, col='darkorange2')
lines(x = c(24,24), y = c(-10,10), type = 'l', lty = 2,lwd=1.4, col='darkorange2')
lines(x = c(36,36), y = c(-10,10), type = 'l', lty = 2,lwd=1.4, col='darkorange2')
mtext(side = 1,text = 'lag', line = 2, cex = 1.3)
mtext(side = 2,text = 'ACF', line = 2, cex = 1)

pacf(x = Y, lag = lag, main = '', ann=FALSE, lwd=2,xlab='lag',ylab='PACF')
lines(x = c(12,12), y = c(-10,10), type = 'l', lty = 2,lwd=1.5, col='darkorange2')
lines(x = c(24,24), y = c(-10,10), type = 'l', lty = 2,lwd=1.5, col='darkorange2')
lines(x = c(36,36), y = c(-10,10), type = 'l', lty = 2,lwd=1.5, col='darkorange2')
mtext(side = 1,text = 'lag', line = 2, cex = 1.3)
mtext(side = 2,text = 'PACF', line = 2, cex = 1)

#ARMA(2,0)x(1,0)s=12
Y<-vector(length = N)
Y[1:(s+2)]<-eps[1:(s+2)]
phi1<-0.6
phi2<- -0.3
psi1<-0.8

for (t in 14:N)
{
  Y[t]<- -phi1*Y[t-1]-psi1*Y[t-s]-phi1*psi1*Y[t-s-1]-phi2*Y[t-2]
  -phi2*psi1*Y[t-s-2]+eps[t]
}

par(mfrow=c(1,1))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))
plot(x = 1:N, y = Y, type = 'l', cex.axis = 0.7, lwd = 0.7, xlab='Time', ylab='Y')

par(mfrow=c(1,2))
acf(x = Y, lag = lag, type = 'correlation',ann=FALSE, lwd=2, main ='', xlab='lag'
    , ylab='ACF')
lines(x = c(12,12), y = c(-10,10), type = 'l', lty = 2,lwd=1.4, col='darkorange2')
lines(x = c(24,24), y = c(-10,10), type = 'l', lty = 2,lwd=1.4, col='darkorange2')
lines(x = c(36,36), y = c(-10,10), type = 'l', lty = 2,lwd=1.4, col='darkorange2')
mtext(side = 1,text = 'lag', line = 2, cex = 1.3)
mtext(side = 2,text = 'ACF', line = 2, cex = 1)

pacf(x = Y, lag = lag, main = '', ann=FALSE, lwd=2,xlab='lag',ylab='PACF')
lines(x = c(12,12), y = c(-10,10), type = 'l', lty = 2,lwd=1.5, col='darkorange2')
lines(x = c(24,24), y = c(-10,10), type = 'l', lty = 2,lwd=1.5, col='darkorange2')
lines(x = c(36,36), y = c(-10,10), type = 'l', lty = 2,lwd=1.5, col='darkorange2')
mtext(side = 1,text = 'lag', line = 2, cex = 1.3)
mtext(side = 2,text = 'PACF', line = 2, cex = 1)

#ARMA(0,1)x(0,1)s=12
Y<-vector(length = N)
Y[1:13]<-eps[1:13]
theta1<- -0.4
Theta1<-0.8

for (t in 14:N)
{
  Y[t]<- eps[t] + theta1*eps[t-1]+Theta1*eps[t-12]+theta1*Theta1*eps[t-13]
}

par(mfrow=c(1,1))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))
plot(x = 1:N, y = Y, type = 'l', cex.axis = 0.7, lwd = 0.7, xlab='Time', ylab='Y')

par(mfrow=c(1,2))
acf(x = Y, lag = lag, type = 'correlation',ann=FALSE, lwd=2, main ='', xlab='lag'
    , ylab='ACF')
lines(x = c(12,12), y = c(-10,10), type = 'l', lty = 2,lwd=1.4, col='darkorange2')
lines(x = c(24,24), y = c(-10,10), type = 'l', lty = 2,lwd=1.4, col='darkorange2')
lines(x = c(36,36), y = c(-10,10), type = 'l', lty = 2,lwd=1.4, col='darkorange2')
mtext(side = 1,text = 'lag', line = 2, cex = 1.3)
mtext(side = 2,text = 'ACF', line = 2, cex = 1)

pacf(x = Y, lag = lag, main = '', ann=FALSE, lwd=2,xlab='lag',ylab='PACF')
lines(x = c(12,12), y = c(-10,10), type = 'l', lty = 2,lwd=1.5, col='darkorange2')
lines(x = c(24,24), y = c(-10,10), type = 'l', lty = 2,lwd=1.5, col='darkorange2')
lines(x = c(36,36), y = c(-10,10), type = 'l', lty = 2,lwd=1.5, col='darkorange2')
mtext(side = 1,text = 'lag', line = 2, cex = 1.3)
mtext(side = 2,text = 'PACF', line = 2, cex = 1)