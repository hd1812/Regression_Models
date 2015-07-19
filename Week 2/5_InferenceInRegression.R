####Inference in Regression

##Review
##Model: Yi=b0+b1Xi+ei

##Null hypothesis assumes no relation ship between the measured phenomena
##Rejecting the null hypothesis concludes that there is relationship

##p-value -- see wiki
##t-statistic--the departure of an estimated parameter from its notional value and its standard error. 

##Statistics (datahat-data)/standard_error often have the following properties
##1:normally distributed and has a finite sample Student's T distribution 
##if estimated variance is replaced with a sample estimate
##2:Can be used to test and compare different inputs
##3:Can be used to create confidence interval

##Look at 01_07 for mathematical drviation 
##on standard error os b0 and b1

##Diamond Example
##The following shows how lm() is implemented in R
library(UsingR);data(diamond)
y<-diamond$price;x<-diamond$carat;n<-length(y)
##Find coefficiens
beta1<-cor(y,x)*sd(y)/sd(x)
beta0<-mean(y)-beta1*mean(x)
##Find residue
e<-y-beta0-beta1*x
##Divide by n-2 to find sigma
sigma<-sqrt(sum(e^2)/(n-2))
ssx<-sum((x-mean(x))^2)
##Standard error
seBeta0<-(1/n+mean(x)^2/ssx)^.5*sigma
seBeta1<-sigma/sqrt(ssx)

tBeta0<-beta0/seBeta0;tBeta1<-beta1/seBeta1
pBeta0<-2*pt(abs(tBeta0),df=n-2,lower.tail=FALSE)
pBeta1<-2*pt(abs(tBeta1),df=n-2,lower.tail=FALSE)
coefTable<-rbind(c(beta0,seBeta0,tBeta0,pBeta0),c(beta1,seBeta1,tBeta1,pBeta1))
colnames(coefTable)<-c("Estimate","Std.Error","tvalue","P(>|t|)")
rownames(coefTable)<-c("(Intercept)","x")
coefTable
##Simple way...
fit <- lm(y ~ x); 
summary(fit)$coefficients
summary(fit)

##Getting a confidence interval
sumCoef <- summary(fit)$coefficients
##Find 97.5% quantile of b0 and b1
sumCoef[1,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[1, 2]
sumCoef[2,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[2, 2]
