####Quiz 2
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit<-lm(y~x)
summary(fit)

##Q3
data("mtcars")

fit<-lm(mpg~wt,data=mtcars)
p1<-predict(fit,newdata=data.frame(wt=mean(mtcars$wt)),interval="prediction")
p1

p2<-predict(fit,newdata=data.frame(wt=3),interval="prediction")
p2
