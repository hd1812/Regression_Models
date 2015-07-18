####Week 2 Linear Model for prediction

##    b0_bar+b1_bar*X

##Example -- Diamond Price
##In lecture they use ggplot system
library(UsingR)
data(diamond)
plot(diamond$carat, diamond$price,  
     xlab = "Mass (carats)", 
     ylab = "Price (SIN $)", 
     bg = "lightblue", 
     col = "black", cex = 1.1, pch = 21,frame = FALSE)
abline(lm(price ~ carat, data = diamond), lwd = 2)##lm speficies the linear model

##Print out coefficients
fit <- lm(price ~ carat, data = diamond)
coef(fit)
summary(fit)##Details of print out
##Summary: 3721.02 increase in price for every carat increase in mass of diamond
##-259.63 is expected price for 0 carat diamond

##Getting a more interpretable intercept, which was negative in fit
fit2 <- lm(price ~ I(carat - mean(carat)), data = diamond) 
##I --as is, do arithmatic expression in function
coef(fit2)
##Mean centered data
##500 is the expected value of average carat 
##In fit2, b0 gives the average price

##change scale, unit not is 1/10 of carat
fit3 <- lm(price ~ I(carat * 10), data = diamond)

##Predict diamond price with new diamonds
newx <- c(0.16, 0.27, 0.34)
coef(fit)[1] + coef(fit)[2] * newx
##simplified code takes parameters and new data
predict(fit, newdata = data.frame(carat = newx))
coef(fit3)


