##'Linear Least Squares'

##prep
library(UsingR)
data(galton)
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
plot(as.numeric(as.vector(freqData$parent)), 
     as.numeric(as.vector(freqData$child)),
     pch = 21, col = "black", bg = "lightblue",
     cex = .05 * freqData$freq, 
     xlab = "parent", ylab = "child")

##Please look at lecture notes 01_03 for detailed derivation
##Y=b0+b1*X
##b1=Cor(Y,X)*Sd(Y)/Sd(X)
##b0=Y-b1*X


##normalization does not affect slope and correlation
y <- galton$child
x <- galton$parent
beta1 <- cor(y, x) *  sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
rbind(c(beta0, beta1), coef(lm(y ~ x)))
yn <- (y - mean(y))/sd(y)
xn <- (x - mean(x))/sd(x)
c(cor(y, x), cor(yn, xn), coef(lm(yn ~ xn))[2])

freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
plot(as.numeric(as.vector(freqData$parent)), 
     as.numeric(as.vector(freqData$child)),
     pch = 21, col = "black", bg = "lightblue",
     cex = .05 * freqData$freq, 
     xlab = "parent", ylab = "child", xlim = c(62, 74), ylim = c(62, 74))
abline(mean(y) - mean(x) * cor(y, x) * sd(y) / sd(x), sd(y) / sd(x) * cor(y, x), lwd = 3, col = "red")
abline(mean(y) - mean(x) * sd(y) / sd(x) / cor(y, x), sd(y) / sd(x) / cor(y, x), lwd = 3, col = "blue")
abline(mean(y) - mean(x) * sd(y) / sd(x), sd(y) / sd(x), lwd = 2)
points(mean(x), mean(y), cex = 2, pch = 19)