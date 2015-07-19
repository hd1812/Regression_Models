##Residual variation

##The sum of residual square /n or /(n-2) equal to sigma^2
##Most peiple use n-2 instead of n so E(sigma^2)=sigma^2
##Actually only have n-2 residuals. e.g two points make a line

data(diamond)
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)
e <- resid(fit)
yhat <- predict(fit)
max(abs(e -(y - yhat)))
max(abs(e - (y - coef(fit)[1] - coef(fit)[2] * x)))

##R squared
##Defined as total variability explained by the linear relationship with predictor
##Regression variation(variance)/Total variation

##R^2 is the percentage of variation explained by the regression model.
##0 <= R^2 <=leq 1
##  $R^2$ is the sample correlation squared.
##R^2 can be a misleading summary of model fit.
##Deleting data can inflate R^2.
##(For later.) Adding terms to a regression model always increases R^2.
##Do example(anscombe) to see the following data.
##Basically same mean and variance of X and Y.
##Identical correlations (hence same $R^2$ ).
##Same linear regression relationship.

##Example
require(stats); require(graphics); data(anscombe)
ff <- y ~ x
mods <- setNames(as.list(1:4), paste0("lm", 1:4))
for(i in 1:4) {
  ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
  ## or   ff[[2]] <- as.name(paste0("y", i))
  ##      ff[[3]] <- as.name(paste0("x", i))
  mods[[i]] <- lmi <- lm(ff, data = anscombe)
  #print(anova(lmi))
}


## Now, do what you should have done in the first place: PLOTS
op <- par(mfrow = c(2, 2), mar = 0.1+c(4,4,1,1), oma =  c(0, 0, 2, 0))
for(i in 1:4) {
  ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
  plot(ff, data = anscombe, col = "red", pch = 21, bg = "orange", cex = 1.2,
       xlim = c(3, 19), ylim = c(3, 13))
  abline(mods[[i]], col = "blue")
}
mtext("Anscombe's 4 Regression data sets", outer = TRUE, cex = 1.5)
par(op)
