####Residual

##Definition--distance from observed value to prediction
##Useful for diagnosis

##Properties
##E[e_i] = 0.
##If an intercept is included, $\sum_{i=1}^n e_i = 0$
##If a regressor variable, $X_i$, is included in the model $\sum_{i=1}^n e_i X_i = 0$.
##Residuals are useful for investigating poor model fit.
##Positive residuals are above the line, negative residuals are below.
##Residuals can be thought of as the outcome ($Y$) with the linear association of the predictor ($X$) removed.
##One differentiates residual variation (variation after removing the predictor) from systematic variation (variation explained by the regression model).
##Residual plots highlight poor model fit

##Example
data(diamond)
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)
##Find Residue
e <- resid(fit)
##Find predicted value for observed inputs
yhat <- predict(fit)
max(abs(e -(y - yhat)))##Find the max residue
max(abs(e - (y - coef(fit)[1] - coef(fit)[2] * x)))
sum(e)##Residues sum to zero

##Plot data
plot(diamond$carat, e,  
     xlab = "Mass (carats)", 
     ylab = "Residuals (SIN $)", 
     bg = "lightblue", 
     col = "black", cex = 1.1, pch = 21,frame = FALSE)
abline(h = 0, lwd = 2)
for (i in 1 : n) 
  lines(c(x[i], x[i]), c(e[i], 0), col = "red" , lwd = 2)

##Non-linear data
x <- runif(100, -3, 3); 
y <- x + sin(x) + rnorm(100, sd = .2); 
plot(x, y); abline(lm(y ~ x))
##Residue plot
plot(x, resid(lm(y ~ x))); 
abline(h = 0)

y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)
summary(fit)$sigma
sqrt(sum(resid(fit)^2) / (n - 2))
