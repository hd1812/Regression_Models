##Lecture Note
##Introduction

##Prep
library(UsingR)
data(galton);
library(reshape)

##plog histograms of child and parents heights
long<-melt(galton)
g<-ggplot(long,aes(x=value,fill=variable))
g<-g+geom_histogram(colour="black",binwidth=1)
g<-g+facet_grid(.~variable)
g

##comparing children's heights and parents
ggplot(galton,aes(x=parent,y=child))+geom_point()

##Rstudio code, compare child and parents height
myPlot <- function(beta){
  y <- galton$child - mean(galton$child)
  x <- galton$parent - mean(galton$parent)
  freqData <- as.data.frame(table(x, y))
  names(freqData) <- c("child", "parent", "freq")
  plot(
    as.numeric(as.vector(freqData$parent)), 
    as.numeric(as.vector(freqData$child)),
    pch = 21, col = "black", bg = "lightblue",
    cex = .15 * freqData$freq, 
    xlab = "parent", 
    ylab = "child"
    )
  abline(0, beta, lwd = 3)
  points(0, 0, cex = 2, pch = 19)
  mse <- mean( (y - beta * x)^2 )
  title(paste("beta = ", beta, "mse = ", round(mse, 3)))
}
manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02))

##Using 'lm' to fit linear model
lm(I(child - mean(child))~ I(parent - mean(parent)) - 1, data = galton)

