swirl_1 <- function() {
    plot(child~parent, galton)
    plot(jitter(child,4) ~parent, galton)
    regrline <- lm(child~parent, galton)
    abline(regrline, lwd=3, col="red")
    
}

swirl_residue <- function() {
    fit <- lm(child~parent, galton)
    # there are 2 assumptions about residue: 1. mean(residue)= 0, 
    # 2. correlation between residue and the predicator is 0
    mean(fit$residue)
    
    ols.ic <- fit$coef[1]
    ols.slope <- fit$coef[2]
    # sqe(ols.slope+sl, ols.intercept+ic) is equal to 
    # sqe(ols.slope, ols.intercept) + sum(est(sl,ic)^2)
    
    # variance of child = var of OSL and variance of residuals
    varChild <- var(galton$child)
    varRes <- var(fit$residuals)
    varEst <- var(est(ols.slope, ols.ic))
    all.equal(varChild, sum(varRes, varEst))
    # therefore, the variance if estimated is always less than the variance of
    # original data
    
    efit <- lm(accel ~mag+dist, attenu)
    mean(efit$residuals)
    cov(efit$residuals, attenu$mag)
    # the correlation between residuals and the predictors are zero
}

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
#manipulate(myPlot(beta), beta = slider(0.4, .8, step = 0.02))

#plot the original Galton data points with larger dots for more freq pts
y <- galton$child
x <- galton$parent
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
plot(as.numeric(as.vector(freqData$parent)), 
     as.numeric(as.vector(freqData$child)), 
     pch = 21, col = "black", bg = "lightblue",
     cex = .07 * freqData$freq, xlab = "parent", ylab = "child")

#original regression line, children as outcome, parents as predictor
abline(mean(y) - mean(x) * cor(y, x) * sd(y) / sd(x), #intercept
       sd(y) / sd(x) * cor(y, x),  #slope
       lwd = 3, col = "red")

#new regression line, parents as outcome, children as predictor
abline(mean(y) - mean(x) * sd(y) / sd(x) / cor(y, x), #intercept
       sd(y) / cor(y, x) / sd(x), #slope
       lwd = 3, col = "blue")

#assume correlation is 1 so slope is ratio of std deviations
abline(mean(y) - mean(x) * sd(y) / sd(x), #intercept
       sd(y) / sd(x),  #slope
       lwd = 2)
points(mean(x), mean(y), cex = 2, pch = 19) #big point of intersection

swirl_lse <- function() {
    l_nor <- lm(gch_nor~gpa_nor)
    
}