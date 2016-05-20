week2_1 <- function() {
    library(UsingR)
    data(diamond)
    library(ggplot2)
    g = ggplot(diamond, aes(x=carat, y=price),)
    g = g + xlab("Mass (carats)")
    g = g + ylab("Rice(STN$)")
    g = g + geom_point(size=6, colour="black", alpha=0.2)
    g = g + geom_point(size=5, colour="blue", alpha=0.2)
    g = g + geom_smooth(method="lm", colour="black")
    g
    
    fit <- lm(price~carat, data=diamond)
    coef(fit)
    
    fit2 <- lm(price~I(carat-mean(carat)), data=diamond)
    coef(fit2)
    # change the unit as 1/10 carat
    fit3 <- lm(price~I(carat*10), data=diamond)
    coef(fit3)
    # predicting the price of a diamond
    newx <- c(0.16, 0.27, 0.34)
    coef(fit)[1] + coef(fit)[2]*newx
    predict(fit, newdata=data.frame(carat=newx))
    
}

week2_2 <- function() {
    data(diamond)
    y <- diamond$price; x<- diamond$carat; n<-length(y)
    fit <- lm(y~x)
    e <- resid(fit)
    yhat <- predict(fit)
    max(abs(e-(y-yhat)))
    max(abs(e-(y-coeff(fit)[1]-coef(fit)[2]*x)))
    plot(diamond$carat, diamond$price, xlab="Mass(carats)",
         ylab="Price(SIN $)", bg="lightblue",
         col="black",cex=1.1, pch=21, frame=FALSE)
    abline(fit, lwd=2)
    for (i in 1:n)
        lines(c(x[i],x[i]), c(y[i],yhat[i]), col="red",lwd=2)
    
    # about residual
    x = runif(100, -3, 3); y=x+sin(x) +rnorm(100, sd=0.2);
    library(ggplot2)
    g = ggplot(data.frame(x=x,y=y), aes(x=x,y=y))
    g = g + geom_smooth(method="lm", colour="black")
    g = g + geom_point(size=7, colour="black", alpha=0.4)
    g = g+ geom_point(size=5, coour="red", alpha=0.4)
    g
    # now plot the residual to see if we can see the sine pattern
    g = ggplot(data.frame(x=x, y=resid(lm(y~x))), aes(x=x,y=y))
    g = g + geom_hline(yintercept=0, size=2);
    g = g + geom_point(size=7, color="black", alpha=0.4)
    g = g + geom_point(size=5, colour="red", alpha=0.4)
    g = g + xlab("X") + ylab("Residual")
    g
    
    # another example: the line looks great. however, when you plot the
    # residual, you got different thing
    x <- runif(100, 0, 6); y <- x+rnorm(100,mean=0, sd=0.01*x)
    g = ggplot(data.frame(x=x, y=y) aes(x=x, y=y))
    g = g + geom_smooth(method="lm", colour="black")
    g = g + geom_point(size=7, colour="black", alpha=0.4)
    g = g + geom_point(size=5, colour="red", alpha=0.4)
    g
    
    # residual data for diamond data
    diamond$e <- resid(lm(price~carat, data=diamond))
    g = ggplot(diamond, aes(x=carat, y=e))
    g = g + xlab("Mass(carats)")
    g = g + ylab("Residual price (SIN $)")
    g = g + geom_hline(yintercept=0, size=2)
    g = g + geom_point(size=7, colur="black", alpha=0.5)
    g = g + geom_point(size=5, colour="blue", alpha=0.2)
    g
    
    e = c(rsid(lm(price~1, data=diamond)),
          resid(lm(price~carat, data=diamond)))
    fit = factor(c(rep("Itc", nrow(diamond)),
                   rep("Itc, slope", nrow(diamond))))
    g = ggplot(data.frame(e=e, fit=fit), aes(y=e, x=fit, fill=fit))
    g = g + geom_dotplot(binaxis="y", size=2, stackdir="center",binwidth=2)
    g = g + xlab("Fitting approach")
    g = g + ylab("Residual price")
    
}

week2_3 <- function() {
    # residual variance
    y <- diamond$price; x<- diamond$carat; n<- length(y)
    fit <- lm(y~x)
    summary(fit)$sigma
    sqrt(sum(resid(fit)^2)/(n-2))
    
    # R squared is the % of the total variablility that is explained by the
    # linear relationship with the predictor
    # therefore, R squared is the % of variation explained by the regression
    # model
    
    data(anscombe); example(anscombe)
    
}

week2_3_1 <- function() {
    # inference in regression
    library(Using); data(diamond)
    y <- diamond$price; x<- diamond$carat; n<- length(y)
    beta1 <- cor(y,x) *sd(y) /sd(x)
    beta0 <- mean(y) - beta1 * mean(x)
    e <- y - beta0 - beta1 * x
    sigma <- sqrt(sum(e^2)/(n-2))
    ssx <- sum(x- mean(x))^2)
    seBeta0 <- (1/n + mean(x) ^2 /ssx)^.5 * sigma
    seBeta1 <- sigma /sqrt(ssx)
    tBeta0 <- beta0 / seBeta0; tBeata1 <- beta1 /seBeta1
    pBeta0 <- 2 * pt(abs(tBeta0), df=n-2, lower.tail=FALSE)
    pBeta1 <- 2 * pt(abs(tBeta1), df=n-2, lower.tail=FALSE)
    coefTable <- rbind(c(beta0, seBeta0, tBeta0, pBeta0), c(beta1, seBeta1, 
                                                            tBeta1, pBeta1))
    colnames(coefTable) <- c("Estimage", "Std. Error", "t value", "P(>|t|")
    rownames(coefTable) <- c("(Intecept)", "x")
    coefTable
    fit <- lm(y~x);
    sumCoef <- summary(fit)$coefficients
    sumCoef[1,1] + c(-1,1) * qt(.975, df=fit$df) * sumCoef[1,2]
    sumCoef[2,1] + c(-1,1) * qt(.975, df=fit$df) * sumCoef[2,2]

}

week2_prediction <- function() {
    library(ggplot2)
    newx = data.frame(x=seq(min(x), max(x), length=100))
    p1 = data.frame(predict(fit, newdata=newx, interval=("confidence")))
    p2 = data.frame(predict(fit, newdata=newx, interval=("prediction")))
    p1$interval = "confidence"
    p2$interval = "prediction"
    p1$x = newx$x
    p2$x = newx$x
    dat = rbind(p1, p2)
    names(da)[1] = "y"
    
    g = ggplot(dat, aes(x=x, y=y))
    g = g + geom_ribbon(aes(ymin=lwr, ymax=upr, fill=interval), alpha=0.2)
    g = g + geom_line()
}