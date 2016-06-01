swirl_3_multi_var1 <- function() {
    # data InsectSprays
    library(datasets)
    summary(InsectSprays[,2])
    sapply(InsectSprays, class)
    fit <- lm(count~spray, data=InsectSprays)
    summary(fit)$coef
    est <- summary(fit)$coef[,1]
    nfit <- lm(count~spray -1, data=InsectSprays)
    spray2 <- relevel(InsectSprays$spray, "C")
    fit2 <- lm(count~spray2, InsectSprays)
    
}

swirl_3_multi_var3 <- function() {
    library(datasets)
    dim(hunger)
    fit <- lm(Numeric~Year, hunger)
    lmF <- lm(Numeric[hunger$Sex=="Female"]~Year[hunger$Sex=="Female"],hunger)
    lmM <- lm(Numeric[hunger$Sex=="Male"]~Year[hunger$Sex=="Male"],hunger)
    lmBoth <- lm(Numeric~Year+Sex, hunger)
    lmInter <- lm(Numeric~Year+Sex+Sex*Year, hunger)
    
}

swirl_3_res_diag <- function() {
    # outlier effect
    fit <- lm(y~x, out2)
    # plot residual versus fitted value
    plot(fit, which=1)
    # build a lm without the outlier which is the first row
    fitno <- lm(y~x, out2[-1,])
    plot(fitno, which=1)
    # dfbeta is taking out the first data, get the coef(fit)-coef(fitno)
    head(dfbeta(fit))
    # this is the residue of the x-value from prediction which exclude this point
    resno <- out2[1,"y"] - predict(fitno,out[1,])
    1- resid(fit)[1]/resno
    # hatvalue auto above calcuation for each point
    head(hatvalues(fit))
    sigma <- sqrt(deviance(fit)/df.residual(fit))
    rstd <- resid(fit)/(sigma*sqrt(1-hatavalues(fit)))
    # rstandard does the same thing as above
    head(chind(rstd, rstandard(fit)))
    # plot shows the square root of standardized resid against fitted value
    plot(fit, which=3)
    plot(fit, which=2)
    sigma1 <- sqrt(deviance(fitno)/df.residual(fitno))
    resid(fit)[1]/(sigma1*sqrt(1-hatvalues(fit)[1]))
    # rstudent() does same as above
    # Cook's distance
    dy <- predict(fitno, out2) - predict(fit, out2)
    sum(dy^2)/(2*sigma^2)
    # same asusing cooks.distance(fit)[1]
    plot(fit, which=5)
    
    
}