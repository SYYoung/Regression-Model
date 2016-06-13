week4_log_1 <- function() {
    download.file("https://dl.dropboxusercontent.com/u/7710864/data/ravensData.rda",
                  destfile="./data/ravensData.rda",method="curl")
    load("./data/ravensData.rda")
    head(ravensData)
    # let's try linear regression first
    lmRavens <- lm(ravensData$ravenWinNum~ravensData$ravenScore)
    summary(lmRavens)$coef
    
    # visualizing fitting logistic regression curves
    x <- seq(-10,10,length=1000)
    manipulate(
        plot(x,exp(beta0+beta1*x)/(1+exp(beta0+beta1*x)),
             type="l",lwd=3,frame=FALSE),
        beta1= slider(-2,2,step=.1,initial=2),
        beta0 = slider(-2,2,step=.1,initial=0))
    
}

week4_log_3 <- function() {
    logRegRavens <- glm(ravensData$ravenWinNum~ravensData$ravenScore,family="binomial")
    summary(logRegRavens)
    plot(ravensData$ravenScore,logRegRavesn$fitted,pch=19,col="blue",
         xlab="Score",ylab="Prob Ravens Win")
    exp(logRegRavens$coeff)
    exp(confint(logRegRavens))
    # anova for logistic regression
    anova(logRegRavens, test="Chisq")
    
}

week4_pois_1 <- function() {
    # investigate traffice of Jeek's site
    download.file("https://dl.dropboxusercontent.com/u/7710864/data/gaData.rda",
                  destfile="./data/gaData.rda",method="curl")
    load("./data/gaData.rda")
    gaData$julian <- julian(gaData$date)
    head(gaData)
    # first, try linear model
    plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",
         ylab="Visits")
    lm1 <-lm(gaData$visits ~ gaData$julian)
    abline(lm1,col="red",lwd=3)
    # 2nd approach: taking the log of the outcome
    # model: log(NHi) = b0 + b1*JDi + ei
    # exp(E[log(Y)]) = exp(1/n*sum(logyi)) = (y1*y2*...*yn)^1/n
    round(exp(coef(lm(I(log(gaData$visits+1)) ~ gaData$julian))),5)
    
}

week4_pois_2 <- function() {
    plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",
         ylab="Visits")
    glm1 <- glm(gaData$visits ~ gaData$julian, family="poisson")
    abline(lm1,col="red",lwd=3); lines(gaData$julian,glm1$fitted,col="blue,lwd=3")
    
    # relative to something
    glm2 <- glm(gaData$simplystats ~ julian(gaData$date),
                offset=log(visits+1), family="poisson",data=gaData)
    plot(julian(gaData$date),glm2$fitted,col="blue",pch=19,
         xlab="Date",ylab="fitted counts")
    points(julian(gaData$date),glm1$fitted,col="red",pch=19)
    
    glm2 <- glm(gaData$simplystats~julian(gaData$date),
                offset=log(visits+1),family="poisson",data=gaData)
    plot(julian(gaData$date),gaData$simplystats/(gaData$visits+1),
         col="grey",xlab="Date",ylab="fitted Rate",
         pch=19)
    lines(julian(gaData$date),glm2$fitted/(gaData$visits+1),
          col="blue",lwd=3)
    
}

week4_hodgepodge <- function() {
    n <-500; s<- seq(0,4*pi,length=n); y<-sin(x)+rnorm(n,sd=.3)
    knots <- seq(0,8*pi,length=20);
    splineTerms <- sapply(knots, function(knot) (x>knot)*(x-knot))
    xMat <- cbind(1,x,splineTerms)
    yhat <- predict(lm(y~xMat -1))
    plot(x,y,frame=FALSE,pch=21,bg="lightblue",cex=2)
    lines(x,yhat,col="red",lwd=2)
    # now since it is discontinus differential, use squqre term instead
    splineTerms <- sapply(knots, function(knot) (x>knot)*(x-knot)^2)
    xMat <- cbind(1,x,x^2,splineTerms)
    yhat<- predict(lm(y~xMat -1))
    
    # Harmonics using linear models
    ## chord finder, playing the white keys on a piano from octave c4-c5
    notes4 <- c(261.63,293.66,329.63,349.23,392.00,440.00,493.88,523.25)
    t <- seq(0,2,by=0.001); n <- length(t)
    c4 <- sin(2*pi*notes4[1]*t); e4 <- sin(2*pi*notes4[3]*t);
    g4 <- sin(2*pi*notes4[5]*t)
    chord <- c4+e4+g4+rnorm(n,0,0.3)
    x <- sapply(notes,function(freq) sin(2*pi*freq*t))
    fit <- lm(chord~x -1)
    # it is just fourier transform
    a <- fft(chord); plot(Re(a)^2, type="l")
    
}