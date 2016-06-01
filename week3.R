week3_multi_ex_1 <- function() {
   require(datasets); data(swiss); require(GGally); require(ggplot2)
   # always do the scatter plot first
   g = ggpairs(swiss, lower=list(continuous="smooth"), params=c(method="loess"))
   g
   
   # now consider all variables
   summary(lm(Fertility~., data=swiss))$coefficients
   # now consider other models, first only consider Agriculture
   summary(lm(Fertility~Agriculture, data=swiss))$coefficients
   
   # now let's run a simulation to investigate how this Simpson Paradox happens
   n <- 100; x2 <- 1:n; x1 <- .01*x2+runif(n, -0.1, 0.1); y=-x1+x2+rnorm(n, sd=0.01)
   summary(lm(y~x1))$coef
   # now the coef of x1 is huge because it picks up the effect of x2
   summary(lm(y~x1+x2))$coef
   
   dat = data.frame(y=y,x1=x1,x2=x2,ey=resid(lm(y~x2)),ex1=resid(lm(x1~x2)))
   library(ggplot2)
   g = ggplot(dat, aes(y=y,x=x1,colour=x2))
   g = g+geom_point(colour="grey50",size=5) +
       geom_smooth(method=lm,se=false,colour="black") +
       geom_point(size=4)
   g
   
   g = ggplot(dat, aes(y=ey, x=ex1, color=x2))
   g = g+geom_point(colour="grey50",size=5) +
       geom_smooth(method=lm,se=false,colour="black") +
       geom_point(size=4)
   g
   
   # if we add unnecessary variables
   z ,- swiss$Agriculture + swiss$Education
   lm(Fertility~. + z, data=swiss)
   
}

week3_multi_ex_2 <- function() {
    # deal with factor variable
    require(datasets); data(InsectSprays); require(stats)
    g = ggplot(data=InsectSprays, aes(y=count, x=spray, fill=spray))
    g = g + geom_violin(colour="black", size=2)
    g = g + xlab("Type of spray") + ylab("Insect count")
    g
    summary(lm(count~spray, data=InsectSprays))$coef
    # hard coding the dummy variables
    summary(lm(count~
                   I((1*(spray=='B')) + I(1*(spray=='C')) +
                    I)(1*(spray=='D')) + I(1*(spray=='E')) +
                   I(1*(spray=='F'))
                    , data=InsectSprays))$coef
    # if we take out the intercept
    summary(lm(count~spray -1, data=InsectSprays))$coef
    # each data will be compared to its mean instead of intercept which is A
    summarise(group_by(InsectSprays, spray), mn=mean(count))
    # the coeff are just the mean of each group
    # reordering thelevels
    spray2 <- relevel(InsectSprays$spray, "C")
    summary(lm(count~spry2, data=InsectSprays))$coef
    
}

week3_multi_ex_3 <- function() {
    library(datasets); data(swiss)
    head(swiss)
    library(dplyr);
    swiss = mutate(swiss, CahtolicBin=1*(Catholic>50))
    
}

week3_multi_ex_4 <- function() {
    # there are 3 models: 
    # model 1: does not include religion
    # model 2: include religion, but there is no relation between religion
    # variable and agriculture
    # model 3: include the relationship between these 2 variables
    # model 1:
    fit = lm(Fertility~Agriculture, data=swiss)
    g1 = g
    g1 = g + geom_abline(intercept=coef(fit)[1], slope=coef(fit)[2], size=2)
    g1
    
    #2 model 2:
    fit = lm(Fertility~Agriculture+factor(CatholicBin), data=swiss)
    g1 = g
    g1 = g1 + geom_abline(intercept=coef(fit[1], slope=coef(fit)[2],size=2))
    g1 = g1 + geom_abline(intercept=coef(fit)[1]+coef(fit)[3],
                          slope=coef(fit)[2],size=2)
    
    #3 model 3:
    fit = lm(Fertility~Agriculture*factor(CatholicBin), data=swiss)
    g1 = g
    g1 = g1 + geom_abline(intercept=coef(fit)[1],slope=coef(fit[2], size=2)
                          g1 = g1+geom_abline(intercept=coef(fit)[1]+coef(fit)[3],
                                              slope=coef(fit)[2]+coef(fit)[4],
                                              size=2))
    g1
    
}

week3_adj_ex <- function() {
    n <- 100; t<-rep(c(0,1), c(n/2,n/2)); x<- c(runif(n/2),runif(n/2));
    beta0 <- 0;beta1<-2; tau<-1; sigma<-0.2
    y <- beta0 + x*beta1 + t*tau + rnorm(n,sd=sigma)
    plot(x,y,ype="n",frame=FALSE)
    abline(lm(y~x),lwd=2)
    abline(h=mean(y[1:(n/2)]),lwd=3)
    abline(h=mean(y[(n/2+1):n]),lwd=3)
    fit <-lm(y~x+t)
    abline(coef(fit)[1],cief(fit)[2],lwd=3)
    abline(coef(fit)[1]+coef(fit)[3],coef(fit)[2],lwd=3)
    points(x[1:(n/2)],y[1:(n/2)],pch=21,col="black",bg="lightblue",cex=)
    points(x[(n/2+1):n],y[(n/2+1):n],pch=21,col="black",bg="salmon")
    
}

week3_res_diag_1 <- function() {
    data(swiss); par(mfrow=c(2,2))
    fit <- lm(Fertility~., data=swiss); plot(fit)
    
    #?influence.measures
    n <- 100; x<- c(10,rnorm(n)); y<- c(10, c(rnorm(n)))
    plot(x,y,frame=FALSE, cex=2, pch=21,bg="lightblue",col="black")
    abline(lm(y~x))
    fit <- lm(y~x)
    round(dfbetas(fit)[1:10,2],3)
    round(hatvalues(fit)[1:10],3)
    
    dat <- read.table('http://www4.stat.ncsu.edu/~stefanski/NFS_Supported/
                      Hidden_images/orly_owl_files/orly_owl.csv')
    pairs(dat)
    summary(lm(V1~. -1, data=dat))$coef
    fit <- lm(V1~. -1, data=dat); plot(predict(fit),resid(fit),pch='.')
    # check the residual plot, we can see the poor model
    
}

week3_model_sel_1 <- function() {
    ## variance inflation
    # now x2, x3 are indep of x1, the variance are close
    n <-100; nosim <- 1000
    x1 <- rnorm(n); x2 <- rnorm(n); x3 <-rnorm(n);
    betas <- sapply(1:nosim, function(i) {
        y <- x1 + rnorm(n, sd=.3)
        c(coef(lm(y~x1))[2],
          coef(lm(y~x1+x2))[2],
          coef(lm(y~x1+x2+x3))[2])
    })
    round(apply(betas, 1, sd), 5)
    
    # now x2, x3 ar related with x1
    n <-100; nosim <- 1000
    x1 <- rnorm(n); x2 <- x1/sqrt(2) + rnorm(n)/ssqrt(2); 
    x3 <-x1*0.95+rnorm(n) * sqrt(1-0.95^2);
    betas <- sapply(1:nosim, function(i) {
        y <- x1 + rnorm(n, sd=.3)
        c(coef(lm(y~x1))[2],
          coef(lm(y~x1+x2))[2],
          coef(lm(y~x1+x2+x3))[2])
    })
    round(apply(betas, 1, sd), 5) 
    
    y <- x1 + rnorm(n, sd=.3)
    a <- summary(lm(y~x1))$cov.unscaled[2,2]
    c(summary(lm(y~x1+x2))$cov.unscaled[2,2],
      summary(lm(y~x1+x2+x3))$cov.unscaled[2,2])/a
    temp <- apply(betas,1,var); temp[2:3]/temp[1]
    
    
    ## Swiss data VIF
    library(car)
    fit <- lm(Fertility~., data=swiss)
    vif(fit)
    sqrt(vif(fit))  #I prefer sd
    
    
}