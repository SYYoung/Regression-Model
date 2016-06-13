makelms <- function(x1, x2, x3){
    # Simulate a dependent variable, y, as x1
    # plus a normally distributed error of mean 0 and 
    # standard deviation .3.
    y <- x1 + rnorm(length(x1), sd = .3)
    # Find the coefficient of x1 in 3 nested linear
    # models, the first including only the predictor x1,
    # the second x1 and x2, the third x1, x2, and x3.
    c(coef(lm(y ~ x1))[2], 
      coef(lm(y ~ x1 + x2))[2], 
      coef(lm(y ~ x1 + x2 + x3))[2])
}

# Regressor generation process 1.
rgp1 <- function(){
    print("Processing. Please wait.")
    # number of samples per simulation
    n <- 100
    # number of simulations
    nosim <- 1000
    # set seed for reproducibility
    set.seed(4321)
    # Point A
    x1 <- rnorm(n)
    x2 <- rnorm(n)
    x3 <- rnorm(n)
    # Point B
    betas <- sapply(1 : nosim, function(i)makelms(x1, x2, x3))
    round(apply(betas, 1, var), 5)
}

# Regressor generation process 2.
rgp2 <- function(){
    print("Processing. Please wait.")
    # number of samples per simulation
    n <- 100
    # number of simulations
    nosim <- 1000
    # set seed for reproducibility
    set.seed(4321)
    # Point C
    x1 <- rnorm(n)
    x2 <- x1/sqrt(2) + rnorm(n) /sqrt(2)
    x3 <- x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95^2)
    # Point D
    betas <- sapply(1 : nosim, function(i)makelms(x1, x2, x3))
    round(apply(betas, 1, var), 5)
}
swirl_4_var_inf <- function() {
    install.packages("cars")
    
    # use vif on swiss data
    data(swiss)
    head(swiss)
    mdl <- lm(Fertility~., data=swiss)
    vif(mdl)
    mdl2 <- lm(Fertility~Agriculture+Education+Catholic+Infant.Mortality, 
               data=swiss)
    vif(mdl2)
    
}

swirl_4_over_under_fit <- function() {
    # must use functions inside fitting.R
    x1c <- simbias()
    apply(x1c,1,mean)
    
    # use anova to evaluate the significant of the regressor 
    fit1 <- lm(Fertility ~ Agriculture, data=swiss)
    fit3 <- lm(Fertility~Agriculture+Examination+Education, data=swiss)
    anova(fit1, fit3)
    d <- deviance(fit3)/43
    n <- (deviance(fit1)-deviance(fit3))/2
    # n/d is same the value of F value from anova()
    n/d
    # calculate this p-value, assume that the residuals are normality
    pf(n/d, 2, 43, lower.tail=FALSE)
    # use shapiro-wilk test to verify
    shapiro.test(fit3$residuals)
    # value0.336, fail to reject
    anova(fit1, fit3, fit5, fit6)
    
}

swirl_4_binary_outcome <- function() {
    
}