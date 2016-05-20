swirl_2_res_var <- function() {
    fit <- lm(child~parent, data=galton)
    sqrt(sum(fit$residuals^2)/n-2)
    summary(fit)$sigma
    sqrt(deviance(fit)/(n-2))
    
    mu <- mean(galton$child)
    sTot <- sum((galton$child-mu)^2)
    sRes <- deviance(fit)
    # R-squared is equal to 1 - sRes/sTot
    1 - sRes/sTot
    summary(fit)$r.squared
    
}

# Regress the given variable on the given predictor,
# suppressing the intercept, and return the residual.
regressOneOnOne <- function(predictor, other, dataframe){
    # Point A. Create a formula such as Girth ~ Height -1
    formula <- paste0(other, " ~ ", predictor, " - 1")
    # Use the formula in a regression and return the residual.
    resid(lm(formula, dataframe))
}

# Eliminate the specified predictor from the dataframe by
# regressing all other variables on that predictor
# and returning a data frame containing the residuals
# of those regressions.
eliminate <- function(predictor, dataframe){
    # Find the names of all columns except the predictor.
    others <- setdiff(names(dataframe), predictor)
    # Calculate the residuals of each when regressed against the given predictor
    temp <- sapply(others, function(other)regressOneOnOne(predictor, other, dataframe))
    # sapply returns a matrix of residuals; convert to a data frame and return.
    as.data.frame(temp)
}

swirl_2_intro_multivar <- function() {
    ones <- rep(1, nrow(galton))
    lm(child~ones+parent -1, galton)
    
    data(trees)
    View(trees)
    head(trees)
    fit <- lm(Volume~Girth+Height+Constant -1, trees)
    trees2 <- eliminate("Girth", trees)
    fit2 <- lm(Volume~Height+Constant -1, trees2)
    lapply(list(fit, fit2), coef)
    lm(formula=Volume~Constant -1, data=eliminate("Height",trees2))
    
    
}

makelms <- function(){
    # Store the coefficient of linear models with different independent variables
    cf <- c(coef(lm(Fertility ~ Agriculture, swiss))[2], 
            coef(lm(Fertility ~ Agriculture + Catholic,swiss))[2],
            coef(lm(Fertility ~ Agriculture + Catholic + Education,swiss))[2],
            coef(lm(Fertility ~ Agriculture + Catholic + Education + Examination,swiss))[2],
            coef(lm(Fertility ~ Agriculture + Catholic + Education + Examination +Infant.Mortality, swiss))[2])
    print(cf)
}

# Regressor generation process 1.
rgp1 <- function(){
    print("Processing. Please wait.")
    # number of samples per simulation
    n <- 100
    # number of simulations
    nosim <- 1000
    # set seed for reproducability
    set.seed(4321)
    # Point A:
    x1 <- rnorm(n)
    x2 <- rnorm(n)
    x3 <- rnorm(n)
    # Point B:
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
    # set seed for reproducability
    set.seed(4321)
    # Point C:
    x1 <- rnorm(n)
    x2 <- x1/sqrt(2) + rnorm(n) /sqrt(2)
    x3 <- x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95^2)
    # Point D:
    betas <- sapply(1 : nosim, function(i)makelms(x1, x2, x3))
    round(apply(betas, 1, var), 5)
}

swirl_multivar_ex <- function() {
    all <- lm(Fertility~., data=swiss)
    summary(all)
    summary(lm(Fertility~Agriculture, swiss))
    cor(swiss$Examination, swiss$Education)
    
    # test what happens when we add a variable that provides no new
    # linear information to a model
    ec <- swiss$Examination + swiss$Catholic
    efit <- lm(Fertility~.+ec, swiss)
    all$coefficients - efit$coefficients
    
}

