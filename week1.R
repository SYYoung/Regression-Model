week1 <- function() {
    install.packages("UsingR")
    library(UsingR); data(galton); library(reshape); long <- melt(galton)
    g <- ggplot(long, aes(x=value, fill=variable))
    g <- g + geom_histogram(color="black", binwidth=1)
    g <- g + face_grid(.~variable)
    g
    
}

week1_ext <- function() {
    library(manipulate)
    manipulate(myHist(mu), mu=slider(62,74,step=0.5))
}

myHist <- function(mu){
    library(manipulate)
    mse <- mean((galton$child - mu)^2)
    g <- ggplot(galton, aes(x=child) + geom_histogram(fill="salmom",color="black",
                                                      binwidth=1))
    g <- g + geom_vline(xintercept=mu, size=3)
    g <- g + ggtitle(paste("mu=", mu, ", MSE=", round(mse,2), sep=""))
    g
}

myPlot <- function(beta) {
    g <- ggplot(filter(freqData, freq>0), aes(x=parent, y=child))
    g <- g + scale_size(range=c(2,20), guide="none")
    g <- g + geom_point(color="grey50", aes(size=freq+20, show_guide=FALSE))
    g <- g + geom_point(aes(color=freq, size=freq))
    g <- g + scale_colour_gradient(low="lightblue", high="white")
    g <- g + geom_abline(intercept=0, slope=beta, size=3)
    mse <- mean((y-beta*x)^2)
    g <- g + ggtitle(paste("beta=", beta, "mse= ", round(mse,3)))
    g
}
week1_father_child <- function() {
    y <- galton$child - mean(galton$child)
    x <- galton$parent - mean(galton$parent)
    freqData <- as.data.frame(table(x,y))
    names(freqData) <- c("child", "parent", "freq")
    freqData$child <- as.numeric(as.character(freqData$child))
    freqData$parent <- as.numeric(as.character(freqData$parent))
    manipulate(myPlot(beta), beta=slider(0.6,1.2,step=0.02))
}