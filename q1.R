# The exponential distribution can be simulated in R with rexp(n, lambda) where lambda is the rate parameter. 
# The mean of exponential distribution is 1/lambda and the standard deviation is also 1/lambda. 
# Set lambda = 0.2 for all of the simulations.


plotExpMean <- function (n=40, t=1000, mylambda=.2) {
    expdata <- matrix(nrow=t, ncol=n)
    expmeans <- c(rep(NULL, t))
    for (i in 1:t)  {
        expdata[i,] <- rexp(n, mylambda)
        expmeans[i] <- mean(expdata[i,])
    }
    par(mfrow=c(1,2))
    hist(expmeans)
    abline(v=mean(expmeans), lwd=2, col="red")
    hist(expdata)
    abline(v=mean(expdata), lwd=2, col="red")
}

plotExpSD <- function (n=40, t=1000, mylambda=.2) {
    expdata <- matrix(nrow=t, ncol=n)
    expSDs <- c(rep(NULL, t))
    for (i in 1:t)  {
        expdata[i,] <- rexp(n, mylambda)
        expSDs[i] <- sd(expdata[i,])
    }
    par(mfrow=c(1,1))
    hist(expSDs)
    abline(v=mean(expSDs), lwd=2, col="red")
}

plotExpVar <- function (n=40, t=1000, mylambda=.2) {
    expdata <- matrix(nrow=t, ncol=n)
    expVars <- c(rep(NULL, t))
    for (i in 1:t)  {
        expdata[i,] <- rexp(n, mylambda)
        expVars[i] <- var(expdata[i,])
    }
    par(mfrow=c(1,1))
    hist(expVars)
    abline(v=mean(expVars), lwd=2, col="red")
}

plotExpMean(t=2000)
readline("press enter to continue...")
plotExpSD(t=2000)
readline("press enter to continue...")
plotExpVar(t=2000)


