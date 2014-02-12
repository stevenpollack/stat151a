### Outline:
### 1) Multivariate normal
###   https://en.wikipedia.org/wiki/Multivariate_normal
### 2) correlation between \hat{\beta}'s.

#####
# Multiple plot function 
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot
# objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
#####

generateBivariateSigmaFromRho <- function(rho, sigma_x=1, sigma_y=1) {
  stopifnot(-1 <= rho & rho <= 1)
  matrix(c(sigma_x^2, rho*sigma_x*sigma_y, rho*sigma_x*sigma_y, sigma_y^2),
         nrow=2)
}

calculateRhoFromSigma <- function(Sigma, verbose=FALSE) {
  stopifnot(eigen(Sigma,symmetric=TRUE,only.values=TRUE)$values > 0)
  varX <- Sigma[1,1]; varY <- Sigma[2,2]
  rho <- Sigma[1,2] / sqrt(varX * varY)
  if (verbose) {
    angle <- atan(sqrt(varX / varY)) * 180 / pi
    print(paste("Angle between X and Y is:", angle))
  }
  return(rho)
}

generateAndPlotBivariateNormal <- function(mu, Sigma, h, verbose=FALSE) {
  stopifnot(t(Sigma) == Sigma)
  stopifnot(h > 0)
  
  require(MASS)
  require(ggplot2)
  
  set.seed(1234)
  X <- mvrnorm(n=1000, mu=mu, Sigma=Sigma)
  df <- data.frame(X=X[,1], Y=X[,2])
  
  if (verbose) {
    cat("Sample correlation matrix for X and Y: \n")
    print(cor(df))
    print(paste("rho is", calculateRhoFromSigma(Sigma, verbose)))
  }
    
  xTitle <- bquote(expression( paste(mu, " = ", .(mu[1]), ", ", sigma^2, " = ", .(Sigma[1,1]), sep="")))
  yTitle <- bquote(expression( paste(mu, " = ", .(mu[2]), ", ", sigma^2, " = ", .(Sigma[2,2]), sep="")))
  
  densityContour <- ggplot(data=df, aes(x=X,y=Y)) + geom_point(shape=1,alpha=0.5) + stat_density2d(h=h, color='darkred')
  xDensity <- ggplot(data=df) + stat_density(aes(x=X), adjust=1.5) + labs(title=eval(xTitle))
  yDensity <- ggplot(data=df) + stat_density(aes(x=Y), adjust=1.5) + labs(title=eval(yTitle))
  
  multiplot(densityContour, xDensity, yDensity, layout=matrix(c(1,2,1,3),2))
}

Sigma1 <- matrix(c(4,1.75,1.75,1),nrow=2)
mu1 <- c(7,2)
h1 <- 3

generateAndPlotBivariateNormal(mu1, Sigma1, h1, verbose=TRUE)

Sigma2 <- matrix(c(0.25,0.2,0.2,1),nrow=2)
mu2 <- c(7,2)
h2 <- 3

generateAndPlotBivariateNormal(mu2, Sigma2, h2)

### go over some other sigma's.

### 2) Beta hat's...

### To perform our simulation study we'll need to
###
### 1. Create a design matrix, X
### 2. Create a (true) vector of coefficients, beta
### 3. Generate 1000, IID error vectors e
### 4. Make 1000, different, Y's according to the model Y = X %*% beta + e
###
### We'll then look at the properties of each betaHat calculated from the
### different Y's that we made with the different e's. 

### load parallel frame work to speed things up
library(doParallel)
registerDoParallel(cores=detectCores())

set.seed(1234)
simulationLength <- 1000

### make the design matrix
X <- cbind(1, rchisq(n=100, df=3))
X_t <- t(X)
### make the model parameters
beta <- c(2,3)
### generate the errors and Y's
response <- foreach(i=seq.int(simulationLength),.combine=cbind) %dopar% {
  e <- rexp(n=100, rate=0.5) # errors have mean=2, var=4
  Y <- X %*% beta + (e-2)
  return(Y)
}

### calculate betaHat for each response vector
betaHats <- foreach(Y=iter(response,by='column'),.combine=rbind) %dopar% {
  betaHat <- as.vector(solve(X_t %*% X) %*% X_t %*% Y)
  return(betaHat)
}

### check out empirical variance of betaHats
var(betaHats)

### compare this to theoretical variance
4*solve(X_t %*% X)

### checkout plots
library(ggplot2)

baseLayer <- ggplot(data=as.data.frame(betaHats))
scatterPlot <- baseLayer + geom_point(aes(x=V1, y=V2),shape=1,alpha=0.5) +
                stat_density2d(aes(x=V1, y=V2),h=0.25) +
                labs(x=expression(beta[0]), y=expression(beta[1])) 
beta0Hist <- baseLayer +
              geom_histogram(aes(x=V1, y=..density..),binwidth=0.08,alpha=0.5) +
              stat_density(aes(x=V1),fill=NaN,color='darkred') +
              labs(x=expression(beta[0]),
                   title=paste("mean =", round(mean(betaHats[,1]),3), "... true value =", beta[1]))
beta1Hist <- baseLayer +
              geom_histogram(aes(x=V2, y=..density..),binwidth=0.02,alpha=0.5) +
              stat_density(aes(x=V2),fill=NaN,color='darkred') +
              labs(x=expression(beta[1]),
                   title=paste("mean =", round(mean(betaHats[,2]),3), "... true value =", beta[2]))

multiplot(scatterPlot, beta0Hist, beta1Hist, layout=matrix(c(1,2,1,3),2))



