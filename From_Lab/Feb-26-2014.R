# Februrary 26, 2014
# Outline:
#   1. Investigate added-variable plots (aka avPlots, partial regression plots)
#      with real data.
#   2. Do a simulation study to show how R^2 increases in a few cases

# 1. av plots
# What are they?
# See: Wikipedia: https://en.wikipedia.org/wiki/Partial_regression_plot
# But, in a (not so terse) nutshell:
# The residuals from Y ~ X1 + X2 gives us a sense of how much variability in Y 
# is not captured by X1 and X2. Hence, the residuals from Z ~ X1 + X2 tell us
# how much variability resides in Z that is not explained by X1 + X2. If the
# residual vectors from these two vectors "have nothing to do with each other"
# Then it would stand to reason adding the extra variability from Z into the
# model "X1 + X2 + Z" should do a better job capturing the variability in Y.
# If the residuals are strongly correlated, then introducing Z to the model
# won't help (much) in capturing the (previously) left over variability since
# X1 + X2 explain Y and Z similarly, so we aren't introducing (a lot of) new
# information. 

# To get a feel for this we'll use Duncan's occupation data and consider the
# base model of "~ education" in modeling prestige. In particular, we'll check
# out the affect of adding income to the model.

library(car) # contains Duncan's occupation data, and avPlots
data(Duncan)

baseModel <- lm(prestige ~ education, data=Duncan)
summary(baseModel)

partialReg <- lm(income ~ education, data=Duncan)
summary(partialReg)

# make the avplot by hand with ggplot2
library(ggplot2)

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

makeGGScatter <- function(X, Y, .res=FALSE) {
  axisLabels <- if (.res) {
    paste0(c(substitute(X), substitute(Y)),
           "'s regression residuals")
  } else {
    paste0(c(substitute(X), substitute(Y)))
  }
  
  ggplot(data=data.frame(X=X,Y=Y), aes(x=X, y=Y)) +
    geom_point(shape=1) +
    stat_smooth(method="lm", se=FALSE, color='red') +
    theme_bw() +
    labs(x=axisLabels[1],
         y=axisLabels[2],
         title=NULL)
}

partialRegRes <- partialReg$residuals
baseModelRes <- baseModel$residuals

within(data=data.frame(), expr={
  income <- partialRegRes
  prestige <- baseModelRes
  ggAVPlot <- makeGGScatter(income, prestige, .res=TRUE)
  show(ggAVPlot)  
})

cor(partialRegRes, baseModelRes) # 0.61 <- this is fairly high

# Now, we check out the extended model: "income + education"
extendedModel <- lm(prestige ~ income + education, data=Duncan)
summary(extendedModel)

# calculate R^2 of the two models
calcR2Increase <- function(baseModel, extendedModel) {
  extendedModelR2 <- var(extendedModel$fitted.values)/var(Duncan$prestige)
  baseModelR2 <- var(baseModel$fitted.values)/var(Duncan$prestige)
  
  # find the percent increase
  abs(baseModelR2 - extendedModelR2) / baseModelR2 
}

calcR2Increase(baseModel, extendedModel) # approximately 14%  

# look at colinearity between income and education
cor(Duncan$income, Duncan$education) # 0.7245

# Let's do some EDA on these variables
with(data=Duncan, makeGGScatter(income, education, FALSE)) # seem pretty colinear

coplot(prestige ~ education | income, data=Duncan, panel=panel.smooth, span=.99)
coplot(prestige ~ income | education, data=Duncan, panel=panel.smooth, span=.99)
# ^^ so prestige conditioning out a variable doesn't completely kill
# predictability of the remainder of the model

# Now, let's consider adding the other variable: "type".
# First, let's do some basic EDA
incVsType <- ggplot(data=Duncan, aes(x=type, y=income)) +
  geom_boxplot(outlier.shape=1) + 
  labs(x=NULL, title=NULL) +
  theme_bw()

educVsType <- ggplot(data=Duncan, aes(x=type, y=education)) +
  geom_boxplot(outlier.shape=1) + 
  labs(x=NULL, title=NULL) +
  theme_bw()

multiplot(incVsType, educVsType, cols = 2)

# Alternatively,
library(reshape2)
ggplot(data=melt(data=Duncan,id.vars=c("type"),
                 measure.vars=c("income", "education")),
       aes(x=type, y=value, fill=variable)) +
  geom_boxplot(outlier.shape=1) +
  scale_fill_discrete(guide=FALSE) +
  facet_grid(~variable, scales="free_y") +
  theme_bw() +
  labs(x=NULL, title=NULL, y=NULL)

# so there's a clear trend between type and income (education)
# Let's try and do the regression "type ~ income + education"

partialReg2 <- lm(factor(type) ~ income + education, data=Duncan)

# STOP! You guys don't know how to predict categorical output, yet.
# Without using this residuals, let's add type to the model, anyway and see
# how the R^2 increases.

extendedModel2 <- lm(prestige ~ income + education + type, data=Duncan)
summary(extendedModel2)

# Notice: dummy cariables for type aren't very significant -- an indicator that
# income and education are doing most of the heavy lifting.

calcR2Increase(extendedModel, extendedModel2) # approximately 10%
calcR2Increase(baseModel, extendedModel2) # approximately 25%

# this exercise was partly to remind you the limitations of our tools.
# now, let's do this all over, but with our base model being just "~ type".

baseModel <- lm(prestige ~ type, data=Duncan)
extendedModel <- lm(prestige ~ type + education, data=Duncan)

calcR2Increase(baseModel, extendedModel) # approximately 8%

# let's check out the avplot
within(data=data.frame(), {
  education <- resid(lm(education ~ type, data=Duncan))
  prestige <- resid(baseModel)
  avPlot <- makeGGScatter(education, prestige, .res=TRUE)
  show(avPlot)
  print(cor(education, prestige)) # about 0.491
})

partialReg2 <- lm(income ~ type + education, data=Duncan)
summary(partialReg2) 

# what does the avplot tell us?
within(data=data.frame(), {
  income <- resid(partialReg2)
  prestige <- resid(extendedModel)
  avPlot <- makeGGScatter(income, prestige, .res=TRUE)
  show(avPlot)
  print(cor(income, prestige)) # about 0.727
})
# where the model fails to explain prestige's behaviour, it also fails to
# explain income's behaviour. => we shouldn't expect to glean too much more
# by adding income to the model

calcR2Increase(extendedModel,extendedModel2) # about 12%
calcR2Increase(baseModel, extendedModel2) # about 21%

# Don't let the numbers above fool you: R^2 will always increase with the
# addition of predictors (assuming you're adding new predictors that aren't 
# linear combinations of previous predictors)

analyzePartialReg <- function(baseModel, partialReg) {
  partialRegResiduals <- resid(partialReg)
  baseModelResiduals <- resid(baseModel)
  avPlot <- makeGGScatter(partialRegResiduals, baseModelResiduals)
  show(avPlot)
  cor(partialRegResiduals,baseModelResiduals)
}

# 2. Simulation study: the various ways R^2 can increase.
#   a) X1,X2 not correlated, both predictive of y

set.seed(1234)
eps <- rnorm(100)
X1 <- rnorm(100, mean=2, sd=1)
X2 <- rnorm(100, mean=-3, sd=1)
Y <- 1 + X1 + X2 + X2^2 + eps

makeGGScatter(X1, X2)
makeGGScatter(X1, Y)
makeGGScatter(X2, Y)

baseModel <- lm(Y ~ X1)
partialReg <- lm(X2 ~ X1)

analyzePartialReg(baseModel, partialReg) # cor \approx -0.93
extendedModel <- lm(Y ~ X1 + X2)
calcR2Increase(baseModel, extendedModel) # > 53x increase!
summary(extendedModel) # R^2 = 0.8639

# ^^ X1 does a crap job predicting X2 (understandably), however
# because of X2 contains a lot of the variability in Y a lot of what X1 can't
# explain about X2 overlaps with what it can't explain about Y. 

# Since we see that Y may vary quadratically with X2, consider doing this
# one more time
X3 <- X2^2
baseModel <- extendedModel
partialReg <- lm(X3 ~ X1 + X2)
summary(partialReg) 
# ^^ R^2 = 0.9265 => this captures a lot of the variability

analyzePartialReg(baseModel, partialReg) # cor \approx 0.84
# ^^ where/how X1 + X2 fails to explain Y has heavy overlap with where/how
# it fails to explain X3. Perhaps part of the background process that generated
# X3 has a hand in the generation of Y?

extendedModel <- lm(Y ~ X1 + X2 + X3)
calcR2Increase(baseModel, extendedModel) # < 11.5% increase
summary(extendedModel) # R^2 = 0.9602

# 2.b) X1,X2 not correlated, only 1 predictive of y

Y <- 1 + X1^2 + eps
makeGGScatter(X1, X2)
makeGGScatter(X1, Y)
makeGGScatter(X2, Y)

baseModel <- lm(Y ~ X1)
summary(baseModel) # R^2 = 0.9682

partialReg <- lm(X2 ~ X1) # R^2 = 0.0086
analyzePartialReg(baseModel, partialReg) # cor less than 0.082
# ^^ residuals are very loosely correlated:
# the phenomena underlying Y and X2 (once X1 is taken into account)
# don't have a very strong link. Shouldn't expect an large increase in R^2
extendedModel <- lm(Y ~ X1 + X2)
calcR2Increase(baseModel, extendedModel) # < 0.5% increase (> 0, though)

# 2.c) X1,X2 correlated, still both significant for Beta
set.seed(1234)
X2 <- X1^2 + rnorm(100, sd=0.5)
cor(X1, X2) # 0.9208464
Y <- 1 + X1 + X2 + eps
makeGGScatter(X1, X2)
makeGGScatter(X1, Y)
makeGGScatter(X2, Y)

baseModel <- lm(Y ~ X1) # R^2 = 0.8414
partialReg <- lm(X2 ~ X1) # R^2 = 0.848
analyzePartialReg(baseModel, partialReg) # cor \approx 0.903
# source of error seems to come from similar places
extendedModel <- lm(Y ~ X1 + X2)
summary(extendedModel) 
# all coefficients are highly significant
# R^2 = 0.9707 which isn't surprising since we've hit the model near dead-on
calcR2Increase(baseModel, extendedModel) # < 16% increase

# 2.d) X1,X2 correlated, jointly predictive of y, neither significant Beta
library(MASS)
set.seed(1234)
X <- mvrnorm(n=100,mu=c(2,-3),Sigma=matrix(c(4,1,1,9),nrow=2))
X1 <- X[,1]; X2 <- X[,2]
Z1 <- rnorm(100, sd=10)
Z2 <- rnorm(100,sd=1)
Z3 <- rexp(n=100,rate=0.5) 

Y <- 1 + 0.05*abs(X1 - X2) + Z3
makeGGScatter(X1, X2) # cor(X1, X2) = 0.776
makeGGScatter(X1, Y) # cor(X1, Y) = 0.411
makeGGScatter(X2, Y) # cor(X2, Y) = 0.351

# check that X1 and X2 are jointly predictive
simData <- data.frame(X1, X2, Y, Breaks=cut(Y,breaks=6))

yVsX1X2 <- ggplot(data=simData, aes(x=X1, y=X2, color=Breaks)) +
  geom_point(size=4) +
  theme_bw()

show(yVsX1X2)

baseModel <- lm(Y ~ X1) # R^2 = 0.2996
partialReg <- lm(X2 ~ X1)
analyzePartialReg(baseModel, partialReg) # cor \approx 0.0556
extendedModel <- lm(Y ~ X1 + X2)
summary(extendedModel) # all coefficients are significant
# ^^ R^2 = 0.33
calcR2Increase(baseModel, extendedModel) # < 11% increase in R^2
