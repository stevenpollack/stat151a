# Februrary 26, 2014
# Outline:
#   1. Investigate added-variable plots (aka avPlots, partial regression plots)

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

baseModel <- lm(prestige ~ education, data=Duncan)
summary(baseModel)

partialReg <- lm(income ~ education, data=Duncan)
summary(partialReg)

# make the avplot by hand with ggplot2
library(ggplot2)

makeGGScatter <- function(X, Y, .res=FALSE) {
  axisLabels <- if (.res) {
    paste0(c(substitute(X), substitute(Y)),
           "'s regression residuals")
  } else {
    paste0(c(substitute(X), substitute(Y)))
  }
  
  ggplot(data=data.frame(X=X,Y=Y), aes(y=X, x=Y)) +
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
extendedModelR2 <- var(extendedModel$fitted.values)/var(Duncan$prestige)
baseModelR2 <- var(baseModel$fitted.values)/var(Duncan$prestige)

# find the percent increase
abs(baseModelR2 - extendedModelR2) / baseModelR2 # approximately 14%

# look at colinearity between income and education
cor(Duncan$income, Duncan$education) # 0.7245

# Let's do some EDA on these variables
with(data=Duncan, makeGGScatter(income, education, FALSE)) # seem pretty colinear

coplot(prestige ~ education | income, data=Duncan, panel=panel.smooth, span=.99)
coplot(prestige ~ income | education, data=Duncan, panel=panel.smooth, span=.99)

extendedModel$effects

?avPlot

avPlots(lm(prestige~income+education+type, data=Duncan))
# 
# 1) X1,X2 not correlated, both predictive of y
# 2) X1,X2 not correlated, only 1 predictive of y
# 3) X1,X2 correlated, still both significant for Beta
# 4) X1,X2 correlated, jointly predictive of y, neither significant Beta

data(Duncan)

# avPlot of Prestige ~ . - Income
prestigeAgainstAllLessIncome <- lm(formula = prestige ~ . - income, data=Duncan)
incomeAgainstAllLessPrestige <- lm(formula = income ~ . - prestige, data=Duncan)

resY <- prestigeAgainstAllLessIncome$residuals
resX <- incomeAgainstAllLessPrestige$residuals

ggplot(data=data.frame(x=resX, y=resY), aes(x=x,y=y)) + 
  geom_point(shape=1) + 
  stat_smooth(method="lm", se=FALSE, color='red') +
  labs(x="Income's regression residuals", y="Prestige's regression residuals") +
  theme_bw()

?Duncan
