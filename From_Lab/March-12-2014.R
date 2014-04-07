# March 12, 2014
#
# prPlots:
#
#
library(MASS)

set.seed(1234)
mu <- 0.33*sample(1:7, size=4, replace=TRUE)
sigmaDiag <- c(1,1,1.5,2)
sigmaOffDiag <- c(0.5, 0.25, 0.69, 0.45, 0.77, 0.9)
sigma <- matrix(1, nrow=4, ncol=4)
sigma[lower.tri(sigma, diag=FALSE)] <- sigmaOffDiag
diag(sigma) <- sigmaDiag
sigma[upper.tri(sigma)] <- t(sigma)[upper.tri(sigma)]
mean(t(sigma) - sigma == 0)

df <- data.frame(mvrnorm(n=1000, mu=mu, Sigma=sigma))
summary(df)
pairs(x=df, panel=panel.smooth, upper.panel=NULL)
cor(df)


df <- within(df, {X5 <- X1^2
                  X6 <- X2^2
                  Y <- X5 + X6 + X3 + rnorm(n=nrow(df))
                  
                  X7 <- log(.Machine$double.eps + abs(X1))
                  Z <- 2*X7 + X6 + X3 + rnorm(n=nrow(df))
                  }
             )

# full model
fullModelForY <- lm(Y ~ X1 + X2 + X3 + X4, data=df)

# partial model 
partialModelForY <- lm(Y ~ X2 + X3 + X4, data=df)
# regress X1 on partial model
partialModelForX1 <- lm(X1 ~ X2 + X3 + X4, data=df)


df <- within(df, {
  adjustedResidual <- fullModelForY$residuals -
    fullModelForY$coefficients["X1"]*df$X1
  r_Y <- partialModelForY$residuals
  r_X1 <- partialModelForX1$residuals
})

library(ggplot2)

prPlot <- ggplot(data=df, aes(x=X1, y=adjustedResidual)) +
  geom_point(shape=1) +
  stat_smooth(method='loess', se=FALSE) +
  stat_smooth(method='lm', se=FALSE, color='red') +
  theme_bw() +
  labs(title="prPlot for variable X1")
show(prPlot)

avPlot <- ggplot(data=df, aes(x=r_X1, y=r_Y)) +
  geom_point(shape=1) +
  stat_smooth(method='loess', se=FALSE) +
  stat_smooth(method="lm", color='red', se=FALSE) +
  theme_bw() +
  labs(title="avPlot for variable X1 on partial model")

show(avPlot)

# What if we did this for a variable that didn't actually
# contribute to our data-generating procedure?
# (E.g. variable X4)

# partial model 
partialModelForY_2 <- lm(Y ~ X1 + X2 + X3, data=df)
# regress X1 on partial model
partialModelForX4 <- lm(X4 ~ X1 + X2 + X3, data=df)


df <- within(df, {
  adjustedResidual2 <- fullModelForY$residuals -
    fullModelForY$coefficients["X4"]*df$X4
  r_Y2 <- partialModelForY_2$residuals
  r_X4 <- partialModelForX4$residuals
})


prPlot2 <- ggplot(data=df, aes(x=X4, y=adjustedResidual2)) +
  geom_point(shape=1) +
  stat_smooth(method='loess', se=FALSE) +
  stat_smooth(method='lm', se=FALSE, color='red') +
  theme_bw() +
  labs(title="prPlot for variable X4")
show(prPlot2)

avPlot2 <- ggplot(data=df, aes(x=r_X4, y=r_Y2)) +
  geom_point(shape=1) +
  stat_smooth(method='loess', se=FALSE) +
  stat_smooth(method="lm", color='red', se=FALSE) +
  theme_bw() +
  labs(title="avPlot for variable X4 on partial model")

show(avPlot2)

bestModel <- lm(Y ~ I(X1^2) + X2 + X3, data=df)
summary(bestModel) # MSE is 1.633
summary(fullModelForY) # MSE is 2.336

# that was easy, what if we have a log'd predictor, though?

# full model
fullModelForZ <- lm(Z ~ X1 + X2 + X3 + X4, data=df) 
# MSE is 2.7777

# partial model 
partialModelForZ <- lm(Z ~ X2 + X3 + X4, data=df)
# regress X1 on partial model
partialModelForX1_2 <- lm(X1 ~ X2 + X3 + X4, data=df)

df <- within(df, {
  adjustedResidual <- fullModelForZ$residuals - fullModelForZ$coefficients["X1"]*df$X1
  r_Z <- partialModelForZ$residuals
  r_X1 <- partialModelForX1_2$residuals
})

prPlot3 <- ggplot(data=df, aes(x=X1, y=adjustedResidual)) +
  geom_point(shape=1) +
  stat_smooth(method='loess', se=FALSE) +
  stat_smooth(method='lm', se=FALSE, color='red') +
  theme_bw() +
  labs(title="prPlot for variable X1")
show(prPlot3)

avPlot3 <- ggplot(data=df, aes(x=r_X1, y=r_Z)) +
  geom_point(shape=1) +
  stat_smooth(method='loess', se=FALSE) +
  stat_smooth(method="lm", color='red', se=FALSE) +
  theme_bw() +
  labs(title="avPlot for variable X1 on partial model")

show(avPlot3)