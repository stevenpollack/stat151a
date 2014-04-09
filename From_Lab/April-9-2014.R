library(faraway)
attach(diabetes)

# let's get some summary stats of the data:
# ?diabetes
summary(diabetes)

# note: 
#   1. bp.2s and bp.2d have 262 NA's... we should probably just remove
#      those columns.
#   2. Other columns have more reasonable numbers of NA's. Let's clean the data
#      up and just remove rows with NA's. 
#   3. id's is unique for each individual, so there's no point keeping that.
#   4. ratio is the ratio of chol/hdl, so for prediction purposes, we're going to
#      drop that too.

df <- diabetes[, !(colnames(diabetes) %in% c("id", "bp.2s", "bp.2d", "ratio"))]

df <- df[!apply(df, 1, function(x) any(is.na(x)) ), ] # 403 -> 366 observations

summary(df) # no more row's with NA's. 

set.seed(1234)
holdoutIndices <- sample(seq.int(nrow(df)), size=72, replace=FALSE)
holdoutData <- df[holdoutIndices, ]

df <- df[-holdoutIndices, ]

# normally we'd do some EDA, but for the sake of time, let's just explore the
# relationship between chol and the other variables
library(ggplot2)
library(reshape2)

melted.df <- melt(data=df, id.vars=c("chol", "location", "gender", "frame"))
responsePlot <- ggplot(data=melted.df, aes(x=value, y=chol)) +
  geom_point(shape=1, alpha=0.5) +
  stat_smooth(color='red', method='loess', se=FALSE, alpha=0.5) +
  stat_smooth(color='blue', method='lm', se=FALSE, alpha=0.5) +
  facet_wrap(~variable, scales="free_x", ncol=2) +
  labs(x=NULL, title=NULL) +
  theme_bw()

responsePlot # these all seem to be pretty crappy predictors of chol (not surprising)

# let's do some brute force subset selection:
library(leaps)
leaps(x=df[, colnames(df) != "chol"], y=df[, "chol"], nbest=3) # ERROR!

# leaps can't handle handle factor variables:
df2 <- df[, !(colnames(df) %in% c("gender", "location", "frame"))]
leaps(x=as.matrix(df2[, colnames(df2) != "chol"]), y=as.vector(df2[, "chol"]), nbest=3)

# one way around this is to re-incode df:
unrollFactorVar <- function(factorVar, factorVarName=names(factorVar)) {

  factorVarLevels <- levels(factorVar)
  
  out <- sapply(factorVarLevels, FUN=function(lvl){
    as.numeric(factorVar == lvl)
  })
  
  colnames(out) <-if (is.null(factorVarName)) {
    factorVarLevels
  } else {
    paste0(factorVarName, ".", factorVarLevels)
  }
  
  return(out)
}

df3 <- lapply(df, function(col) {
  if (inherits(col, "factor")) {
    unrollFactorVar(col)
  } else {
    col
  }
})

df3 <- as.data.frame(df3)

leaps(x=df3[, colnames(df3) != "chol"], y=df3[, "chol"], nbest=3) # ERROR again
leapsOutput <- regsubsets(chol ~ 0 + ., data=df3, nbest=10, nvmax=19)
leapsSummary <- summary(leapsOutput)

modelSize <- as.vector(sapply(seq.int(14), function(i) rep.int(i, 10)))
modelStats <- cbind(modelSize=modelSize, as.data.frame(leapsSummary[2:6]))

modelStatsExtrema <- as.data.frame(lapply(modelStats[-1],
                            function(col) {
                              c(min=modelStats$modelSize[which.min(col)],
                                max=modelStats$modelSize[which.max(col)])
                              }))

modelStatsExtrema <- merge(melt(modelStatsExtrema[1,], value.name="min"),
                           melt(modelStatsExtrema[2,], value.name="max"))

subsetStatsPlot <- ggplot(data=melt(modelStats, id.vars="modelSize"), 
                          aes(x=as.factor(modelSize), y=value)) +
  geom_boxplot() +
  geom_point(shape=1, alpha=0.25) +
  facet_wrap(~variable, scales="free_y") +
  geom_vline(data=modelStatsExtrema, aes(xintercept=min), color='blue') +
  geom_vline(data=modelStatsExtrema, aes(xintercept=max), color='red') +
  labs(x="Number of Predictors", y=NULL, title=NULL) +
  theme_bw()

subsetStatsPlot
modelStatsExtrema 
# Cp and adjR^2 both imply a 7 variable model is best
# not surprisingly, R^2 and RSS both imply full models.
# BIC says take a 5 variable model.

modelStats
# Check out models 69 and 70, and 41
colnames(df3[, leapsSummary$which[69, ]]) # leapsSummary$adjr2[69] == 0.1732
colnames(df3[, leapsSummary$which[51, ]]) # leapsSummary$cp[70] == 2.057817

# BIC chosen model:
colnames(df3[, leapsSummary$which[41, ]])

# ^ heavy overlap with the models above.


# build models
m69 <- lm(chol ~ 0 + . , data=df3[, leapsSummary$which[51, ]]) 
m70 <- lm(chol ~ 0 + . , data=df3[, leapsSummary$which[52, ]]) # <- has lower adjR^2


# these models were built using an exhaustive search. Let's see what kind of
# full model would have been made had we use LASSO, instead.
# see: http://www.stanford.edu/~hastie/glmnet/glmnet_alpha.html
library(glmnet)
library(doParallel)
registerDoParallel(cores=detectCores())

# note that alpha=1 <=> LASSO and alpha=0 <=> Ridge inside glmnet
cvLasso <- cv.glmnet(type.measure="mse", parallel=TRUE, nfolds=10, alpha=1,
                     x=as.matrix(df3[, colnames(df3) != "chol"]), y=df3[, "chol"])

plot(cvLasso)
colnames(df3)[as.logical(coef(cvLasso, s="lambda.min"))] # 10 predictors
colnames(df3)[as.logical(coef(cvLasso, s="lambda.1se"))] # 2 predictors
minLassoMSE <- cvLasso$cvm[which(cvLasso$lambda == cvLasso$lambda.min)] #1747

# build the models
lassoPreds <- predict(cvLasso, newx=as.matrix(df3[,-1]),
                      s=c(cvLasso$lambda.min, cvLasso$lambda.1se))

calcAdjR2 <- function(y_M, p) {
  y <- df3$chol
  n <- length(y)
  sigma2_hat <- var(y)
  1 - sum((y_M - y)^2) / ( (n - p - 1) * sigma2_hat )
}

calcAdjR2(y_M=lassoPreds[,1], p=10) # 0.1467
calcAdjR2(y_M=lassoPreds[,2], p=2) # 0.0467

# now consider ridge:

cvRidge <- cv.glmnet(type.measure="mse", parallel=TRUE, nfolds=10, alpha=0,
                     x=as.matrix(df3[, colnames(df3) != "chol"]), y=df3[, "chol"])

plot(cvRidge) # note that this does little model selection
coef(cvRidge, s="lambda.min")
coef(cvRidge, s="lambda.1se")
minRidgeMSE <- cvRidge$cvm[which(cvRidge$lambda == cvRidge$lambda.min)] #1664.5

ridgePreds <- predict(cvRidge, newx=as.matrix(df3[,-1]),
                      s=c(cvRidge$lambda.min, cvRidge$lambda.1se))

calcAdjR2(y_M=ridgePreds[,1], p=18) # 0.118
calcAdjR2(y_M=ridgePreds[,2], p=18) # -0.065

## test these models on the holdout data

holdoutData <-lapply(holdoutData, function(col) {
  if (inherits(col, "factor")) {
    unrollFactorVar(col)
  } else {
    col
  }
}) 
holdoutData <- as.data.frame(holdoutData)

lassoPreds <- predict(cvLasso, newx=as.matrix(holdoutData[,-1]),
                      s=c(cvLasso$lambda.min, cvLasso$lambda.1se))


ridgePreds <- predict(cvRidge, newx=as.matrix(holdoutData[,-1]),
                      s=c(cvRidge$lambda.min, cvRidge$lambda.1se))

MSE_hat <- within(data.frame(lasso.Min=0), {
  lasso.Min <- mean( (lassoPreds[,1] - holdoutData$chol)^2 )
  lasso.1se <-  mean( (lassoPreds[,2] - holdoutData$chol)^2 )
  ridge.Min <- mean( (ridgePreds[,1] - holdoutData$chol)^2 )
  ridge.1se <-  mean( (ridgePreds[,2] - holdoutData$chol)^2 )
#   m69 <- mean( (predict(m69, newdata=holdoutData) - holdoutData$chol)^2 )
#   m70 <- mean( (predict(m70, newdata=holdoutData) - holdoutData$chol)^2 )
})

msePlot <- ggplot(data=melt(MSE_hat), aes(x=variable, y=value, size=1/value, fill=value)) +
  geom_point(shape=21) +
  theme_bw() +
  labs(x="model", y="MSE", title=NULL) +
  scale_fill_gradient("MSE", high="blue", low="red")

msePlot

detach(diabetes)
