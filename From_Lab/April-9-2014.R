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
leapsOutput <- regsubsets(chol ~ ., data=df3, nbest=10, nvmax=19)
leapsSummary <- summary(leapsOutput)

modelSize <- as.vector(sapply(seq.int(15), function(i) rep.int(i, 10)))
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
colnames(df3[, leapsSummary$which[69, ]])
colnames(df3[, leapsSummary$which[70, ]])

# ^^ pretty much same models, except one considers Louisa instead of Buckingham

colnames(df3[, leapsSummary$which[41, ]])

# heavy overlap with the models above.

# do LASSO and compare models.
# do RIDGE and compare models.
# do step-wise regression 


ggplot() + geom_point(data=modelStatsExtrema, aes(x=min,y=max)) +  geom_hline(data=modelStatsExtrema, aes(x=min), color='blue') +
  facet_wrap(~variable, scales="free_y")
detach(diabetes)
