# Objectives:
# 1) Demonstrate how to use logistic regression to make a
#    basic binary classifier. We'll measure performance 
#    using accuracy and look at accuracy on training data
#    as well as a held-out data set.
# 2) Use 'nnet' to fit a multinomial logistic regression,
#    so we can make a multi-class classifier.

library(mlbench)
data(Glass)
summary(Glass)

set.seed(1234)
holdoutIndices <- sample(x=seq.int(nrow(Glass)),
                         size=20,
                         replace=FALSE)

data <- within(Glass, {
  type1 <- as.logical(Type == "1")
  type6 <- as.logical(Type == "6")
  Type <- NULL
})

holdoutData <- data[holdoutIndices, ]
trainingData <- data[-holdoutIndices, ]

# we'll use logistic regression to find the
# probability that a particular piece of glass
# is of type 1
type1Classifier <- glm(type1 ~ .,
                       family="binomial",
                       data=trainingData[, -10])

# predict on the training data
predictedProbs <- predict(type1Classifier,
                          newdata=trainingData[,-10],
                          type="response")

# predict on the hold out data
predictedProbs2 <- predict(type1Classifier,
                          newdata=holdoutData[,-10],
                          type="response")

# use the decision rule that if a piece of class has
# probability of being type 1 >= 0.5, then we say it's
# type 1
type1ConfMat1 <- table((predictedProbs >= 0.5), trainingData$type1)
type1ConfMat1
# ^^ 118 True Negatives, 23 False Negatives
# 15 False Positives, 38 True Positives,
# accuracy = (TN + TP) / (TN + FN + FP + TP) = 80.4%

type1ConfMat2 <- table((predictedProbs2 >= 0.5), holdoutData$type1)
type1ConfMat2
# accuracy on hold-out set is 55%

sum(diag(type1ConfMat1)) / sum(type1ConfMat1)
sum(diag(type1ConfMat2)) / sum(type1ConfMat2)

# do this again but with type 6

type6Classifier <- glm(type6 ~ .,
                       family="binomial",
                       data=trainingData[, -11])

predictedProbs <- predict(type6Classifier,
                          newdata=trainingData[,-11],
                          type="response")

predictedProbs2 <- predict(type6Classifier,
                           newdata=holdoutData[,-11],
                           type="response")

type1ConfMat1 <- table((predictedProbs >= 0.5), trainingData$type6)
type1ConfMat1 # a ton of false positives

type1ConfMat2 <- table((predictedProbs2 >= 0.5), holdoutData$type6)
type1ConfMat2

sum(diag(type1ConfMat1)) / sum(type1ConfMat1) # 68.6% accurate
sum(diag(type1ConfMat2)) / sum(type1ConfMat2) # 75% accurate

# Let's consider multi-class classifiers, using
# multinomial logistic regression.

library(nnet)
# see ?multinom for documentation;
# multinom likes to have standardized predictors, so
# let's sweep over the columns of Glass that aren't "Type"
# and transform their output to [0,1]
X <- lapply(Glass[, -10], FUN=function(col) {
  (col - min(col)) / diff(range(col))
})

X <- as.data.frame(X)

multiModel <- multinom(Type ~ .,
                       data=data.frame(X,Type=Glass$Type),
                       maxit = 500,
                       trace = TRUE)

confMat1 <- table(predict(multiModel, X, type="class"),
                  Glass$Type)

confMat1
# ^^ does a good job with types 5,6,7
# bigger issues with 1,2,3.

sum(diag(confMat1)) / sum(confMat1) # 73% accurate

# redo this, but WITHOUT standardizing the columns

multiModel2 <- multinom(Type ~ .,
                       data=Glass,
                       maxit = 500,
                       trace = TRUE)

confMat2 <- table(predict(multiModel2,
                          Glass[,-10],
                          type="class"),
                  Glass$Type)

confMat2 
# ^^ pretty sloppy at differentiating
# between types 1 and 2.

sum(diag(confMat2)) / sum(confMat2) # 72% accurate

# TAKEAWAY: you may get better performance 
# if you standardize your variables; however,
# this is more of a consequence of neural nets,
# than anything else.
