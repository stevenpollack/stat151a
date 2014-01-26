## Outline:
## 1. Review code that generate plots in lecture handouts
##  a) coplots
##  b) LOESS curves -- what and how
##  c) pairs plots (with annotation)
## 2. Do a bit of linear algebra review
##  a) basic matrix multiplication and inner product. I.e. matrix
##  multiplication can be viewed in terms of the columns or rows that
##  make up the matrix
##  b) operations: trace, transpose, inverse of a matrix;
##  c) positive (semi)-definite matrix; rank of matrix;
##  d) inverse of a matrix
##  e) orthogonal vectors and matrices
##  f) Expected values and variance of random vectors 
##  (e.g. E(Ax+c)= AE(x)+c, var(Ax+c)=Avar(x)A' ). 
##  g) Relationship of inverse of matrix to solving a system of linear
##  equations.

## R review:

## load Boston Housing Data

library(data.table)

column.names <- c("CRIM","ZN","INDUS","CHAS","NOX",
                  "RM","AGE","DIS","RAD","TAX","PTRATIO",
                  "B","LSTAT","MEDV")

housing.data <- data.table(read.table(file="Data/boston_housing_data.txt",
                                      header=FALSE,
                                      col.names=column.names))

## Consider the relationship between Crime Rate, Number of Rooms,
## % of Old Homes, a measure of diversity, % lower status, pupil:teacher and median value

pairs(MEDV ~ CRIM + RM + AGE + B + LSTAT + PTRATIO,
      data=housing.data,
      lower.panel=NULL)

library(ggplot2)
library(reshape2)

cor.dt <- melt(cor(housing.data[,list(MEDV,CRIM,RM,AGE,B,LSTAT,PTRATIO)]))

gg.heatmap <- ggplot(data=cor.dt,aes(x=Var1,y=Var2,fill=value))
gg.heatmap <- gg.heatmap + geom_tile() 
gg.heatmap <- gg.heatmap + geom_text(aes(label=round(value,3)),color='red')
gg.heatmap <- gg.heatmap + labs(x=NULL,y=NULL,title="Correlation Structure")

show(gg.heatmap)

## MEDV has strong correlation with LSTAT and RM...
## LSTAT and RM have a strong correlation, as well, though!

## Let's look at a coplot of MEDV, LSTAT, and RM:

## facets wrap from bottom to top, starting at the left...
## bottom-left corresponds to first "shingle", bottom-middle
## to second "shingle", and so forth...
coplot(MEDV ~ RM | LSTAT, data=housing.data)

## Conditioning on higher values of LSTAT seems to remove a lot of the
## correlation between MEDV and RM. However, a positive relationship
## seems to exist for lower values of observed LSTAT.

## Note that we can condition on 2 variables with coplot().
## Check out AGE, LSTAT, and RM
pairs(MEDV ~ AGE + LSTAT + RM, data=housing.data)
coplot(MEDV ~ AGE | LSTAT * RM, housing.data, show.given=FALSE)

## Let's turn things around and build our own coplot(s)
## Ignore the guidance that intervals should overlap...
housing.data[,RM.breaks := cut(RM,breaks=c(3,4,5,6,7,8,9))]
housing.data[,LSTAT.breaks:=cut_interval(LSTAT,n=3)]
housing.data[,AGE.breaks:=cut_interval(AGE,n=6)]

## Focus on MEDV ~ AGE | LSTAT * RM
melted.data <- melt(data=housing.data[,list(MEDV,AGE,
                                            RM.breaks,
                                            LSTAT.breaks,
                                            AGE.breaks)
                                      ],
                    id.vars=c("MEDV","RM.breaks",
                              "LSTAT.breaks", "AGE.breaks")) 

gg.coplot <- ggplot(data=melted.data, aes(x=value,y=MEDV,group=RM.breaks)) 
gg.coplot <- gg.coplot + geom_point() + facet_grid(RM.breaks~LSTAT.breaks,
                                                   scales="free")
gg.coplot <- gg.coplot + stat_smooth(formula=y~1,method="lm",
                                     color='red',
                                     lty=2,
                                     se=FALSE)
gg.coplot <- gg.coplot + labs(x="AGE",title="MEDV ~ AGE | LSTAT * RM")

## we can add various types of regression lines to this plot
show(gg.coplot + stat_smooth(method="lm", se=FALSE))

## ?loess shows we are locally fitting quadratics.
show(gg.coplot + stat_smooth(method="loess", se=FALSE, span=0.9))

## Note how some plots have few, or no, points?
## Welcome to the "curse of dimensionality".

## To get a feel for the bias-variance trade-off of the span= parameter
## let's look at MEDV ~ LSTAT
gg.loess <- ggplot(data=housing.data, aes(x=LSTAT,y=MEDV)) + geom_point()

lapply(X=c(0.1, 0.25,0.4,0.75),
       FUN=function(span) {
         tmp <- gg.loess + stat_smooth(method="loess",
                                       se=FALSE,
                                       span=span,
                                       size=1)
         show(tmp + labs(title=paste("Span = ", span)))
})

## Simulation: demonstrating Y ~ X | Z, when X and Z are (in)dependent.

## First case: X and Z are independent.
set.seed(1234)
simulation1.dt <- data.table(X=rnorm(n=100,mean=2),
                             Z=rnorm(n=100,mean=-1,sd=sqrt(2)),
                             noise=rnorm(100))
simulation1.dt[, Y1 := X + Z + noise ]

pairs(simulation1.dt)

coplot(Y1 ~ X | Z, data=simulation1.dt)
coplot(Y1 ~ Z | X, data=simulation1.dt)

## Second case: X and Z are dependent.
set.seed(1234)
simulation2.dt <- data.table(X=rnorm(n=100,mean=2),
                             noise=rnorm(100))
simulation2.dt[, Z := X + atan(X) ]
simulation2.dt[, Y2 := X + Z + noise ]

pairs(simulation2.dt)

## Conditioning on X removes most of Z's predictive power
coplot(Y2 ~ Z | X, data=simulation2.dt)
coplot(Y2 ~ X | Z, data=simulation2.dt)

