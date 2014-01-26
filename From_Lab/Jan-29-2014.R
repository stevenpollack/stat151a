## Outline:
## 1. Review code that generate plots in lecture handouts
##  a) coplots
##  b) LOESS curves -- what and how
##  c) pairs plots (with annotation)
## 2. Do a bit of linear algebra review
##  a) basic matrix multiplication and inner product. I.e. matrix
##  multiplication can be viewed in terms of the columns or rows that make up
##  the matrix
##  b) operations: trace, transpose, inverse of a matrix;
##  c) positive (semi)-definite matrix; rank of matrix;
##  d) inverse of a matrix
##  e) orthogonal vectors and matrices
##  f) Expected values and variance of random vectors (e.g. E(Ax+c)= AE(x)+c,
##  var(Ax+c)=Avar(x)A' ). 
##  g) Relationship of inverse of matrix to solving a system of linear
##  equations.

######
# On Thursday, we got through deriving the least squares regression solution (see the handout I uploaded on bspace 'for Instructors' section). I hope to finish the simple regression handout next Tuesday, so that Thursday I can move to multiple regression in matrix format. I will post the first HW this weekend. All points in all homeworks will be added up for the final score; most homeworks will be similar length/difficulty, but it will just depend.
# 
# I am thinking for next Wednesday, I would like you to go over some R details about the plots I've shown in class -- how to do what I did, but also how to do a bit more -- and some linear algebra review. I skipped section 5 ('An Extended Example') in the first handout, so you could go through that example too, but this is already a lot of material for a single section. Use your judgement, and some of it might have to continue over until the next week.
# 
# These are my thoughts about the plots:
# 
# 1) coplots (see 1st handout, they are graphical techniques for seeing the relationship of one variable conditioned on the value of another variable). There was a great deal of confusion as to what was being plotted in a coplot, and I'm not sure my example was particularly good in retrospect. In particular in my coplot, the variable that I conditioned on was broken into 6 pieces by the functions, and so the 6 corresponding scatterplots were wrapped around and it was hard to see how they related to the 6 bins. The example in the help of coplot would be better where there are the 4 bins in a clear order.  
# 
# Furthermore, my example showed two explanatory variables that were highly correlated, so when you conditioned on one there was no relationship left over. Which was my goal. But I realize that it would be helpful for them to also see what would happen if the two predictor variables were not related. You could simulate independent normal predictors and a Y that was linearly related to both of them, and show a coplot and then contrast it with if the two predictors were highly correlated, etc. Also, it can be used with 3 predictors (i.e. condition on two predictor variables, and plot against the third), which I sort of implied in lecture couldn't be done, so that was a mistake on my part. You can add 1 more variable. There's an example in the help.
# 
# In short, spend some time explaining this plot, because it is quite a complicated plot, they won't have seen it before (unlike the other plots) and there are a lot of options. And I think it's pretty nice way to look at multiple variables. 
# 
# 2) Loess curves -- review slightly what the plot is doing -- it's discussed in the book -- and show them how if you change the bandwidth it changes the smoothness of the plot. I try to emphasis that there is not a cure-all, you are always making choices.
# 3) Pairs plots -- pretty straightforward, but show them how you can add a fitted regression line or loess curve on top of the pairs (see the help file, you need to set panel=).
# 4) I think boxplots and histograms are pretty straightforward, but you could go over density curves if you feel inclined.
# 
# For all of these, I think you should be able to either use the examples from class (data and code is on bspace) or from the help files. 
#####
# Regarding linear algebra, I've put on bspace ('for Instructors') an old 'cheat sheet' I had made when I was a GSI that you could give them; for an exercise to get them practicing, you could tell them to verify some of these facts on the sheet. I've put the latex file up so you can add to it. Please make sure all vectors are column vectors; in other words, if you want a 1xp vector, write x' where x is a px1 vector. Off the top of my head, I think the things they most need to get a review on:
#   
#   1) basic matrix multiplication and inner product. But more importantly, like I have on the cheat sheet, might be to discuss how the matrix multiplication can be viewed in terms of the columns or rows that make up the matrix
# 2) operations: trace, transpose, inverse of a matrix; positive (semi)-definite matrix; rank of matrix; most importantly that inverse of a matrix is not inverse of the elements or anything simple like that -- usually use a computer to determine. 
# 3) orthogonal vectors and matrices
# 4) Expected values and variance of random vectors (e.g. E(Ax+c)= AE(x)+c ; var(Ax+c)=Avar(x)A' ). 
# 5) Relationship of inverse of matrix to solving a system of linear equations.
#There are some slides from someone else (sorry, don't have the .tex file) in the 'forInstructors' that goes over some major points in this regard. Also, if you take any multivariate statistics book (e.g. Mardia, Kent and Bibby or Anderson etc) they usually have an appendix with all this information in it. That would be good to copy and put online for students as a reference. If you don't have such a book, I have several.
# 
# Clearly you can spend forever on the linear algebra, so don't go overboard. It is suppose to be a review. Some things you'll review as they come up over the semester (e.g. eigenvalues are not critical until much later in the course). But familiarity with matrix inverse is critical right away.

#####

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
housing.data[,LSTAT.breaks:=cut_interval(LSTAT,n=6)]
housing.data[,AGE.breaks:=cut_interval(AGE,n=6)]

melted.data <- melt(data=housing.data[,list(MEDV,LSTAT,RM.breaks)],
                    id.vars=c("MEDV","RM.breaks")) 

gg.coplot <- ggplot(data=melted.data, aes(x=value,y=MEDV,group=RM.breaks)) 
gg.coplot <- gg.coplot + geom_point() + facet_grid(RM.breaks~.,
                                                   scales="free_y")
gg.coplot <- gg.coplot + stat_smooth(formula=y~1,method="lm",
                                     color='red',
                                     lty=2,
                                     se=FALSE)
gg.coplot <- gg.coplot + labs(x="LSTAT")

## we can add various types of regression lines to this plot
show(gg.coplot + stat_smooth(method="lm", se=FALSE))

## ?loess shows we are locally fitting quadratics.
show(gg.coplot + stat_smooth(method="loess", se=FALSE, span=0.9))

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
