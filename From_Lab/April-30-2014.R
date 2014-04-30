data(iris)

summary(iris)

df <- iris
df$Species <- as.numeric(df$Species == "setosa")

glmFit <- glm(formula=Species ~ ., data=df, family="binomial")

predictions <- as.numeric(predict(glmFit, type="response") > 0.5)

table(df$Species, predictions)

glm(formula=Species ~ 1, data=df, family="binomial")
