library(car)
library(ggplot2)
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
