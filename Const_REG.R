library(GLMsData)
require(ggplot2)

data(lime); str(lime)

ggplot(lime, aes(x=DBH, y=Foliage, color=Origin))+
  geom_point() + facet_grid(~Origin)

ggplot(lime, aes(x=log(DBH), y=log(Foliage), color=Origin))+
  geom_point() + facet_grid(~Origin)

ggplot(lime, aes(x=DBH, y=Foliage, color=Origin))+
  geom_point() + facet_grid(Age~Origin)

#TESTING the presence or absence of heteroscedasticity
#null hypothesis that the variance of the residuals is constant (homocedasticity)
lmMod <- lm(Foliage ~ Origin + Age, data=lime) # initial model
lmtest::bptest(lmMod)  # Breusch-Pagan test

car::ncvTest(lmMod)  # Breusch-Pagan test

## CHECKING THE DEGREE OF RELATION V(μ)
coef(lm(log(lime$Foliage)~log(lime$Age)))

glmMod <- glm(Foliage ~ Origin + Age, data=lime,family = Gamma(link="log"))
summary(glmMod)

# residual deviance test (NO lack of fit, if alpha=0.05)
1-pchisq(glmMod$deviance/summary(glmMod)$dispersion,
         glmMod$df.residual, lower.tail = FALSE)

require(jtools)
# plot regression coefficients for mod1
plot_summs(glmMod, scale = TRUE, exp = TRUE,ci_level = 0.95)

library(effects)
plot(allEffects(glmMod,transformation=list(link=log, inverse=exp)))

## Visualizing the GLM model
ggplot(lime, aes(x=DBH, y=Foliage,col=Origin)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family=Gamma(link="log")))

## Comparing LM vs GLM w/ Multiplicative Error Variance
ggplot(lime, aes(x=DBH, y=Foliage,col=Origin)) +
  geom_point() + facet_grid(~Origin) +
  geom_smooth(method = "glm", 
              method.args = list(family=Gamma(link="log")))+
  geom_smooth(method = "lm", se = FALSE)

