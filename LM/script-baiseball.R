library(ISLR)
library(dplyr)

## Visualizing NA data
Amelia::missmap(Hitters)

Hitters <- na.omit(Hitters)
str(Hitters)

model <- lm(Salary ~ ., data = Hitters)
summary(model)

par(mfrow = c(2, 2))
plot(model)

cooksD <- cooks.distance(model)
plot(cooksD,type="h")
abline(h=3 * mean(cooksD, na.rm = TRUE),col="red",lty=2) #or 4/(N−k−1)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential

# Checking Y empirical distribution
plot(density(Hitters$Salary,adjust = 1))

## MODEL 2
names_of_influential <- names(influential)
outliers <- Hitters[names_of_influential,]
hitters_without_outliers <- Hitters %>% 
                            anti_join(outliers)

model2 <- lm(Salary ~ ., data = hitters_without_outliers)
summary(model2)
length(coef(model2))

par(mfrow = c(2, 2))
plot(model2)

## STEPWISE (BACKWARD, FOWARD, BOTH-DIRECTIONS)
model3 <- step(lm(Salary ~ ., data = hitters_without_outliers))
summary(model3)
length(coef(model3))

model4 <- step(lm(Salary ~ .^2, data = hitters_without_outliers))
summary(model4)
length(coef(model4))
length(coef(lm(Salary ~ .^2, data = hitters_without_outliers)))

AIC(model);AIC(model2);AIC(model3);AIC(model4)
BIC(model);BIC(model2);BIC(model3);BIC(model4)