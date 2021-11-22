####################
## SYNTHETIC DATA ##
####################
set.seed(9)
sampDB=rpois(100,lambda = 4)

# MLE of the POISSON PARAMETER
mean(sampDB)

# ALTERNATIVLY GLM WITH AN INTERCEPT
fit.model=glm(sampDB~1,family = "poisson")
summary(fit.model)
exp(fit.model$coef[1])

# LAW OF LARGE NUMBER IS APPLIED
sampDB2=rpois(10000,lambda = 4)
exp(glm(sampDB2~1,family = "poisson")$coef)


#######################################################
## EXAMPLE 01-Number of cargo ships damaged by waves ##
##----------- (McCullagh & Nelder, 1989) ------------##
#######################################################
library(MASS); data(ships)
shipsf = ships;
names(shipsf)
str(shipsf)

# Make as factors
shipsf$type = factor(shipsf$type)
shipsf$year = factor(shipsf$year)
shipsf$period = factor(shipsf$period)

hist(shipsf$incidents/shipsf$service)

library(GGally)
ggpairs(shipsf, columns = c("incidents","type","year",
                            "period"))

# Fit a model
mod1 = glm(incidents ~ type + year + period
           + offset(log1p(service)), family=poisson,
           control=glm.control(epsilon=0.0001,maxit=100),
           data=shipsf)

# Checking the assumption [VAR(Y) = E(Y)]
pchisq(mod1$deviance,mod1$df.residual,lower.tail = FALSE)

summary(mod1)

require(jtools)
# plot regression coefficients for mod1
plot_summs(mod1, scale = TRUE, exp = TRUE)
cat_plot(mod1, pred = period, modx = type)

## WITHOUT OFFSET
poisson.model2=glm(incidents ~ type + year + period, family=poisson,
                control=glm.control(epsilon=0.0001,maxit=100),
                data=shipsf)
summary(poisson.model2) #Violation VAR(Y) not equal E(Y)
plot_summs(mod1, poisson.model2, scale = TRUE, exp = TRUE)


## RESIDUAL ANALYSIS
# Suppose your fitted model is modresiduals(mod,type = "response") ## response residuals
residuals(mod1,type = "pearson") ## pearson residuals
residuals(mod1,type = "deviance") ## deviance residuals
residuals(mod1,type = "deviance") ## pearson residuals

# residuals against estimates (mu_i) first
plot(residuals(mod1) ~ predict(mod1,type="response"),xlab=expression(hat(mu)),ylab="Deviance residuals",pch=20,col="red")
#that most points are squeezed at the left side of the plot, 
#which makes it hard to interpret. 
#Therefore, it’s better to check that without the link function’s transformation.

plot(residuals(mod1) ~ predict(mod1,type="link"),xlab=expression(hat(eta)),ylab="Deviance residuals",pch=20,col="blue")
#expect a constant variation in the plot because the deviance 
#residuals should not have the nonconstant variation that is 
#already rescaled out.

## OUTLIERS DETECTION
i_n = influence(mod1)$hat # calculate the influence of data points
which.max(i_n)
shipsf[9,]

c_d = cooks.distance(mod1)
which.max(c_d)
shipsf[30,]

#QQ plot in GLM diagnostics because the residuals are not 
#expected to be normally distributed. The only purpose of 
#the QQ plot in GLM is to find the outliers in the data.
faraway::halfnorm((i_n)) #leverages
faraway::halfnorm((c_d)) #Cook's distance

# RESIDUAL CHECKING (UNITED)
plot(mod1)

# make a dataframe with new data
newdata = data.frame(type="B", year="65", period="60",service=1000)
# use 'predict()' to run model on new data
predict(mod1, newdata = newdata, type = "response")


######################
## REAL APPLICATION ##
######################
require(datasets)
data < - warpbreaks
#https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/warpbreaks.html

names(data) # Extract column names from dataframe
str(data)

hist(data$breaks)

#library(GGally)
#ggpairs(data, columns = c("breaks","wool","tension"))

mean(data$breaks) # calculate mean
var(data$breaks) # calculate variance

# model poisson regression using glm()
poisson.model <- glm(breaks ~ wool + tension, data, family = poisson(link = "log"))
summary(poisson.model)

pchisq(poisson.model$deviance,
       poisson.model$df.residual,lower.tail = FALSE)

poisson.model2 <- glm(breaks ~ wool + tension, data = data, family = quasipoisson(link = "log"))
summary(poisson.model2)
