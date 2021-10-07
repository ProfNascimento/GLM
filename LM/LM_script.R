
## https://www.kaggle.com/vedavyasv/usa-housing
# Reading data
housing <- read.csv("~/Desktop/GLM/LINEAR/USA_Housing.csv")[,-7]
str(housing)

# Print top 6 observations
head(housing)

summary(housing)

#Descritive
library(psych)
psych::describe(housing)

par(mfrow=c(2,3)) 
for(i in 1:dim(housing)[2]){
  boxplot(housing[,i],main=colnames(housing)[i])
}
dev.off()

library(reshape)
library(ggplot2)
meltData <- melt(housing)
ggplot(meltData, aes(x=factor(variable), y=value)) + 
  geom_boxplot() + facet_wrap(~variable, scale="free")
#geom_violin()

require(corrgram)
corrgram(housing, order=TRUE)

require(corrplot)
require(Hmisc)
M<-cor(housing,method = "pearson")
cor_5 <- rcorr(as.matrix(housing))
M <- cor_5$r
p_mat <- cor_5$P
corrplot(M, type = "upper", order = "hclust", method = "color", 
         p.mat = p_mat, sig.level = 0.05)

# Checking Y empirical distribution
library(ggplot2)
# Building histogram
ggplot(data=housing, aes(x=housing$Price)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

# Fitting a linear model
lmModel <- lm(Price ~ . , data = housing)
summary(lmModel) # Printing the model object

#Model2
lmModel2 <- lm(Price ~ `Avg..Area.Income`+`Avg..Area.House.Age`+
                `Avg..Area.Number.of.Rooms`+`Area.Population`, data = housing)
summary(lmModel2)

#Model2.1
lmModel21 <- lm(Price ~ . -`Avg..Area.Number.of.Bedrooms`, data = housing)
summary(lmModel21)

#StepWise Model
SwModel=step(lm(Price ~ . , data = housing),direction="both")
summary(SwModel)

AIC(lmModel);BIC(lmModel)
AIC(lmModel2);BIC(lmModel2)
AIC(SwModel);BIC(SwModel)

#------------------------------------------------------------------------#
fit <- lm(Price ~ `Avg..Area.Income`+`Avg..Area.House.Age`, data = housing)
summary(fit)

fit2 <- lm(Price ~ I(`Avg..Area.Income`*`Avg..Area.House.Age`), data = housing)
summary(fit2)

fit3 <- lm(Price ~ I(`Avg..Area.Income`^2)+I(`Avg..Area.House.Age`^4), data = housing)
summary(fit3)

fit4 <- lm(Price ~ (`Avg..Area.Income`+`Avg..Area.House.Age`+
                 `Avg..Area.Number.of.Rooms`)*(`Area.Population`), data = housing)
summary(fit4)


fit.nb2 <- MASS::stepAIC(lm(Price ~ . + I(`Avg..Area.Income`^2)+I(`Avg..Area.House.Age`^4), data = housing))
fit.nb2$anova
