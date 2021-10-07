# Packages
library(tidyverse)  # data manipulation and visualization
library(modelr)     # provides easy pipeline modeling functions
library(broom)      # helps to tidy up model outputs

## https://www.kaggle.com/purbar/advertising-data?select=Advertising.csv
# Load data (remove row numbers included as X1 variable)
advertising <- read.csv("https://raw.githubusercontent.com/ProfNascimento/GLM/main/LM/Advertising.csv")[,-1]
head(advertising)
dim(advertising)

summary(advertising)

ggplot(advertising, aes(TV, Sales)) +
  geom_point()

#Descriptive
psych::describe(advertising)
psych::pairs.panels(advertising,bg=c("yellow","blue")[(advertising$Sales > 20)+1],pch=21,stars=TRUE)

require(corrplot)
require(Hmisc)
M<-cor(advertising, method = "spearman")
cor_5 <- rcorr(as.matrix(advertising))
M <- cor_5$r
p_mat <- cor_5$P
corrplot(M, type = "upper", order = "hclust", method = "color", 
         p.mat = p_mat, sig.level = 0.05)

#Assessing Our Model Visually
ggplot(advertising, aes(TV, Sales)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_smooth(se = FALSE, color = "red",lty=2)

#Fitting a linear model
set.seed(123)

model1 <- lm(Sales ~ TV, data = advertising)
summary(model1)
confint(model1)

## Goodness-of-fit
sigma(model1) #Residual standard error (RSE)
rsquare(model1, data = advertising) #R2
cor(advertising$TV, advertising$Sales)^2

#STEPWISE
fullmodel <- step(lm(Sales ~ ., data = advertising))
