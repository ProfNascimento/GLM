hurricanes <- read.csv("https://raw.githubusercontent.com/ProfNascimento/GLM/main/LOGISTIC/hurricanes.csv")
str(hurricanes)

class(hurricanes$Type)
hurricanes$Type = factor(hurricanes$Type, levels = c(3,1,0))
hurricanes$Type = relevel(hurricanes$Type, "0")

require(nnet)
#Training the multinomial model
multinom_model2 <- multinom(Type ~ FirstLat, data = hurricanes, family = binomial(logit) )
#Checking the model
summary(multinom_model)

exp(coef(multinom_model))

head(round(fitted(multinom_model), 2))

#Predicting the values
y_hat <- predict(multinom_model, newdata = hurricanes, "class")
#Building classification table
tab <- table(hurricanes$Type, y_hat)
#Calculating accuracy
round((sum(diag(tab))/sum(tab))*100,2)