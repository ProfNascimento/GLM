## LOGISTIC DENSITY FUNC ##

# DENSITY FUNC SHAPE
plot(dlogis(seq(-7,7,0.01), location = 0, scale = 1, log = FALSE))
# CUMULATIVE FUNC
plot(plogis(seq(-7,7,0.01), location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE))
# QUANTILE FUNC
plot(qlogis(seq(0,1,0.01), location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE))

## BINARY REGRESSION [ODDS IN (0,1)] ##
#https://www.geo.fu-berlin.de/en/v/soga/Basics-of-statistics/Logistic-Regression/Logistic-Regression-in-R---An-Example/index.html
# build a model that predicts the group membership of a hurricane
# either tropical or non-tropical, based on the latitude of formation

hurricanes <- read.csv("https://raw.githubusercontent.com/ProfNascimento/GLM/main/LOGISTIC/hurricanes.csv")
str(hurricanes)

library(ggplot2)
ggplot(hurricanes, aes(x = Year)) + geom_bar(stat = "count")

ggplot(hurricanes, aes(x = Year, fill = factor(Type))) +
  geom_bar(stat = "count") +
  scale_fill_discrete(name = "Type of Hurricane",
                      labels = c("tropical-only", "baroclinic influences", "baroclinic initiation"))

# RESPONSE - 3 CLASSES
(hurricanes.table <- table(hurricanes$Type))

# RESPONSE - 2 CLASSES
hurricanes$Type.new  <- ifelse(test = hurricanes$Type==0, yes = 0, no = 1)
table(hurricanes$Type.new)

options(viewer=NULL)
library(leaflet)
m <- leaflet()
m <- addTiles(m)
m <- addProviderTiles(m, "Esri.OceanBasemap")

cols <- c("red", "navy")
m <- addCircleMarkers(m, 
                      lng = hurricanes$FirstLon, 
                      lat = hurricanes$FirstLat,
                      radius = 2.5,
                      color = cols[hurricanes$Type.new+1],
                      popup = paste('Year:', as.character(hurricanes$Year)))
m <- addLegend(m,
               "topright", 
               colors = cols, 
               labels = c('tropical', 'non-tropical'),
               title = "Type of Hurricane",
               opacity = 1)
m

## FIT MODEL -- log(TROPICAL/BARO) --
log.model <- glm(Type.new ~ FirstLat, data = hurricanes, family = binomial(logit) )
summary(log.model)

log.model2 <- glm(Type.new ~ FirstLat, data = hurricanes, family = binomial(link = probit))
summary(log.model2)

log.model3 <- glm(Type.new ~ FirstLat, data = hurricanes, family = binomial(link = cloglog))
summary(log.model3)

colors <- c("LOGIT" = "blue", "PROBIT" = "red", "cLOG-LOG" = "green")
ggplot(hurricanes, aes(FirstLat,Type.new)) + 
  geom_point(size = 2) + 
  geom_smooth(method = "glm", se = F, aes(color="LOGIT"), 
              method.args = list(family =binomial(link=logit)))+ 
  geom_smooth(method = "glm", se = F, aes(color="PROBIT"), lty=4,
              method.args = list(family =binomial(link=probit)))+ 
  geom_smooth(method = "glm", se = F, aes(color="cLOG-LOG"), 
              method.args = list(family =binomial(link=cloglog)))+ 
  labs(x = "LATITUDE (X1)",
       y = expression(paste("Probability (",pi,")")),
       color = "Legend") + scale_color_manual(values = colors)


## INTERPRETATION COEF FROM THE LOGIT REG
list( log.model$coefficient, 
      round( 1 - ( log.model$deviance / log.model$null.deviance ), 2 ) )

summary(log.model)$coefficients
exp(coefficients(log.model)[2])

confint.default(log.model)[2,]
exp(confint.default(log.model)[2,])

predict(log.model, newdata = list(FirstLat = c(10, 23.5,30)), type = "response")

## TRANSFORMING PROB INTO CLASSES
mod_pred=ifelse(log.model$fitted.values>0.5,1,0)

require('caret')
#Creates vectors having data points
y <- factor(hurricanes$Type.new)
y_hat <- factor(mod_pred)

#Creating confusion matrix
example <- confusionMatrix(data=y_hat, reference = y)
example

###
mod_pred=ifelse(log.model$fitted.values>0.25,1,0)


library(ROCit)

ROCit_obj <- rocit(score=log.model$fitted.values, class=expected_value)
plot(ROCit_obj)
