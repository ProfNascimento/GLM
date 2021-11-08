## LOGISTIC DENSITY FUNC ##

# DENSITY FUNC SHAPE
plot(dlogis(seq(-7,7,0.01), location = 0, scale = 1, log = FALSE))
# CUMULATIVE FUNC
plot(plogis(x, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE))
# QUANTILE FUNC
plot(qlogis(seq(0,1,0.01), location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE))

## BINARY REGRESSION [ODDS IN (0,1)] ##
#https://www.geo.fu-berlin.de/en/v/soga/Basics-of-statistics/Logistic-Regression/Logistic-Regression-in-R---An-Example/index.html
# build a model that predicts the group membership of a hurricane, 
# either tropical or non-tropical, based on the latitude of formation.

hurricanes <- read.csv("~/Downloads/hurricanes.csv")
str(hurricanes)

library(ggplot2)
ggplot(hurricanes, aes(x = Year)) + geom_bar(stat = "count")

ggplot(hurricanes, aes(x = Year, fill = factor(Type))) +
  geom_bar(stat = "count") +
  scale_fill_discrete(name = "Type of Hurricane",
                      labels = c("tropical-only", "baroclinic influences", "baroclinic initiation"))

hurricanes.table <- table(hurricanes$Type)
hurricanes.table 

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

## FIT MODEL
log.model <- glm(Type.new ~ FirstLat, data = hurricanes, family = 'binomial')
summary(log.model)

summary(log.model)$coefficients

exp(coefficients(log.model)[2])

confint.default(log.model)[2,]
exp(confint.default(log.model)[2,])

predict(log.model, newdata = list(FirstLat = c(10, 23.5,30)), type = "response")

##
lats = seq(min(hurricanes$FirstLat), max(hurricanes$FirstLat), 0.1)

probs = predict(log.model, 
                newdata = data.frame(FirstLat = lats), 
                type = "response", 
                se.fit = TRUE)

pm = probs$fit
pu = probs$fit + probs$se.fit * 1.96 # 95% confidence interval
pl = probs$fit - probs$se.fit * 1.96 # 95% confidence interval

plot(hurricanes$FirstLat, 
     hurricanes$Type.new, 
     pch = 16, 
     cex = 1, 
     ylab = "Probability", 
     xlab = "Formation Latitude (N)")

grid()

polygon(c(rev(lats),lats), c(rev(pl),pu),
        col = "grey90", border = NA)

lines(lats, pm, lwd = 2)
lines(lats, pu, lwd = 2, col = "red")
lines(lats, pl, lwd = 2, col = "red")

abline(h=0.1, lty=2)
abline(h=0.5, lty=2)
abline(h=0.9, lty=2)
