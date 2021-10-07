plot(anscombe[,c("x2","y2")],ylim=c(0,15),pch=15)
fit=lm(y2~x2, data=anscombe)
summary(fit)
abline(fit,col="red")

fit2=lm(y2~x2+I(x2^2), data=anscombe)
summary(fit2)

fit22=lm(y2~poly(x2,2,raw=T), data=anscombe)
summary(fit22)

quadratic = fit2$coefficient[3]*anscombe$x2^2 + fit2$coefficient[2]*anscombe$x2 + fit2$coefficient[1]
quadratic

library(ggplot2)
ggplot(anscombe, aes(x=x2, y=y2)) + geom_point()+
  stat_smooth(se=F, method='lm', formula=y~poly(x,2))

##############################################
plot(anscombe[,c("x3","y3")])

fit.model=lm(y3~x3, data=anscombe)

cooksD <- cooks.distance(model)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential

## MODEL 2
fit.model2=lm(y3~x3, data=anscombe[-3,])
summary(fit.model2)


##############################################3
plot(anscombe[,c("x4","y4")])

fit4.model=lm(y4~x4, data=anscombe)
cooksD <- cooks.distance(fit4.model)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential

## MODEL 2
fit4.model2=lm(y4~x4, data=anscombe[-8,])
summary(fit4.model2)

abline(fit4.model,col="red",lty=2)
abline(h=coef(fit4.model2)[1],col="red")