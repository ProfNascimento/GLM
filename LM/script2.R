## MODEL 1
# Descriptive
summary(anscombe[,c("y1","x1")])
cor(anscombe$y1,anscombe$x1)
plot(anscombe$x1,anscombe$y1,pch=16,xlab="X1",ylab="Y1",main="Anscombe Set #1")

# Checking Y empirical distribution
plot(density(anscombe$y1,adjust = 1))

# LEAST-SQUARED (Y1~X1)
(fit.model <- lm(y1~x1, data = anscombe))
coef(fit.model)
summary(fit.model)

plot(anscombe$x1,anscombe$y1,pch=16,xlab="X1",xlim=c(0,14),ylim=c(0,11),
     ylab="Y1",main="Anscombe Set #1")
abline(fit.model,col="red")

3.0001/1.1247 #b0 Estimate / SE(b0)
0.5001/0.1179 #b1 Estimate / SE(b1)

pt(2.667467,9) #b0 t-statistic 

# T-DISTRIBUTION (DEGREE 9)
plot(seq(-4,4,0.01),dt(seq(-4,4,0.01),9),type="l",ylim=c(0,0.45),
     xlab="x",ylab="f(x)")
abline(v=0,lwd = 2.5)
text(0.15,0,expression(beta[0]))
text(0,0.43,"NULL HYPOTHESIS")
# NORM DISTRIBUTION
lines(seq(-4,4,0.01),dnorm(seq(-4,4,0.01)),type="l",lty=2,col="gray",add=T)
legend("topright",c("t DIST","NORM DIST"),col=c("black","gray"),lty=c(1,2))

qt(0.975,9) #UPPER LIMIT 97.5%
abline(v=2.262157,col="red",lty=3) #ACCEPTENCE NULL HYPOTHESYS REGION
qt(0.025,9) #LOWER LIMIT 2.5%
abline(v=-2.262157,col="red",lty=3) #ACCEPTENCE NULL HYPOTHESYS REGION
# PROBABILITY OF b0 BEEN DRAW FROM A T-DISTRIBUTION WITH MEAN ZERO (b0=0)
points(2.667,0,col="red",pch=15) 
text(2.667,0.035,expression(widehat(beta[0])),col="red")

#PROBABILITY OF b0 EQUAL OR HIGHER THAN 3.0001
1-pt(2.667467,9)

# CONSIDERING BOTH SIDES TEST => P-VALUE
2*(1-pt(2.667467,9))

## DOING ALL THE LM COEF AT ONCE
sqrt(diag(vcov(fit.model))) #SE of the lm.model
df.residual(fit.model) #FREEDOM DEGREE
2 * (1-pt(coef(fit.model)/sqrt(diag(vcov(fit.model))),
          df.residual(fit.model))) # P-VALUE

# COEF(b0,b1) --MATRIX FORM--
X=cbind(rep(1,length(anscombe$x1)),anscombe$x1) #Design Matrix
solve(t(X)%*%X)%*%t(X)%*%anscombe$y1

# LEAST-ABSOLUTE
fm <- lad(y1~x1, data = anscombe, method = "BR")
summary(fm)

plot(anscombe$x1,anscombe$y1)
abline(fm,col="red")
abline(fit.model,col="red",lty=2)

################################################################################
## MODEL 2
## Page 9: Plant Weight Data.
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)

boxplot(weight~group)

lm.D9 <- lm(weight ~ group)
lm.D90 <- lm(weight ~ group - 1) # omitting intercept

anova(lm.D9)
summary(lm.D9)

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm.D9, las = 1)      # Residuals, Fitted, ...
par(opar)


################################################################################
## MODEL 3 (https://www.theanalysisfactor.com/r-tutorial-4/)
#One way of checking for non-linearity in your data is to fit a polynomial 
#model and check whether the polynomial model fits the data better than a 
#linear model. However, you may also wish to fit a quadratic or higher model 
#because you have reason to believe that the relationship between the variables
#is inherently polynomial in nature. Let’s see how to fit a quadratic model in R.
A <- structure(list(Time = c(0, 1, 2, 4, 6, 8, 9, 10, 11, 12, 13, 
                             14, 15, 16, 18, 19, 20, 21, 22, 24, 25, 26, 27, 28, 29, 30), 
                    Counts = c(126.6, 101.8, 71.6, 101.6, 68.1, 62.9, 45.5, 41.9, 
                               46.3, 34.1, 38.2, 41.7, 24.7, 41.5, 36.6, 19.6, 
                               22.8, 29.6, 23.5, 15.3, 13.4, 26.8, 9.8, 18.8, 25.9, 19.3)), .Names = c("Time", "Counts"),
               row.names = c(1L, 2L, 3L, 5L, 7L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 19L, 20L, 21L, 22L, 23L, 25L, 26L, 27L, 28L, 29L, 30L, 31L),
               class = "data.frame")

attach(A);names(A)

linear.model <-lm(Counts ~ Time)
summary(linear.model)

plot(Time, Counts, pch=16, ylab = "Counts ", cex.lab = 1.3, col = "red" )
abline(lm(Counts ~ Time), col = "blue")

quadratic.model <-lm(Counts ~ Time + I(Time^2))
summary(quadratic.model)

timevalues <- seq(0, 30, 0.1)

predictedcounts <- predict(quadratic.model,list(Time=Time, Time2=Time^2))
plot(Time, Counts, pch=16, xlab = "Time (s)", ylab = "Counts", cex.lab = 1.3, col = "blue")
lines(Time, predictedcounts, col = "darkgreen", lwd = 3)
