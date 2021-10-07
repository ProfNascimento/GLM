
## IMPORT SET
femur <- read.csv("https://raw.githubusercontent.com/ProfNascimento/GLM/main/LM/MulticollinearityExample.csv")[,-c(5,6,7)]
str(femur)

psych::pairs.panels(femur)

fit=lm(Femoral.Neck ~ X.Fat*Weight.kg + Activity,data=femur)
summary(fit)
car::vif(fit)

fit2=lm(Femoral.Neck ~ I(X.Fat-mean(X.Fat))*I(Weight.kg-mean(Weight.kg)) + I(Activity-mean(Activity)),data=femur)
summary(fit2)
car::vif(fit2)

## https://statisticsbyjim.com/regression/multicollinearity-in-regression-analysis/