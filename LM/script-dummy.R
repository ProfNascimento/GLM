#######################################
### DUMMY EXAMPLE 1 - GPA <-> SAT
library(ggplot2)

SAT_set <- read.csv("~/Downloads/Dummy_set.csv")
str(SAT_set)
#tapply(SAT_set$GPA, SAT_set$Attendance,summary)
psych::describeBy(SAT_set, group="Attendance")

sp = ggplot(SAT_set, aes(y=GPA,x=SAT,color=Attendance)) + geom_point()

## Dummy Transformation
SAT_set$Att <- ifelse(SAT_set$Attendance == 'No', 0, 1)

fit.dummy=lm(GPA~SAT+Att,SAT_set)
summary(fit.dummy)

plot(fit.dummy)

sp + geom_abline(intercept = 0.6438, slope = as.numeric(coef(fit.dummy)[2]), color="red") +
  geom_abline(intercept = 0.8664945, slope = as.numeric(coef(fit.dummy)[2]), color="lightblue",size=1.5)

# https://365datascience.com/tutorials/statistics-tutorials/dummy-variable/

#######################################
### EXAMPLE 2
## IMPORT DATA
dataf <- read.csv('https://vincentarelbundock.github.io/Rdatasets/csv/carData/Salaries.csv')
str(dataf)

# Create dummy variables:
dataf$Disc_A <- ifelse(dataf$discipline == 'A', 1, 0)
dataf$Disc_B <- ifelse(dataf$discipline == 'B', 1, 0)

library(fastDummies)
dataf <- dummy_cols(dataf, select_columns = 'rank')

##---------- or ----------##
dataf.2 <- dummy_cols(dataf, select_columns = c('rank', 'discipline','sex'),
                      remove_selected_columns = TRUE)

str(dataf.2)

fit2.model=lm(salary~.,data=dataf.2)
summary(fit2.model)

# https://www.marsja.se/create-dummy-variables-in-r/