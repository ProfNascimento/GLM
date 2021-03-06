library(tidyverse)

# Load the data and remove NAs
data("PimaIndiansDiabetes2", package = "mlbench")
str(PimaIndiansDiabetes2)
Amelia::missmap(PimaIndiansDiabetes2)

BD <- na.omit(PimaIndiansDiabetes2)
# Inspect the data
sample_n(BD, 3)

lm.fit=lm(as.numeric(diabetes) ~ glucose, data = BD)
summary(lm.fit)

ggplot(BD, aes(glucose, diabetes)) + geom_point() +
  geom_abline(intercept = 0.36564519, slope = 0.00787741, color="red", 
              linetype="dashed", size=1.5)+
  labs(x = "Plasma Glucose Concentration",
    y = "Dicotomic Response (being diabete)")


## Y in {0=neg, 1=pos} ~ BINOMIAL(p)
## INSTEAD GLM (PROB. OF THE POSITIVE GROUP)
model <- glm( diabetes ~ glucose, data = BD, family = binomial)
summary(model)$coef

BD %>%  mutate(prob = ifelse(diabetes == "pos", 1, 0)) %>%
ggplot(aes(glucose, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(title = "Logistic Regression Model", 
       x = "Plasma Glucose Concentration",
       y = "Probability of being diabete-pos") + 
  geom_vline(xintercept=143.7) +
  geom_hline(yintercept=0.5,lty=2) +
  geom_text(x=75, y=0.7, label="CLASS 2 = 'pos'") +
  geom_text(x=75, y=0.3, label="CLASS 1 = 'neg'")

x=143.7
p_logit=summary(model)$coef[1]+summary(model)$coef[2]*x
exp(p_logit)/(1+exp(p_logit))
