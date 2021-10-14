library(ggplot2)
## TWO PARAMETER MODEL --VISUAL--
ggplot(anscombe, aes(x=x1, y=y1)) + geom_point()+
  stat_smooth(se=F, method='lm')

dim(anscombe[,c("x1","y1")])
## SATURATED (FULL) MODEL --VISUAL--
ggplot(anscombe, aes(x=x1, y=y1)) + geom_point()+
  stat_smooth(se=F, method='lm', formula=y~poly(x,10))

full=lm(y1~poly(x1,10),data=anscombe) # FULL MODEL
summary(full) # ERROR NON-EXISTENT
