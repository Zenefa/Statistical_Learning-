library(ISLR)
library(MASS)
fix(Carseats)
names(Carseats)
attach(Carseats)
summary(Carseats)

lm.fit = lm(Sales~Price+US)
summary(lm.fit)

summary(lm.fit)$r.sq
summary(lm.fit)$sigma

lm.fit2 = lm(Sales ~ Price + US)
summary(lm.fit2)

confint(lm.fit2)

plot(predict(lm.fit2), rstudent(lm.fit2))

plot ( hatvalues ( lm.fit2) )
which.max(hatvalues(lm.fit2))











par(mfrow=c(2,2))
plot(lm.fit2)


