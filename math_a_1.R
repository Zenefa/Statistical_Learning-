library(ISLR)
summary(Carseats)
summary(Boston)

attach(Carseats)
 lm.fit = lm(Sales~Price+Urban+US)
summary(lm.fit)


lm.fit2 = lm(Sales ~ Price + US)
summary(lm.fit2)

confint(lm.fit2)

b2=lm.fit2$coef; b2
ab.0 = c(b2[1],b2[2]) ; ab.0
ab.1 = c(b2[1]+b2[3],b2[2]) ; ab.1
pchar = rep("+",length(Sales))
pchar[(US=="Yes")] = "o"
color = rep("blue",length(Sales))
color[(US=="Yes")] = "red"
plot(Price,Sales, xlim=c(0,200),ylim=c(0,20),col = color,pch=pchar)
abline(ab.0)
abline(ab.1)

plot(predict(lm.fit2), rstudent(lm.fit2))
plot(hatvalues(lm.fit2))
which.max(hatvalues(lm.fit2))
par(mfrow=c(2,2))
plot(lm.fit2)

#-----------------------------------------------------------------------------------------------------------------------
# 	Problem 10, page 171 (ISLR)
library(ISLR)
summary(Weekly)
plot(Weekly$Volume)
pairs(Weekly)
cor(Weekly[, -9])


attach(Weekly)
glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, 
              family = binomial)
summary(glm.fit)


glm.probs = predict(glm.fit, type = "response")
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction)



train = (Year < 2009)
Weekly.0910 = Weekly[!train, ]
glm.fit = glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
glm.probs = predict(glm.fit, Weekly.0910, type = "response")
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"
Direction.0910 = Direction[!train]
table(glm.pred, Direction.0910)
mean(glm.pred == Direction.0910)


library(MASS)
lda.fit = lda(Direction ~ Lag2, data = Weekly, subset = train)
lda.pred = predict(lda.fit, Weekly.0910)
table(lda.pred$class, Direction.0910)


qda.fit = qda(Direction ~ Lag2, data = Weekly, subset = train)
qda.class = predict(qda.fit, Weekly.0910)$class
table(qda.class, Direction.0910)

mean(qda.class == Direction.0910)


library(class)
train.X = as.matrix(Lag2[train])
test.X = as.matrix(Lag2[!train])
train.Direction = Direction[train]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.0910)
mean(knn.pred == Direction.0910)




glm.fit = glm(Direction ~ Lag2:Lag1, data = Weekly, family = binomial, subset = train)
glm.probs = predict(glm.fit, Weekly.0910, type = "response")
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"
Direction.0910 = Direction[!train]
table(glm.pred, Direction.0910)
mean(glm.pred == Direction.0910)

lda.fit = lda(Direction ~ Lag2:Lag1, data = Weekly, subset = train)
lda.pred = predict(lda.fit, Weekly.0910)
table(lda.pred$class, Direction.0910)
mean(lda.pred$class == Direction.0910)


qda.fit = qda(Direction ~ Lag2 + sqrt(abs(Lag2)), data = Weekly, subset = train)
qda.class = predict(qda.fit, Weekly.0910)$class
table(qda.class, Direction.0910)
mean(qda.class == Direction.0910)



knn.pred = knn(train.X, test.X, train.Direction, k = 50)
table(knn.pred, Direction.0910)
mean(knn.pred == Direction.0910)
#-----------------------------------------------------------------------------------------------------------------------

Power <-function() {
  a<- 2^3
  print(a)
}
Power()

Power2 <-function(x,a) {
  ans<-x^a
  print(ans)
}

Power3 <-function(x,a) {
  ans<-x^a
  return(ans)
}

x = 1:10
plot(x, Power3(x, 2), log = "xy", ylab = "y = x^2", xlab = "x", 
     main = "log of x^2 Vs log x")

PlotPower = function(x, a) {
  plot(x, Power3(x, a), ylab = "y = x^a", xlab = "x")
}
PlotPower(1:10, 3)
#-----------------------------------------------------------------------------------------------------------------------
rm(list = ls())

library(MASS)
summary(Boston)
attach(Boston)
names(Boston)

crime_rate = rep(0, dim(Boston)[1])
crime_rate[crim > median(crim)] = 1
Boston = data.frame(Boston, crime_rate)
x<-dim(Boston)
train<-1: (x[1]/2)

test<-(x[1]/2 + 1 ):x[1]
Boston.train=Boston[train,]
Boston.test=Boston[test,]
crime_rate.test=crime_rate[test]

glm.fit = glm(crime_rate ~ . - crime_rate - crim, data = Boston, family = binomial, 
              subset = train)
glm.probs = predict(glm.fit, Boston.test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
summary(glm.fit)
mean(glm.pred != crime_rate.test)
mean(glm.pred == crime_rate.test)

glm.fit = glm(crime_rate ~ . - crime_rate - crim -chas -tax, data = Boston, family = binomial, 
              subset = train)
summary(glm.fit)
glm.probs = predict(glm.fit, Boston.test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1

mean(glm.pred != crime_rate.test)
mean(glm.pred == crime_rate.test)

lda.fit = lda(crime_rate ~ . - crime_rate - crim, data = Boston, subset = train)
summary(lda.fit)

lda.pred = predict(lda.fit, Boston.test)
mean(lda.pred$class != crime_rate.test)
mean(lda.pred$class == crime_rate.test)

lda.fit = lda(crime_rate ~ . - crime_rate - crim -chas -tax, data = Boston, subset = train)
summary(lda.fit)

lda.pred = predict(lda.fit, Boston.test)
mean(lda.pred$class != crime_rate.test)
mean(lda.pred$class == crime_rate.test)


library(class)
train.X = cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, lstat, medv)[train, ]
test.X = cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, lstat, medv)[test, ]
train.crime_rate = crime_rate[train]
set.seed(1)

knn.pred = knn(train.X, test.X, train.crime_rate, k = 1)
table(knn.pred , crime_rate.test)
mean(knn.pred == crime_rate.test)
mean(knn.pred != crime_rate.test)


knn.pred = knn(train.X, test.X, train.crime_rate, k = 3)
table(knn.pred , crime_rate.test)
mean(knn.pred != crime_rate.test)
mean(knn.pred == crime_rate.test)

knn.pred = knn(train.X, test.X, train.crime_rate, k = 5)
table(knn.pred , crime_rate.test)
mean(knn.pred != crime_rate.test)
mean(knn.pred == crime_rate.test)

knn.pred = knn(train.X, test.X, train.crime_rate, k = 10)
table(knn.pred , crime_rate.test)
mean(knn.pred != crime_rate.test)
mean(knn.pred == crime_rate.test)

knn.pred = knn(train.X, test.X, train.crime_rate, k = 50)
table(knn.pred , crime_rate.test)
mean(knn.pred != crime_rate.test)
mean(knn.pred == crime_rate.test)

knn.pred = knn(train.X, test.X, train.crime_rate, k = 100)
table(knn.pred , crime_rate.test)
mean(knn.pred != crime_rate.test)
mean(knn.pred == crime_rate.test)

train.X = cbind(zn, indus, nox, rm, age, dis, rad, ptratio, black, lstat, medv)[train, ]
test.X = cbind(zn, indus, nox, rm, age, dis, rad, ptratio, black, lstat, medv)[test, ]
train.crime_rate = crime_rate[train]
set.seed(1)

knn.pred = knn(train.X, test.X, train.crime_rate, k = 1)
table(knn.pred , crime_rate.test)
mean(knn.pred == crime_rate.test)
mean(knn.pred != crime_rate.test)


knn.pred = knn(train.X, test.X, train.crime_rate, k = 3)
table(knn.pred , crime_rate.test)
mean(knn.pred != crime_rate.test)
mean(knn.pred == crime_rate.test)

knn.pred = knn(train.X, test.X, train.crime_rate, k = 5)
table(knn.pred , crime_rate.test)
mean(knn.pred != crime_rate.test)
mean(knn.pred == crime_rate.test)

knn.pred = knn(train.X, test.X, train.crime_rate, k = 10)
table(knn.pred , crime_rate.test)
mean(knn.pred != crime_rate.test)
mean(knn.pred == crime_rate.test)

knn.pred = knn(train.X, test.X, train.crime_rate, k = 50)
table(knn.pred , crime_rate.test)
mean(knn.pred != crime_rate.test)
mean(knn.pred == crime_rate.test)

knn.pred = knn(train.X, test.X, train.crime_rate, k = 100)
table(knn.pred , crime_rate.test)
mean(knn.pred != crime_rate.test)
mean(knn.pred == crime_rate.test)


#######################
# Exploring the Zip Code data set
#


pltdigit = function(xx){     # function to plot 16x16 digit image
  x = as.matrix(xx,ncol=16)  #forces numeric and sets dimension
  dim(x)<-c(16,16)	  # 
  y = x[1:16,16:1]   # vertical reflection
  image(1:16,1:16,z=y,zlim=c(-1,1),col=gray((32:0)/32))  #flip black/white
}

list.files()

# Remember to Change Dir to the one with data files

xx = read.table(file="ZIPCODES/zip.train.R",sep=" ")
dim(xx)

xx[1,]

xxx = xx[,c(-1,-258)]
dim(xxx[1,])

pltdigit(xxx[1,])

for (i in 1:20){
  par(ask=T)
  
  pltdigit(xxx[i,])
  
}
par(ask=F)

pr.out =prcomp ( train_data , scale =TRUE)


Spam=read.table(file='spam.data.txt',sep=" ")
summary(Spam)
dim(Spam)
pr.out =prcomp ( Spam , scale =TRUE)
dim(pr)