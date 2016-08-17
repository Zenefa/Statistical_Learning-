library(ISLR)

## Problem : Chapter 8 - Ex 9 ##

attach(OJ)
summary(OJ)
dim(OJ)
set.seed(10)

## a ##

train = sample(dim(OJ)[1], 800)
OJ.train = OJ[train, ]
OJ.test = OJ[-train, ]

## b ##

library(tree)
oj.tree = tree(Purchase ~ ., data = OJ.train)
summary(oj.tree)

## c ##
oj.tree

## D ##

plot(oj.tree)
text(oj.tree, pretty = 0)

## E ##

oj.pred = predict(oj.tree, OJ.test, type = "class")
table(OJ.test$Purchase, oj.pred)

## F ##

cv.oj = cv.tree(oj.tree, FUN = prune.tree)

## g ##

plot(cv.oj$size, cv.oj$dev, type = "b", xlab = "Tree Size", ylab = "Deviance")

## h ##

## i ##

oj.pruned = prune.tree(oj.tree, best = 6)

## j ##


summary(oj.pruned)

## k ##

pred.unpruned = predict(oj.tree, OJ.test, type = "class")
misclass.unpruned = sum(OJ.test$Purchase != pred.unpruned)
misclass.unpruned/length(pred.unpruned)
                                                                           


pred.pruned = predict(oj.pruned, OJ.test, type = "class")
misclass.pruned = sum(OJ.test$Purchase != pred.pruned)
misclass.pruned/length(pred.pruned)


## Problem : Chapter 8 - Ex 11 ##
library(ISLR)

## a ##

library(ISLR)
train = 1:1000
Caravan$Purchase = ifelse(Caravan$Purchase == "Yes", 1, 0)
Caravan.train = Caravan[train, ]
Caravan.test = Caravan[-train, ]
library(gbm)
## b ##

set.seed(1)
set.seed(342)
boost.caravan = gbm(Purchase ~ ., data = Caravan.train, n.trees = 1000, shrinkage = 0.01, distribution = "bernoulli")

summary(boost.caravan)
## c ##

boost.prob = predict(boost.caravan, Caravan.test, n.trees = 1000, type = "response")
boost.pred = ifelse(boost.prob > 0.2, 1, 0)
#Form a confusion matrix.
a.table<-table(Caravan.test$Purchase, boost.pred)
#What fraction of the people predicted to make a purchase
a.table[4][1]/(a.table[4][1]+a.table[3][1])


lm.caravan = glm(Purchase ~ ., data = Caravan.train, family = binomial)
lm.prob = predict(lm.caravan, Caravan.test, type = "response")
lm.pred = ifelse(lm.prob > 0.2, 1, 0)
b.table<-table(Caravan.test$Purchase, lm.pred)
b.table[4][1]/(b.table[4][1]+b.table[3][1])

#---------------Chapter 9: Exercise 8---------------------------------------------------------#


library(ISLR)
set.seed(1)
train = sample(dim(OJ)[1], 800)
OJ.train = OJ[train, ]
OJ.test = OJ[-train, ]

library(e1071)
svm.linear = svm(Purchase ~ ., kernel = "linear", data = OJ.train, cost = 0.01)
summary(svm.linear)


train.pred = predict(svm.linear, OJ.train)
table(OJ.train$Purchase, train.pred)


test.pred = predict(svm.linear, OJ.test)
table(OJ.test$Purchase, test.pred)


set.seed(100)
tune.out = tune(svm, Purchase ~ ., data = OJ.train, kernel = "linear", ranges = list(cost = 10^seq(-2, 
                                                                                                   1, by = 0.25)))
summary(tune.out)

svm.linear = svm(Purchase ~ ., kernel = "linear", data = OJ.train, cost = tune.out$best.parameters$cost)
train.pred = predict(svm.linear, OJ.train)
table(OJ.train$Purchase, train.pred)

test.pred = predict(svm.linear, OJ.test)
table(OJ.test$Purchase, test.pred)


#f#

set.seed(10)
svm.radial = svm(Purchase ~ ., data = OJ.train, kernel = "radial")
summary(svm.radial)
train.pred = predict(svm.radial, OJ.train)
table(OJ.train$Purchase, train.pred)

test.pred = predict(svm.radial, OJ.test)
table(OJ.test$Purchase, test.pred)

set.seed(100)
tune.out = tune(svm, Purchase ~ ., data = OJ.train, kernel = "radial", ranges = list(cost = 10^seq(-2, 
                                                                                                   1, by = 0.25)))
summary(tune.out)
svm.radial = svm(Purchase ~ ., data = OJ.train, kernel = "radial", cost = tune.out$best.parameters$cost)


train.pred = predict(svm.radial, OJ.train)
table(OJ.train$Purchase, train.pred)

test.pred = predict(svm.radial, OJ.test)
table(OJ.test$Purchase, test.pred)



#G#

set.seed(10)
svm.poly = svm(Purchase ~ ., data = OJ.train, kernel = "poly", degree = 2)
summary(svm.poly)


train.pred = predict(svm.poly, OJ.train)
table(OJ.train$Purchase, train.pred)

test.pred = predict(svm.poly, OJ.test)
table(OJ.test$Purchase, test.pred)

set.seed(100)
tune.out = tune(svm, Purchase ~ ., data = OJ.train, kernel = "poly", degree = 2, 
                ranges = list(cost = 10^seq(-2, 1, by = 0.25)))
summary(tune.out)


svm.poly = svm(Purchase ~ ., data = OJ.train, kernel = "poly", degree = 2, cost = tune.out$best.parameters$cost)
train.pred = predict(svm.poly, OJ.train)
table(OJ.train$Purchase, train.pred)

test.pred = predict(svm.poly, OJ.test)
table(OJ.test$Purchase, test.pred)
(72 + 44)/(450 + 44 + 72 + 234)
(31 + 19)/(140 + 19 + 31 + 80)




data = read.csv("./Ch10Ex11.csv", header=F)
dim(data)

dd = as.dist(1 - cor(data))
plot(hclust(dd, method="complete"))

plot(hclust(dd, method="single"))
plot(hclust(dd, method="average"))

pr.out = prcomp(t(data))
summary(pr.out)

total_load = apply(pr.out$rotation, 1, sum)
indices = order(abs(total_load), decreasing=T)
indices[1:10]
total_load[indices[1:10]]
