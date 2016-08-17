
#------------------------------------------------Classification Tree----------------------------------------------------#
library(tree)

Spam=read.table(file='spam.data.txt',sep=" ")
summary(Spam)
dim(Spam)
set.seed(101)
train = sample(dim(Spam)[1], dim(Spam)[1]/2)
spam.train = Spam[train, ]
spam.test =Spam[-train, ]
tree.spam = tree(V58 ~ ., data = spam.train)

plot(tree.spam)
text(tree.spam, pretty = 0)

pred.spam = predict(tree.spam, spam.test)
mean((spam.test$V58- pred.spam)^2)

cv.spam = cv.tree(tree.spam, FUN = prune.tree)
plot(cv.spam$size, cv.spam$dev, type = "b")
cv.spam$size
cv.spam$dev

pruned.spam = prune.tree(tree.spam, best = 9)
pred.pruned = predict(pruned.spam, spam.test)
#mean((spam.test$V58 - pred.pruned)^2)

plot(pruned.spam)

text(pruned.spam, pretty = 0)

#----------------------------------------------------Rpart Algorithm------------------------------------------------------#
# library(rpart)
# 
# tree.mod1 = rpart(V58 ~ ., data = spam.train, method = "class", cp = 0.01)
# plot(tree.mod1, margin = 0.05)
# text(tree.mod1, use.n = TRUE)
# tree.mod1
# 
# pred.spam = predict(tree.mod1, spam.test)
# mean((spam.test$V58- pred.spam)^2)
# 
# 
# tree.mod2 = rpart(V58 ~ ., data = spam.train, method = "class", cp = 0, 
#                   xval = 10)
# pred.spam = predict(tree.mod2, spam.test)
# mean((spam.test$V58- pred.spam)^2)
# 
# plot(tree.mod2)
# 
# printcp(tree.mod2)
# 
# pruned.tree.mod2 = prune(tree.mod2, cp = 0.0006)
# plot(pruned.tree.mod2, margin = 0.05)
# text(pruned.tree.mod2, use.n = TRUE)
# pred.spam = predict(pruned.tree.mod2, spam.test)
# mean((spam.test$V58- pred.spam)^2)

#_________________________________------------------------------------Boost---------------_____________________________________#
library(gbm)
Spam=read.table(file='spam.data.txt',sep=" ")
summary(Spam)
dim(Spam)
set.seed(101)
train = sample(dim(Spam)[1], dim(Spam)[1]/2)
spam.train = Spam[train, ]
spam.test =Spam[-train, ]

boost.spam = gbm(V58~., data =spam.train, distribution="bernoulli",n.trees =5000 , interaction.depth =4)
summary(boost.spam)
par ( mfrow =c(1 ,1) )
plot(boost.spam ,i="V53")
plot(boost.spam ,i="V52")
boost.prob = predict(boost.spam, spam.test, n.trees = 1000)
boost.pred = ifelse(boost.prob > 0.49, 1, 0)
table(spam.test$V58, boost.pred)
######################################## Linear model


lm.spam = glm(V58 ~ ., data = spam.train, family = binomial)
lm.prob = predict(lm.spam, spam.test, type = "response")
lm.pred = ifelse(lm.prob > 0.2, 1, 0)
table(spam.test$V58, lm.pred)

#--------------------------------------------------------------Random Forest-----------------------------------------------
library (randomForest)
library(ElemStatLearn)
summary(spam)
dim(spam)
train = sample(dim(spam)[1], dim(spam)[1]/2)
spam.train = spam[train, ]
spam.test =spam[-train, ]


set.seed (1)
bag.spam =randomForest(spam~.,data=spam ,subset =train , mtry=57, importance =TRUE)
bag.spam
yhat.bag = predict (bag.spam ,newdata =spam.test)
table(spam.test$spam, yhat.bag)


bag.spam =randomForest(spam~.,data=spam ,subset =train , mtry=19, ntree=25)
bag.spam
yhat.bag = predict (bag.spam ,newdata =spam.test)
table(spam.test$spam, yhat.bag)
importance (bag.spam)
varImpPlot (bag.spam)

#--------------------------------------------------------------SVM-----------------------------------------------
library(ElemStatLearn)
summary(spam)
dim(spam)
set.seed(101)
train = sample(dim(spam)[1], dim(spam)[1]/2)
spam.train = spam[train, ]
spam.test =spam[-train, ]


library(e1071)
svm.linear = svm(spam ~ ., kernel = "linear", data = spam.train, cost = 0.01)
summary(svm.linear)

train.pred = predict(svm.linear, spam.train)
table(spam.train$spam, train.pred)

test.pred = predict(svm.linear, spam.test)
table(spam.test$spam, test.pred)

# tune.out = tune(svm, spam ~ ., data = spam.train, kernel = "linear")
# summary(tune.out)
# 
# svm.linear = svm(spam ~ ., kernel = "linear", data = spam.train, cost = tune.out$best.parameters$cost)
# train.pred = predict(svm.linear, spam.train)
#table(spam.train$spam, train.pred)

#test.pred = predict(svm.linear, spam.test)
#table(spam.test$spam, test.pred)


svm.linear = svm(spam ~ ., kernel = "radial", data = spam.train, cost = 0.01)
summary(svm.linear)

train.pred = predict(svm.linear, spam.train)
table(spam.train$spam, train.pred)

test.pred = predict(svm.linear, spam.test)
table(spam.test$spam, test.pred)


svm.linear = svm(spam ~ ., kernel = "poly", data = spam.train, cost = 0.01)
summary(svm.linear)

train.pred = predict(svm.linear, spam.train)
table(spam.train$spam, train.pred)

test.pred = predict(svm.linear, spam.test)
table(spam.test$spam, test.pred)





