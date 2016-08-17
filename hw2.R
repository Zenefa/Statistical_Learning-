set.seed (1)
y=rnorm (100)
x=rnorm (100)
y=x-2* x^2+ rnorm (100)
y
n<-100
p<-2
plot(x, y)

library(boot)
Data = data.frame(x, y)
set.seed(1)
# i.
glm.fit = glm(y ~ x)
coef(glm.fit)
error<-cv.glm(Data, glm.fit)$delta
#ii
fit.glm.2 <- glm(y ~ poly(x, 2))
error<-cv.glm(Data, fit.glm.2)$delta
error

#iii
fit.glm.3 <- glm(y ~ poly(x, 3))
error<-cv.glm(Data, fit.glm.3)$delta
error

#iv

fit.glm.4 = glm(y ~ poly(x, 4))
error<-cv.glm(Data, fit.glm.4)$delta
error

Data = data.frame(x, y)
set.seed(100)
# i.
glm.fit = glm(y ~ x)
summary(glm.fit)
coef(glm.fit)
error<-cv.glm(Data, glm.fit)$delta
error
#ii
fit.glm.2 <- glm(y ~ poly(x, 2))
summary(fit.glm.2)
error<-cv.glm(Data, fit.glm.2)$delta
error

#iii
fit.glm.3 <- glm(y ~ poly(x, 3))
summary(fit.glm.3)
error<-cv.glm(Data, fit.glm.3)$delta
error

#iv

fit.glm.4 = glm(y ~ poly(x, 4))
summary(fit.glm.4)
error<-cv.glm(Data, fit.glm.4)$delta
error
#________________________________________________________

set.seed(1)
X = rnorm(100)
noise = rnorm(100)
X
noise

b0 = 1
b1 = 2
b2 = -1
b3 = -2 
Y = b0 + b1 * X + b2 * X^2 + b3 * X^3 + noise



library(leaps)
data = data.frame(y = Y, x = X)
model= regsubsets(y ~ poly(x, 10, raw = T), data = data, nvmax = 10)
model.summary = summary(model)

model.summary
# Find the model size for best cp, BIC and adjr2
which.min(model.summary$cp)
which.min(model.summary$bic)
which.max(model.summary$adjr2)
plot(model.summary$cp, xlab = "Subset Size", ylab = "Cp", pch = 20, type = "l")
points(which.min(model.summary$cp), model.summary$cp[which.min(model.summary$cp)], pch = 1, col = "red", lwd = 7)
plot(model.summary$bic, xlab = "Subset Size", ylab = "BIC", pch = 20, type = "l")
points(which.min(model.summary$bic), model.summary$bic[(which.min(model.summary$bic))], pch = 2, col = "red", lwd = 7)
plot(model.summary$adjr2, xlab = "Subset Size", ylab = "Adjusted R2", pch = 20, type = "l")
points(which.max(model.summary$adjr2), model.summary$adjr2[which.max(model.summary$adjr2)], pch = 3, col = "red", lwd = 5)
coefficients(model, id = 3)


model.fwd = regsubsets(y ~ poly(x, 10, raw = T), data = data, nvmax = 10, method = "forward")
fwd.summary = summary(model.fwd)
which.min(fwd.summary$cp)
which.min(fwd.summary$bic)
which.max(fwd.summary$adjr2)
plot(fwd.summary$cp, xlab = "Subset Size", ylab = "Cp", pch = 20, type = "l")
points(which.min(fwd.summary$cp), fwd.summary$cp[which.min(fwd.summary$cp)], pch = 1, col = "red", lwd = 7)
plot(fwd.summary$bic, xlab = "Subset Size", ylab = "BIC", pch = 20, type = "l")
points(which.min(fwd.summary$bic), fwd.summary$bic[(which.min(fwd.summary$bic))], pch = 2, col = "red", lwd = 7)
plot(fwd.summary$adjr2, xlab = "Subset Size", ylab = "Adjusted R2", pch = 20, type = "l")
points(which.max(fwd.summary$adjr2), fwd.summary$adjr2[which.max(fwd.summary$adjr2)], pch = 3, col = "red", lwd = 5)
model.bwd = regsubsets(y ~ poly(x, 10, raw = T), data = data, nvmax = 10, method = "backward")

bwd.summary = summary(model.bwd)
which.min(bwd.summary$cp)
which.min(bwd.summary$bic)
which.max(bwd.summary$adjr2)
plot(bwd.summary$cp, xlab = "Subset Size", ylab = "Cp", pch = 20, type = "l")
points(which.min(bwd.summary$cp), bwd.summary$cp[which.min(bwd.summary$cp)], pch = 1, col = "red", lwd = 7)
plot(bwd.summary$bic, xlab = "Subset Size", ylab = "BIC", pch = 20, type = "l")
points(which.min(bwd.summary$bic), bwd.summary$bic[(which.min(bwd.summary$bic))], pch = 4, col = "red", lwd = 7)
plot(bwd.summary$adjr2, xlab = "Subset Size", ylab = "Adjusted R2", pch = 20, type = "l")
points(which.max(bwd.summary$adjr2), bwd.summary$adjr2[which.max(bwd.summary$adjr2)], pch = 1, col = "red", lwd = 5)
model.bwd = regsubsets(y ~ poly(x, 10, raw = T), data = data, nvmax = 10, method = "backward")
#----------------------------------------------------------------------------------------------------
library(glmnet)
xmat = model.matrix(y ~ poly(x, 10, raw = T), data = data)[, -1]
model.lasso = cv.glmnet(xmat, Y, alpha = 1)
opt.lambda = model.lasso$lambda.min
opt.lambda
plot(model.lasso)


opt.model = glmnet(xmat, Y, alpha = 1)
lasso.pred<-predict(opt.model, s = opt.lambda, type = "coefficients")
mean(( lasso.pred -Y)^2)



b7 = 3
Y = b0 + b7 * X^7 + noise
# Predict using regsubsets
data = data.frame(y = Y, x = X)
model = regsubsets(y ~ poly(x, 10, raw = T), data = data, nvmax = 10)
model.summary = summary(model)
model.summary
# Find the model size for best cp, BIC and adjr2
which.min(model.summary$cp)
coefficients(model, id = which.min(model.summary$cp))
which.min(model.summary$bic)
coefficients(model, id = which.min(model.summary$bic))
which.max(model.summary$adjr2)
coefficients(model, id = which.max(model.summary$adjr2))

plot(model.summary$cp, xlab = "Subset Size", ylab = "Cp", pch = 20, type = "l")
points(which.min(model.summary$cp), model.summary$cp[which.min(model.summary$cp)], pch = 1, col = "red", lwd = 7)
plot(model.summary$bic, xlab = "Subset Size", ylab = "BIC", pch = 20, type = "l")
points(which.min(model.summary$bic), model.summary$bic[(which.min(model.summary$bic))], pch = 2, col = "red", lwd = 7)
plot(model.summary$adjr2, xlab = "Subset Size", ylab = "Adjusted R2", pch = 20, type = "l")
points(which.max(model.summary$adjr2), model.summary$adjr2[which.max(model.summary$adjr2)], pch = 3, col = "red", lwd = 5)
coefficients(model, id = 3)



xmat = model.matrix(y ~ poly(x, 10, raw = T), data = data)[, -1]
model.lasso = cv.glmnet(xmat, Y, alpha = 1)
opt.lambda = model.lasso$lambda.min
opt.lambda
plot(model.lasso)
opt.model = glmnet(xmat, Y, alpha = 1)
predict(opt.model, s = opt.lambda, type = "coefficients")


library(ISLR)
set.seed(10)
sum(is.na(College))

train.size = dim(College)[1] / 2
train = sample(1:dim(College)[1], train.size)
test = (-train)
College.train = College[train, ]
College.test = College[test, ]
College.train
College.test 
summary(College)
lm.fit = lm(Apps~., data=College.train)
lm.pred = predict(lm.fit, College.test)
mean((College.test[, "Apps"] - lm.pred)^2)

library(glmnet)

train.mat = model.matrix(Apps~., data=College.train)
test.mat = model.matrix(Apps~., data=College.test)
grid = 10 ^ seq(10, -2, length=100)
mod.ridge = cv.glmnet(train.mat, College.train[, "Apps"], alpha=0, lambda=grid, thresh=1e-12)
lambda.best = mod.ridge$lambda.min
lambda.best


ridge.pred = predict(mod.ridge, newx=test.mat, s=lambda.best)
mean((College.test[, "Apps"] - ridge.pred)^2)

model.lasso = cv.glmnet(train.mat, College.train[, "Apps"], alpha=1, lambda=grid, thresh=1e-12)
lambda.best = model.lasso$lambda.min
lambda.best

lasso.pred = predict(model.lasso, newx=test.mat, s=lambda.best)
mean((College.test[, "Apps"] - lasso.pred)^2)

mod.lasso = glmnet(model.matrix(Apps~., data=College), College[, "Apps"], alpha=1)
predict(mod.lasso, s=lambda.best, type="coefficients")

library(pls)

pcr.fit = pcr(Apps~., data=College.train, scale=T, validation="CV")
validationplot(pcr.fit, val.type="MSEP")

pcr.pred = predict(pcr.fit, College.test, ncomp=16)
mean((College.test[, "Apps"] - data.frame(pcr.pred))^2)


pls.fit = plsr(Apps~., data=College.train, scale=T, validation="CV")
validationplot(pls.fit, val.type="MSEP")


pls.pred = predict(pls.fit, College.test, ncomp=8)
mean((College.test[, "Apps"] - data.frame(pls.pred))^2)


test.avg = mean(College.test[, "Apps"])
lm.test.r2 = 1 - mean((College.test[, "Apps"] - lm.pred)^2) /mean((College.test[, "Apps"] - test.avg)^2)
ridge.test.r2 = 1 - mean((College.test[, "Apps"] - ridge.pred)^2) /mean((College.test[, "Apps"] - test.avg)^2)
lasso.test.r2 = 1 - mean((College.test[, "Apps"] - lasso.pred)^2) /mean((College.test[, "Apps"] - test.avg)^2)
pcr.test.r2 = 1 - mean((College.test[, "Apps"] - data.frame(pcr.pred))^2) /mean((College.test[, "Apps"] - test.avg)^2)
pls.test.r2 = 1 - mean((College.test[, "Apps"] - data.frame(pls.pred))^2) /mean((College.test[, "Apps"] - test.avg)^2)

barplot(c(mean((College.test[, "Apps"] - lm.pred)^2),mean((College.test[, "Apps"] - ridge.pred)^2),mean((College.test[, "Apps"] - lasso.pred)^2),mean((College.test[, "Apps"] - data.frame(pcr.pred))^2),mean((College.test[, "Apps"] - data.frame(pls.pred))^2)), names.arg=c("Linear", "Ridge", "Lasso", "PCR", "PLS"), main="Test Error")

