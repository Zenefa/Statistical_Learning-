library(ISLR)
attach(Carseats)
set.seed(1)

train = sample(dim(Carseats)[1], dim(Carseats)[1]/2)
Carseats.train = Carseats[train, ]
Carseats.test = Carseats[-train, ]


library(tree)
tree.carseats = tree(Sales ~ ., data = Carseats.train)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty = 0)
pred.carseats = predict(tree.carseats, Carseats.test)
mean((Carseats.test$Sales - pred.carseats)^2)
cv.carseats = cv.tree(tree.carseats, FUN = prune.tree)

plot(cv.carseats$size, cv.carseats$dev, type = "b")


tree.min <- which.min(cv.carseats$dev)
points(tree.min, cv.carseats$dev[tree.min], col = "red", cex = 2, pch = 20)
plot(cv.carseats$k, cv.carseats$dev, type = "b")

pruned.carseats = prune.tree(tree.carseats, best = 9)

plot(pruned.carseats)
text(pruned.carseats, pretty = 0)

pred.pruned = predict(pruned.carseats, Carseats.test)
mean((Carseats.test$Sales - pred.pruned)^2)
m$CompPrice=10
new <- data.frame(CompPrice=102,Incom=102, Income=87, Advertising=10, Population = 346,  Price=70,ShelveLoc='Mediun',Age=64,Education=15,Urban='Yes',US='Yes')
pred.pruned = predict(pruned.carseats, new)



data <- read.csv("simdata.csv")
data <- subset(data, select = -c(X) )
names(data)
row.names(data)
apply ( data , 2, mean )
apply ( data , 2, var )

pr.out =prcomp ( data , scale =TRUE)
names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation 
dim (pr.out$x )
biplot (pr.out , scale =0)

pr.out$rotation=-pr.out$rotation
pr.out$x =-pr.out$x
biplot (pr.out , scale =0)
pr.var =pr.out$sdev ^2
pr.out$sdev
pve =pr.var /sum (pr.var )
 pve
 
plot(pve,xlab="Principal Component", ylab =" Proportion of Variance Explained",
      ylim=c(0 ,1),type="b")

plot( cumsum (pve ), xlab =" Principal Component ", ylab ="Cumulative Proportion of Variance Explained ",
      ylim=c(0 ,1) , type="b")
