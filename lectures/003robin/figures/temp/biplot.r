data(iris)
attach(iris)

##### PRINCOMP VERSION - UNTRANSFORMED
iris.prcomp = prcomp(iris[, c(1:4)], scale=T, center=T)
print(iris.prcomp)
summary(iris.prcomp)

op=par(mfrow=c(1,1))

data(iris); attach(iris)
iris.prcomp = prcomp(iris[, c(1:4)], scale=T, center=T)

pdf (file="irisbiplot.pdf", width=9, height=6)
biplot(iris.prcomp, col=c("black", "red"), xlim=c(-0.15, 0.2))
abline(h=0); abline(v=0)

dev.off()
