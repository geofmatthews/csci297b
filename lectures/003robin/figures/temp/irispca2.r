data(iris)
attach(iris)

iris.prcomp = prcomp(iris[, c(1:4)], scale=T, center=T)
print(iris.prcomp)
summary(iris.prcomp)

op=par(mfrow=c(1,1))

data(iris); attach(iris)
iris.prcomp = prcomp(iris[, c(1:4)], scale=T, center=T)

pdf(file="irisPRCOMP2.pdf", width=9, height=6)
plot(iris.prcomp$x,
     pch=c(21,22,24)[unclass(iris$Species)],
     bg=c("pink", "violet", "purple")[unclass(iris$Species)],
     cex=1.5)

abline(h=0); abline(v=0)

legend(x="topright", c("I. setosa", "I. versicolor", "I. virginica"),
     pch=c(21, 22, 24), pt.bg=c("pink", "violet", "purple"),
     bty="n", cex=1)

dev.off()

