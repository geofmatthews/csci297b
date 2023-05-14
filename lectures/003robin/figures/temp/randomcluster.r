##### Randomization Testing - part !
##### Create a Randomized Data File:
data(iris)
attach(iris)

for (i in 2:5) {iris[,i] = sample(iris[,i])}
write.table(iris, file="riris.csv", quote=FALSE, row.names=FALSE, sep=",")

riris = read.csv("riris.csv", T)
attach(riris)

kmeans.riris = kmeans(riris[, c(1:4)], 3)
print(table(riris$Species, kmeans.riris$cluster))
print(chisq.test(riris$Species, kmeans.riris$cluster))


