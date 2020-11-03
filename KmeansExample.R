library(cluster)
library(factoextra)
library(fpc)
x = data.frame(c(1,6,10,12,3,20,21,11,26))

k2 = kmeans(x, centers = 2)
str(k2)
k2
k2$cluster
k2$centers
clusplot(x, k2$cluster)

