##################################################
###### Find inderkredsen ! #######################

setwd("~/My Dropbox/R/interlocks/")

# Hvis du ikke har installeret cluster pakken så kør:
install.packages("cluster")

library(ggplot2)
library(cluster)
library(igraph)


# Download data om indsomhederne
# Windows
download.file("https://raw.github.com/antongrau/soc.sna/master/Organisation_BIQ_top.csv", destfile="Organisation_BIQ_top.csv")
download.file("https://raw.github.com/antongrau/soc.sna/master/Relation_BIQ_top.csv", destfile="Relation_BIQ_top.csv")
download.file("https://raw.github.com/antongrau/soc.sna/master/soc.sna.R", destfile="soc.sna.r")
download.file("https://raw.github.com/antongrau/soc.sna/master/net.inner.R", destfile="net.inner.R")

# Mac og Linux
download.file("https://raw.github.com/antongrau/soc.sna/master/Organisation_BIQ_top.csv", destfile="Organisation_BIQ_top.csv", method="curl")
download.file("https://raw.github.com/antongrau/soc.sna/master/Relation_BIQ_top.csv", destfile="Relation_BIQ_top.csv", method="curl")
download.file("https://raw.github.com/antongrau/soc.sna/master/soc.sna.R", destfile="soc.sna.r", method="curl")
download.file("https://raw.github.com/antongrau/soc.sna/master/net.inner.R", destfile="net.inner.R", method="curl")

source("soc.sna.R")

rel         <- read.csv("Relation_BIQ_top.csv", sep="|", encoding="UTF-8")
org         <- read.csv("Organisation_BIQ_top.csv", sep="|", encoding="UTF-8")

netmat <- data.frame(rel$NAVN, rel$ORG_NAVN)
colnames(netmat) <- c("navn", "org")

tabnet          <- table(netmat)
tabnet          <- as.matrix(tabnet)
adj.ind         <- tabnet%*%t(tabnet) # Individ*individ
diagonal.ind    <- diag(adj.ind)
diag(adj.ind)   <- 0

# Her smider vi alle der ikke har mere end 1 virksomheds tie.
bridges         <- diagonal.ind > 1
adj.ind         <- adj.ind[bridges, bridges]
diagonal        <- diagonal.ind[bridges]
net.bridge      <- graph.adjacency(adj.ind, weighted=TRUE)

# Her finder vi den største component
com            <- clusters(net.bridge)
largest.com    <- which.max(com$csize) == com$membership
net.com        <- net.bridge - which(largest.com == FALSE)
diagonal.com   <- diagonal[largest.com]
adj.com        <- adj.ind[largest.com, largest.com]

# Her finder vi inderkredsen
circle3        <- circles(net.com, 3, "total")
clust3         <- agnes(circle3)
k20            <- cutree(clust3, k=20)
inner.circle   <- which(k20 == 1)
adj.inner      <- adj.com[inner.circle, inner.circle]
diagonal.inner <- diagonal.com[inner.circle]
net.inner      <- graph.adjacency(adj.inner, weighted=TRUE)


save(net.inner, diagonal.inner, adj.inner, file="inner.circle")
save(net.com, diagonal.com, adj.com, file="inner.component")













