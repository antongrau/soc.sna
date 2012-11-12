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

rel         <- read.csv("~/My Dropbox/Elite/Data/Data/Relation_BIQ_top_kapitalfonde.csv", sep="|", encoding="UTF-8")
org         <- read.csv("~/My Dropbox/Elite/Data/Data/Organisation_BIQ_top_kapitalfonde.csv", sep="|", encoding="UTF-8")

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

plot(clust3)


### 10
k10            <- cutree(clust3, k=10)
table(k10)
network.by.variable(net.com, k10)

net.del <- net.com - which(k10 != 4)
gplot(net.del)


## 25 Her ryger Ane Uggla
k25            <- cutree(clust3, k=25)
table(k25)

network.by.variable(net.com, k25==4)
net.del <- net.com - which(k25 != 4)
gplot(net.del)

## 32 
k32            <- cutree(clust3, k=32)
table(k32)

network.by.variable(net.com, k32==6)
net.del <- net.com - which(k32 != 6)
gplot(net.del)

a <- as.matrix(sort(degree(net.com)[k32==1]))
a
head(a)

who(net.com, "Jesper Mailind")

inder <- net.com - which(k32 != 1)
as.matrix(sort(degree(inder)))

## 58 Her ryger Anders Christen Obel AV!
k58            <- cutree(clust3, k=58)
table(k58)
network.by.variable(net.com, k58==4)
network.by.variable(net.com, k58==1)

net.del <- net.com - which(k58 != 4)
gplot(net.del)


as.matrix(sort(degree(net.com)[k58==4]))

## 


### Sammenfald med tidligere analyse

load("inner.circle")
net.del <- net.com - which(k32 != 1) 

navn.inner <- V(net.inner)$name
navn.ny    <- V(net.del)$name

sum(navn.ny %in% navn.inner) / length(navn.ny)
sum(navn.inner %in% navn.ny) / length(navn.inner)













