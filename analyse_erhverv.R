### Erhvervsorganisationer

setwd("~/My Dropbox/R/interlocks")


library(igraph)
library(ggplot2)
library(cluster)
source("soc.sna.R")

datmat <- read.csv("~/My Dropbox/Elite/Data/Data/Relation_FOA_NGO_erhverv.csv", sep="|", encoding="UTF-8")
netmat <- data.frame(datmat$NAVN, datmat$ORG_NAVN)
colnames(netmat) <- c("navn", "org")


tabnet   <- table(netmat)
tabnet   <- as.matrix(tabnet)
adj.org  <- t(tabnet)%*%tabnet # Virk*virk
adj.ind  <- tabnet%*%t(tabnet) # Individ*individ
diagonal.org  <- diag(adj.org)
table(diagonal.org)

sort(diagonal.org)

diag(adj.org) <- 0
diagonal.ind <- diag(adj.ind)
table(diagonal.ind)
diag(adj.ind) <- 0

net.org <- graph.adjacency(adj.org, weighted=TRUE)
net.ind <- graph.adjacency(adj.ind, weighted=TRUE)


lay.org <- layout.fruchterman.reingold(net.org)
gplot(net.org, lay.org, text.alpha=0)

as.matrix(sort(degree(net.org)))
as.matrix(sort(degree(net.ind)))

as.matrix(sort(betweenness(net.org)))
as.matrix(sort(closeness(net.org)))

as.matrix(sort(betweenness(net.ind)))
as.matrix(sort(closeness(net.ind)))

plot(sort(betweenness(net.ind)))
plot(sort(closeness(net.ind)))

com <- clusters(net.ind)
str(com)
table(com$csize)

adj.red <- adj.ind[diagonal.ind != 1, diagonal.ind != 1]

net.red <- graph.adjacency(adj.red, weighted=TRUE)

gplot(net.red, edge.alpha=0.1)

c3 <- circles(net.red, 3, "total")
clust <- agnes(c3)

plot(clust)

klynger <- cutree(clust, k=5)
table(klynger)
network.by.variable(net.red, klynger)

del         <- net.red - which(klynger != 4)
gplot(del)

klynger <- cutree(clust, k=10)
table(klynger)
network.by.variable(net.red, klynger)

klynger <- cutree(clust, k=20)
table(klynger)
network.by.variable(net.red, klynger)

del         <- net.red - which(klynger != 11)
gplot(del)

klynger <- cutree(clust, k=30)
table(klynger)
network.by.variable(net.red, klynger)

del         <- net.red - which(klynger != 6)
gplot(del, edge.alpha=0.05)

klynger <- cutree(clust, k=35)
table(klynger)
network.by.variable(net.red, klynger)

as.matrix(sort(closeness(del)))








