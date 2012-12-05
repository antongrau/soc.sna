##################################################
###### Find inderkredsen ! #######################

setwd("~/My Dropbox/R/interlocks/")

# Hvis du ikke har installeret cluster pakken så kør:
library(ggplot2)
library(cluster)
library(igraph)
memory.size(4000)


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

### Nu laves netværksobjekterne
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

# adj.b<- rownames(adj.com)
# adj.n<- rownames(adj.com)

# 
# n <- rel$BIQ_PERSON_ID
# nn <- rel$NAVN[n %in% adj.b]
# 
# m.biq<-levels(as.factor(as.character(nn)))
# 
# adj.n[adj.n %in% m.biq == FALSE]


# Her finder vi inderkredsen
circle3        <- circles(net.com, 3, "total")
clust3         <- agnes(circle3)
#plot(clust3)


### 10 # Tjek 4
# k10            <- cutree(clust3, k=10)
# table(k10)
# network.by.variable(net.com, k10)
# 
# net.del <- net.com - which(k10 != 4)
# gplot(net.del)
# as.matrix(sort(degree(net.del)))
## 25 Her ryger Ane Uggla
k25            <- cutree(clust3, k=25)
 table(k25)

#network.by.variable(net.com, k25==4)
#net.del <- net.com - which(k25 != 4)
#gplot(net.del)

de.170         <- which(k25==1)
net.inderkreds <- net.com - which(k25!=1)

navne <- V(net.inderkreds)$name

navn.170 <- navne[de.170]

save(net.inderkreds, file="inderkredsen")
save(navn.170, file="inderkredsens.navne")
write.csv(navn.170, file="inderkredsens.navne.csv")

# 
# degree(net.com)[de.170]
# sort(closeness(net.com))[de.170]
# sort(betweenness(net.com))[de.170]
# network.by.variable(net.com, k25==1)
# 
# between.all <- sort(betweenness(net.com), decreasing=TRUE)[1:170]
# between.170 <-betweenness(net.com)[de.170]
# sum(between.all %in% between.170)/170
# 
# close.all <- sort(closeness(net.com), decreasing=TRUE)[1:170]
# close.170 <-closeness(net.com)[de.170]
# sum(close.all %in% close.170)/170
# 
# naf <- (close.all %in% close.170)
# names(naf) <- names(close.all)
# as.matrix(naf[naf==FALSE])
# 
# 
# n3 <- neighborhood.size(net.com, 3)
# names(n3) <- V(net.com)$name
# 
# n.all <- sort(n3, decreasing=TRUE)[1:170]
# n.170 <- n3[de.170]
# 
# nn.all <- names(n.all)
# nn.170 <- names(n.170)
# 
# sum(nn.all %in% nn.170) /170
# 
# mistet <- nn.all %in% nn.170
# names(mistet) <- nn.all
# as.matrix(mistet[mistet==FALSE])
# 
# mastet <- nn.170 %in% nn.all
# names(mastet) <- nn.170
# as.matrix(mastet[mastet==FALSE])
# 
# as.matrix(sort(ce3[de.170]))
# as.matrix(sort(n3[de.170]))
# 
# hurma <- (n.170 %in% n.all)
# sum(hurma)/170
# names(hurma) <- names(n.170)
# as.matrix(hurma[hurma==FALSE])
# 
# 
# 
# close.between <- betweenness(net.com) * closeness(net.com)
# plot(sort(close.between))
# cb.all <- sort(close.between, decreasing=TRUE)[1:170]
# cb.170 <- close.between[de.170]
# sum(cb.all %in% cb.170)/170
# 
# as.matrix(cb.all)
# 
# 
# n4 <- neighborhood.size(net.com, 4)
# plot(sort(n4))
# 
# ce3 <- closeness.estimate(net.com, cutoff=3)
# plot(sort(ce3))
# 
# ce3.all <- sort(ce3, decreasing=TRUE)[1:170]
# ce3.170 <- ce3[de.170]
# 
# nce3.all <- names(ce3.all)
# nce3.170 <- names(ce3.170)
# 
# sum(nce3.all %in% nce3.170) /170
# 
# mistet <- nce3.all %in% nce3.170
# names(mistet) <- nce3.all
# as.matrix(mistet[mistet==FALSE])
# 
# mastet <- nce3.170 %in% nce3.all
# names(mastet) <- nce3.170
# as.matrix(mastet[mastet==FALSE])
# 
# 
# 
# 
# 
# 
# ## 32 
# k32            <- cutree(clust3, k=32)
# table(k32)
# 
# network.by.variable(net.com, k32==6)
# net.del <- net.com - which(k32 != 6)
# gplot(net.del)
# 
# a <- as.matrix(sort(degree(net.com)[k32==1]))
# a
# head(a)
# 
# who(net.com, "Jesper Mailind")
# 
# inder <- net.com - which(k32 != 1)
# as.matrix(sort(degree(inder)))
# 
# ## 58 Her ryger Anders Christen Obel AV!
# k58            <- cutree(clust3, k=58)
# table(k58)
# network.by.variable(net.com, k58==4)
# network.by.variable(net.com, k58==1)
# 
# net.del <- net.com - which(k58 != 4)
# gplot(net.del)
# 
# 
# as.matrix(sort(degree(net.com)[k58==4]))
# 
# ## 
# 
# 
# ### Sammenfald med tidligere analyse
# 
# load("inner.circle")
# net.del <- net.com - which(k32 != 1) 
# 
# navn.inner <- V(net.inner)$name
# navn.ny    <- V(net.del)$name
# 
# sum(navn.ny %in% navn.inner) / length(navn.ny)
# sum(navn.inner %in% navn.ny) / length(navn.inner)

# MERE JUNK

# net.all <- graph.adjacency(adj.ind, weighted=TRUE)
# 
# # Her finder vi den største component
# com            <- clusters(net.all)
# largest.com    <- which.max(com$csize) == com$membership
# net.com        <- net.all - which(largest.com == FALSE)
# 
# n.com           <- V(net.com)$name
# b               <- rel$BIQ_PERSON_ID %in% n.com
# 
# # Her laver vi en dataindsamlingsmatrice for alle individer
# n               <- as.factor(rel$NAVN)
# nid             <- rel$BIQ_PERSON_ID
# nidu <- nid[b]
# n    <- n[b]
# 
# n   <-  n[duplicated(nidu)==FALSE]
# nidu <- nidu[duplicated(nidu)==FALSE]
# 
# dat <- data.frame(nidu, n)
# colnames(dat) <- c("BIQ_ID", "NAVN")
# 
# write.csv(dat, file="data_indsamling_component.csv", fileEncoding="UTF-8")
# 

# # Her laver vi en dataindsamlingsmatrice for alle individer
# n               <- as.factor(rel$NAVN)
# nid             <- rel$BIQ_PERSON_ID
# nidu <- nid[duplicated(nid)==FALSE]
# n    <- n[duplicated(nid)==FALSE]
# 
# dat <- data.frame(nidu, n)
# colnames(dat) <- c("BIQ_ID", "NAVN")
# 
# write.csv(dat, file="data_indsamling_alle.csv", fileEncoding="UTF-8")











