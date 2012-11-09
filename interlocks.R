######## Analyse af virksomhedernes interlocks

setwd("~/My Dropbox/R/interlocks")

rel <- read.csv("~/My Dropbox/Elite/Data/Data/Relation_BIQ_top.csv", sep="|", encoding="UTF-8")
org <- read.csv("~/My Dropbox/Elite/Data/Data/Organisation_BIQ_top.csv", sep="|", encoding="UTF-8")
source("~/My Dropbox/R/soc.sna/soc.sna.R")
library(ggplot2)
library(cluster)
library(igraph)

netmat <- data.frame(rel$NAVN, rel$ORG_NAVN)
colnames(netmat) <- c("navn", "org")

tabnet          <- table(netmat)
tabnet          <- as.matrix(tabnet)
adj.org         <- t(tabnet)%*%tabnet # Individ*individ
diagonal.org    <- diag(adj.org)
diag(adj.org)   <- 0

net.org         <- graph.adjacency(adj.org, weighted=TRUE)

vægt <- E(net.org)$weight
E(net.org)[vægt==4]

sort(rowSums(adj.org == diagonal.org))

adj.div <- adj.org / diagonal.org      # Hvor mange procent af deres bestyrelse de deler med en anden virksomhed
subber  <- which(adj.div >= 0.5, arr.ind = TRUE)
overlap <- adj.div[subber[,1], subber[,2]]

danger <- data.frame(rownames(overlap), colnames(overlap), round(diag(overlap),2))
danger <- danger[order(danger[,3], decreasing=TRUE),]
colnames(danger) <- c("fra", "til", "med")
write.csv(danger, file="danger.csv")

