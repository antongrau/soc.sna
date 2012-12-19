####################################################
##### Soc.ca og soc.sna integration

setwd("~/My Dropbox/R/soc.ca/")
source("soc.ca.r")
library(ggplot2)
library(igraph)
setwd("~/My Dropbox/R/soc.sna/")
source("soc.sna.r")
data <- read.csv("Data_inderkreds_individer.csv", sep="|", fileEncoding="UTF-8")
load("inderkredsen")

attach(data)
str(data)

udd_længde <- as.character(uddannelse_længde)
udd_længde[is.na(uddannelse_længde)] <- "MISSING" 
udd_længde <- as.factor(udd_længde)

levels(FariBB) <- c("MISSING", "Far: BB", "MISSING", "Far: Ikke i BB", "Far: Ikke i BB", "Far: Ikke i BB")

levels(ÆgtefælleiBB)[1] <- "MISSING"

between <- betweenness(net.inderkreds)
between <- cut(between, breaks=quantile(between), include.lowest=TRUE, labels=c("Betweenness: 1. Lavest", "Betweenness: 2.", "Betweenness: 3.", "Betweenness: 4. Højest" ))
summary(between)

n3 <- neighborhood.size(net.inderkreds, 3)
n3 <- cut(n3, breaks=quantile(n3), include.lowest=TRUE, labels=c("N3: 1. Lavest", "N3: 2.", "N3: 3.", "N3: 4. Højest" ))
summary(n3)




##### Soc.ca
active <- data.frame(køn, MBA, udd_længde, ÆgtefælleiBB, FariBB, kommissionspost,
                     between, n3)

identifier <- as.character(Navn)

sup<- uddannelse_type

set.passive("MISSING")


result <- soc.ca(active, identifier=identifier)
result
p.id(result, point.label=TRUE)

ca.coord <- result$coord.ind[,1:2]

gplot(net.inderkreds, vertex.coord=ca.coord, edge.alpha=0.2)

