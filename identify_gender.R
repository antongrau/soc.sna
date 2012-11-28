#############################################
### Find køn
setwd("~/Dropbox/R/soc.sna/")
load("names_gender")
rel         <- read.csv("Relation_BIQ_top.csv", sep="|", encoding="UTF-8")
navne <- as.character(rel$NAVN)

find.køn <- function(navne, names.gender=names.gender){
  n.list <- strsplit(navne, " ")
  fornavne <- vector(length=length(navne))
  for (i in 1:length(fornavne)){
    fornavne[i] <- n.list[[i]][1]
  }
  fornavne <- toupper(fornavne)
  
  ng <- names.gender[names.gender$Navn %in% fornavne,]
  
  køn   <- vector(length=length(navne))
  for (i in 1:length(navne)){
    n <- fornavne[i]
    køn.navn <- as.numeric(ng[match(n, ng$Navn),][5])
    køn[i]     <- køn.navn
  }
  b <- c(0, 0.2, 0.8, 1)
  kategori <- cut(køn, b, include.lowest=TRUE, labels=c("Kvinde", "Binominal", "Mand"))
  return(kategori)
}


system.time(kan <- find.køn(navne, names.gender))
rel$køn <- kan

save(rel ,file="rel.gender")
load("rel.gender")

##### køn i netværk 

load("inderkredsen")

source("soc.sna.R")
library(igraph)
library(ggplot2)

in.navne <- V(net.inderkreds)$name
in.køn   <- find.køn(in.navne, names.gender) 
table(in.køn)
in.navne[in.køn=="Kvinde"]

gplot(net.inderkreds, vertex.fill=in.køn, text.alpha=0, edge.alpha=0.1)

load("~/My Dropbox/R/interlocks/net.ind")


ind.navn <- V(net.ind)$name
ind.køn  <- find.køn(ind.navn, names.gender)
table(ind.køn)
prop.table(table(ind.køn))

gplot(net.ind, vertex.fill=in.køn, text.alpha=0, edge.alpha=0.1)

network.by.variable(net.ind, ind.køn)