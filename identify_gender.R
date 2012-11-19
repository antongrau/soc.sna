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

køn   <- vector(length=length(navne))
for (i in 1:length(navne)){
n <- fornavne[i]
køn.navn <- as.numeric(names.gender[names.gender$Navn %in% n, ][5])
køn[i]     <- køn.navn
}
b <- c(0, 0.2, 0.8, 1)
kategori <- cut(køn, b, include.lowest=TRUE, labels=c("Kvinde", "Binominal", "Mand"))
return(kategori)
}

rel$køn <- kategori

save(rel ,file="rel.gender")
load("rel.gender")

load("inderkredsen")
library(igraph)

in.navne <- V(net.inderkreds)$name






