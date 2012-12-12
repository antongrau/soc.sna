setwd("~/My Dropbox/R/soc.sna/")
library(igraph)
library(ggplot2)
source("soc.sna.R")

bag <- read.csv("data_baggrund.csv", sep="|", fileEncoding="UTF-8")

n <- as.character(bag$Navn)


##############################
# find advokaterne

advokat <- read.csv("data_advokater.csv", sep="|", fileEncoding="UTF-8")
advokat$X[is.na(advokat$X)] <- 0
advokat <- advokat[advokat$BIQ_ID != 0,] # Her fjerner vi alle advokater uden bestyrelsesposter - for de har ingen BIG_ID
advokat_biq <- advokat[advokat$X != 0,]
advokat <- advokat[advokat$X==0,]  # Her fjerner vi alle bestyrelsesmedlemmer der ikke er advokater - de har en anden X værdi end 0   
advokat <- advokat[duplicated(advokat$BIQ_ID)==FALSE,] # Her fjerner vi nogle advokater der af en eller anden årsag opstod flere gange. Det er 2.
advokat_BIQ_navn <- advokat_biq$Navn[advokat_biq$BIQ_ID %in% advokat$BIQ_ID]

advokat.ind <- as.factor(n %in% advokat_BIQ_navn)
levels(advokat.ind) <- c("Ikke advokat", "Advokat")
bag$advokat <- advokat.ind

summary(advokat.ind)
n[advokat.ind == "Advokat"]

##############################
# Uddannelse

uddannelse      <- read.csv("data_uddannelse.csv", sep="|", fileEncoding="UTF-8")
uddannelse$Navn <- sub(' +$', '', uddannelse$Navn)
nu              <- as.character(uddannelse$Navn)
data            <- cbind(bag, uddannelse[,-1])
data$Navn       <- nu


###############################
# Find køn

setwd("~/My Dropbox/R/soc.sna/")
gender.mat <- read.csv("names_gender.csv", sep="|", fileEncoding="UTF-8")

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

køn <- find.køn(nu, gender.mat)
data$køn <- køn

###############################
### Gem

data <- data[, -1]

write.table(data, "Data_inderkreds_individer.csv", sep="|", fileEncoding="UTF-8", row.names = FALSE)

