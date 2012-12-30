setwd("~/My Dropbox/R/soc.sna/")
library(igraph)
library(ggplot2)
source("soc.sna.R")

bag    <- read.csv("data_baggrund.csv", sep="|", fileEncoding="UTF-8")
n      <- as.character(bag$Navn)


##############################
# Uddannelse

uddannelse      <- read.csv("data_uddannelse.csv", sep="|", fileEncoding="UTF-8")
uddannelse$Navn <- sub(' +$', '', uddannelse$Navn)
nu              <- as.character(uddannelse$Navn)
data            <- cbind(bag, uddannelse[,-1])
data$Navn       <- nu



###################################################################
# Find Biq_id


rel            <- read.csv("Data_virksomheder_rel.csv", sep="|", fileEncoding="UTF-8")

rel.n          <- as.character(rel$NAVN)[rel$NAVN %in% nu]
rel.id         <- as.character(rel$BIQ_PERSON_ID)[rel$NAVN %in% nu]
fest <- unique(data.frame(rel.n, rel.id))
tb <- table(fest$rel.n)
fest <- fest[order(fest[,1]),]
eliminate <- c(1154180, 270261, 908748, 282522, 1655058, 468144, 468165, 989328, 513857, 1605072, 1881695, 919971, 570550)
festa <- fest[(fest[,2] %in% eliminate)==FALSE ,]
festa[,2] <- as.numeric(as.character(festa[,2]))
festa[,2][festa[,1] %in% c("Jørgen Jensen", "Lars Jensen", "Niels Fog")]   <- NA
festa <- festa[order(festa[,1]),]
data$BIQ_ID <- festa[,2]

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

#################################
## Find kommissionsmedlemmer

ind  <- read.csv("indsamling_kommission.csv", sep="|", fileEncoding="UTF-8")
dn <- data$Navn

indo <- ind[ind$BIQ_ID != 0,]
indo <- indo[indo$Data_kilde != "Bestyrelse",]
bid  <- indo$BIQ_ID
bestyrelse <- ind[ind$BIQ_ID %in% bid & ind$Data_kilde == "Bestyrelse",]
kommissionspost <- as.factor(data$Navn %in% bestyrelse$NAVN)
levels(kommissionspost) <- c("Ingen kommissionspost" , "Kommissionspost")

data$kommissionspost <- kommissionspost

###############################
### Gem

data <- data[, -1]

write.table(data, "Data_inderkreds_individer.csv", sep="|", fileEncoding="UTF-8", row.names = FALSE)

