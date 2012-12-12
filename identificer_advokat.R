setwd("~/My Dropbox/R/interlocks/")
library(igraph)
library(ggplot2)
source("soc.sna.R")


advokat <- read.csv("data_advokater.csv", sep="|", fileEncoding="UTF-8")
advokat$X[is.na(advokat$X)] <- 0
advokat <- advokat[advokat$BIQ_ID != 0,] # Her fjerner vi alle advokater uden bestyrelsesposter - for de har ingen BIG_ID
advokat_biq <- advokat[advokat$X != 0,]
advokat <- advokat[advokat$X==0,]  # Her fjerner vi alle bestyrelsesmedlemmer der ikke er advokater - de har en anden X værdi end 0   
advokat <- advokat[duplicated(advokat$BIQ_ID)==FALSE,] # Her fjerner vi nogle advokater der af en eller anden årsag opstod flere gange. Det er 2.
advokat_BIQ_navn <- advokat_biq$Navn[advokat_biq$BIQ_ID %in% advokat$BIQ_ID]
advokat_BIQ_id   <- advokat_biq$BIQ_ID[advokat_biq$BIQ_ID %in% advokat$BIQ_ID]


###################################################
### Kode til at koble de her data til de andre data
rel    <- read.csv("~/My Dropbox/R/soc.sna/Data_virksomheder_rel.csv", sep="|", fileEncoding="UTF-8") # Indlæs relationsmatricen

netmat <- data.frame(rel$NAVN, rel$ORG_NAVN)
colnames(netmat) <- c("navn", "org")

### Nu laves netværksobjektet
tabnet          <- table(netmat)
tabnet          <- as.matrix(tabnet)
adj.ind         <- tabnet%*%t(tabnet) # Individ*individ
diagonal.ind    <- diag(adj.ind)
diag(adj.ind)   <- 0
net             <- graph.adjacency(adj.ind, weighted=TRUE)

ja.advokat      <- V(net)$name %in% advokat_BIQ_navn       # Sammenlign med navnene i et netværksobjekt


network.by.variable(net, ja.advokat)

####################################################
#### Hvad med virksomhederne?

org         <- read.csv("~/My Dropbox/R/soc.sna/Data_virksomheder_org.csv", sep="|", fileEncoding="UTF-8") # Indlæs organisationsmatricen

a           <- rel$BIQ_PERSON_ID %in% advokat_BIQ_id # Er relationen taget af en advokat?
summary(a)                                           # Der er 165 relationer taget af advokater
advokat.org <- rel$ORG_ID[a==TRUE]                   # De her virksomheder har advokater i bestyrelsen
advokat.org <- unique(advokat.org)                   # Så er der kun en række fra hver virksomhed

advokat.i.bestyrelsen   <- org$ORG_ID %in% advokat.org      # Har virksomheden en advokat i bestyrelsen?

table(advokat.i.bestyrelsen, org$BØRSEN.BRANCHE)


####################################################
#### Sidder de i inderkredsen?

load("inderkredsens.navne")                       # Indlæs filen med inderkredsens navne
load("inderkredsen")
inderkreds.advokat  <- navn.170 %in% advokat.navn # Match på navne
gplot(net.inderkreds, vertex.fill=inderkreds.advokat)
