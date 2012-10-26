#### 3. Undervisning - Degrees og density!


setwd("~/My Dropbox/R/interlocks/")

library(ggplot2)
library(igraph)

# Download data om virksomhederne
# Windows
download.file("https://raw.github.com/antongrau/soc.sna/master/Organisation_BIQ_top.csv", destfile="Organisation_BIQ_top.csv")
download.file("https://raw.github.com/antongrau/soc.sna/master/Relation_BIQ_top.csv", destfile="Relation_BIQ_top.csv")
download.file("https://raw.github.com/antongrau/soc.sna/master/delbeskrivelse.r", destfile="delbeskrivelse.r")
download.file("https://raw.github.com/antongrau/soc.sna/master/gplot2.R", destfile="gplot2.r")

# Mac/Linux
# Hvis du har en gammel mac så må du hente det ned manuelt og ligge filerne direkte i dit working directory
download.file("https://raw.github.com/antongrau/soc.sna/master/Organisation_BIQ_top.csv", destfile="Organisation_BIQ_top.csv", method="curl")
download.file("http://raw.github.com/antongrau/soc.sna/master/Relation_BIQ_top.csv", destfile="Relation_BIQ_top.csv", method="curl")
download.file("https://raw.github.com/antongrau/soc.sna/master/delbeskrivelse.r", destfile="delbeskrivelse.r", method="curl")
download.file("https://raw.github.com/antongrau/soc.sna/master/gplot2.R", destfile="gplot2.r", method="curl")

# Indlæs data
rel           <- read.csv("Relation_BIQ_top.csv", sep="|", encoding="UTF-8", strip.white=TRUE) 
virk          <- read.csv(file="Organisation_BIQ_top.csv", sep="|", fileEncoding="UTF-8", dec = ",")

edges         <- data.frame(as.character(rel$NAVN), as.character(rel$ORG_NAVN))
edge.table    <- table(edges)
tabnet        <- as.matrix(edge.table)
adj.virk      <- t(tabnet)%*%tabnet 
diag(adj.virk) <- 0 

net.virk      <- graph.adjacency(adj.virk, mode="undirected", weighted=TRUE)

##################################################
############### Plot #############################
source("gplot2.r")
layout <- layout.kamada.kawai(net.virk)
gplot(net.virk, vertex.coord=layout, text.alpha=0)

##################################################
############### Degrees ##########################

degree.virk    <- sort(degree(net.virk), decreasing=TRUE)
plot(degree.virk)
table(degree.virk)
as.matrix(head(degree.virk, 20))

deg <- degree(net.virk)
n <- gplot(net.virk, vertex.coord=layout, vertex.fill=deg ,text.alpha=0, vertex.size=4, edge.alpha=0.1)
n + scale_fill_gradient2(high = "red")

totaldegree     <- sum(degree.virk)
totaldegree
average.degree  <- totaldegree/nrow(adj.virk)
average.degree


######################################
######## Neighborhoods ###############

n1               <- neighborhood.size(net.virk, 1)-1
n1

n2               <- neighborhood.size(net.virk, 2)-1
n2

grannis.g.factor <- sum(n2-n1)/sum(n1)
grannis.g.factor
names(n2)        <- rownames(adj.virk)
as.matrix(head(sort(n2, decreasing=TRUE), 20))
as.matrix(head(sort(n2-n1, decreasing=TRUE), 20))

n3               <- neighborhood.size(net.virk, 3)-1
names(n3)        <- rownames(adj.virk)
as.matrix(head(sort(n3, decreasing=TRUE), 20))

plot(sort(n3-n2-n1))


### Et lille plot
neighbours <- neighborhood.size(net.virk, 2)
n <- gplot(net.virk, vertex.coord=layout, vertex.fill=neighbours ,text.alpha=0, vertex.size=4, edge.alpha=0.1)
n + scale_fill_gradient2(high = "blue")

#######################################################
################# Density #############################

# Average path
plot(path.length.hist(net.virk)$res) # Wauv den er normalfordelt!
average.path.length(net.virk)

# Inclusiveness
no.clusters(net.virk)

# Density
L <- ecount(net.virk) # Antallet af edges
L
n <- vcount(net.virk) # Antallet af vertices
n
L/(n*(n-1)/2)
graph.density(net.virk) # Det samme uden matematik!


### Forskellige mål fordelt på størrelse

# Først tjekker vi at rækkefølgen af de to matricer er den samme!
virk <- virk[order(virk$ORG_NAVN),]
rnavn <- rownames(adj.virk)
navn  <- as.character(virk$ORG_NAVN)
identical(rnavn, navn)

# Så laver vi en omkodning af omsætning
omsætning <- cut(virk$OMSÆTNING.10, quantile(virk$OMSÆTNING.10, na.rm=TRUE))
levels(omsætning) <- c("1. kvartil: Mindst", "2. kvartil", "3. kvartil", "4. kvartil: Størst")
summary(omsætning)


ansatte <- cut(virk$ANSATTE.10, quantile(virk$ANSATTE.10, na.rm=TRUE))
levels(ansatte) <- c("1. kvartil: Mindst", "2. kvartil", "3. kvartil", "4. kvartil: Størst")
summary(ansatte)

egenkapital <- cut(virk$EGENKAPITAL.10, quantile(virk$EGENKAPITAL.10, na.rm=TRUE))
levels(egenkapital) <- c("1. kvartil: Mindst", "2. kvartil", "3. kvartil", "4. kvartil: Størst")
summary(egenkapital)

source("delbeskrivelse.r", encoding="UTF-8")

del.beskrivelse(net.virk, omsætning)
del.beskrivelse(net.virk, virk$BØRSEN.BRANCHE)
del.beskrivelse(net.virk, ansatte)
del.beskrivelse(net.virk, egenkapital)
del.beskrivelse(net.virk, virk$region)
del.beskrivelse(net.virk, virk$dominerende)