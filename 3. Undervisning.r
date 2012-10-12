#### 3. Undervisning - Degrees og density!


setwd("~/My Dropbox/R/interlocks/")

library(ggplot2)
library(igraph)

############ UPLOAD NY DATA!!!!

rel    <- read.csv("~/My Dropbox/Elite/Data/Data/Relation_BIQ_top.csv", sep="|", encoding="UTF-8", strip.white=TRUE) 
org    <- read.csv("~/My Dropbox/Elite/Data/Data/Organisation_BIQ_top.csv", sep="|", encoding="UTF-8", strip.white=TRUE)


# Download data om virksomhederne
# Windows
download.file("https://raw.github.com/antongrau/soc.sna/master/Organisation_BIQ_top_U.csv", destfile="Organisation_BIQ_top_U.csv")
download.file("https://raw.github.com/antongrau/soc.sna/master/Relation_BIQ_top.csv", destfile="Relation_BIQ_top.csv")
download.file("https://raw.github.com/antongrau/soc.sna/master/delbeskrivelse.r", destfile="delbeskrivelse.r")

# Mac/Linux
# Hvis du har en gammel mac så må du hente det ned manuelt og ligge filerne direkte i dit working directory
download.file("https://raw.github.com/antongrau/soc.sna/master/Organisation_BIQ_top_U.csv", destfile="Organisation_BIQ_top_U.csv", method="curl")
download.file("http://raw.github.com/antongrau/soc.sna/master/Relation_BIQ_top.csv", destfile="Relation_BIQ_top.csv", method="curl")
download.file("https://raw.github.com/antongrau/soc.sna/master/delbeskrivelse.r", destfile="delbeskrivelse.r", method="curl")

# Indlæs data
rel           <- read.csv("Relation_BIQ_top.csv", sep="|", encoding="UTF-8", strip.white=TRUE) 
virk          <- read.csv(file="Organisation_BIQ_top_U.csv", sep="|", fileEncoding="UTF-8", dec = ",")
source("delbeskrivelse.r", encoding="UTF-8")

edges         <- data.frame(as.character(rel$NAVN), as.character(rel$ORG_NAVN))
edge.table    <- table(edges)
tabnet        <- as.matrix(edge.table)
adj.virk      <- t(tabnet)%*%tabnet 
diag(adj.virk) <- 0 

net.virk      <- graph.adjacency(adj.virk, mode="undirected", weighted=TRUE)

##################################################
############### Degrees ##########################

degree.virk    <- sort(degree(net.virk), decreasing=TRUE)
plot(degree.virk)
table(degree.virk)
as.matrix(head(degree.virk, 20))

totaldegree     <- sum(degree.virk)
totaldegree
average.degree  <- totaldegree/nrow(adj.virk)
average.degree


######################################
######## Neighborhoods ###############

n1               <- neighborhood.size(net.virk, 1)-1
n1
#antal first n
n2               <- neighborhood.size(net.virk, 2)-1
n2
grannis.g.factor <- sum(n2-n1)/sum(n1)
grannis.g.factor
names(n2)        <- rownames(adj.virk)
head(sort(n2, decreasing=TRUE), 20)
as.matrix(head(sort(n2-n1, decreasing=TRUE), 200))


n3               <- neighborhood.size(net.virk, 3)-1
names(n3) <- rownames(adj.virk)
head(sort(n3, decreasing=TRUE), 50)

plot(sort(n3-n2-n1))
plot(sort(n3))

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
org <- org[order(org$ORG_NAVN),]
rnavn <- rownames(adj.virk)
navn  <- as.character(org$ORG_NAVN)
identical(rnavn, navn)

# Så laver vi en omkodning af omsætning
omsætning <- cut(org$OMSÆTNING.10, quantile(org$OMSÆTNING.10, na.rm=TRUE))
levels(omsætning) <- c("1. kvartil: Mindst", "2. kvartil", "3. kvartil", "4. kvartil: Størst")
summary(omsætning)


ansatte <- cut(org$ANSATTE.10, quantile(org$ANSATTE.10, na.rm=TRUE))
levels(ansatte) <- c("1. kvartil: Mindst", "2. kvartil", "3. kvartil", "4. kvartil: Størst")
summary(ansatte)

egenkapital <- cut(org$EGENKAPITAL.10, quantile(org$EGENKAPITAL.10, na.rm=TRUE))
levels(egenkapital) <- c("1. kvartil: Mindst", "2. kvartil", "3. kvartil", "4. kvartil: Størst")
summary(egenkapital)

del.beskrivelse(net.virk, omsætning)
del.beskrivelse(net.virk, org$BØRSEN.BRANCHE)
del.beskrivelse(net.virk, ansatte)
del.beskrivelse(net.virk, egenkapital)
del.beskrivelse(net.virk, org$region)
del.beskrivelse(net.virk, org$dominerende)

### Average degree på størrelse