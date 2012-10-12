#### 3. Undervisning - Degrees og density!


setwd("~/My Dropbox/R/interlocks/")

library(ggplot2)
library(igraph)

############ UPLOAD NY DATA!!!!

rel    <- read.csv("~/My Dropbox/Elite/Data/Data/Relation_BIQ_top.csv", sep="|", encoding="UTF-8", strip.white=TRUE) 
org    <- read.csv("~/My Dropbox/Elite/Data/Data/Organisation_BIQ_top.csv", sep="|", encoding="UTF-8", strip.white=TRUE)

edges <- data.frame(as.character(rel$NAVN), as.character(rel$ORG_NAVN), as.character(rel$post))



# Download data om virksomhederne
# Windows
download.file("https://raw.github.com/antongrau/soc.sna/master/Organisation_BIQ_top_U.csv", destfile="Organisation_BIQ_top_U.csv")
download.file("https://raw.github.com/antongrau/soc.sna/master/Relation_BIQ_top.csv", destfile="Relation_BIQ_top.csv")

# Mac/Linux
# Hvis du har en gammel mac så må du hente det ned manuelt og ligge filerne direkte i dit working directory
download.file("https://raw.github.com/antongrau/soc.sna/master/Organisation_BIQ_top_U.csv", destfile="Organisation_BIQ_top_U.csv", method="curl")
download.file("http://raw.github.com/antongrau/soc.sna/master/Relation_BIQ_top.csv", destfile="Relation_BIQ_top.csv", method="curl")

# Indlæs data
rel           <- read.csv("Relation_BIQ_top.csv", sep="|", encoding="UTF-8", strip.white=TRUE) 
virk          <- read.csv(file="Organisation_BIQ_top_U.csv", sep="|", fileEncoding="UTF-8", dec = ",")

edges         <- data.frame(as.character(rel$NAVN), as.character(rel$ORG_NAVN))
edge.table    <- table(edges)
tabnet        <- as.matrix(edge.table)
adj.virk      <- t(tabnet)%*%tabnet 
diag(adj.virk) <- 0 

net.virk      <- graph.adjacency(adj.virk, mode="undirected", weighted=TRUE)

# Kun den største component
com               <- clusters(net.virk)
largest.cluster   <- which.max(com$csize)
largest.component <- com$membership == largest.cluster
adj.c             <- adj.virk[largest.component, largest.component]
net.c             <- graph.adjacency(adj.c, mode="undirected", weighted=TRUE)


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




# Funktionen
del.beskrivelse <- function(net, variabel){
dele <- levels(variabel)
output <- matrix(nrow=11, ncol=length(dele)) # Output matrix
for ( i in 1:length(dele)){
del <- dele[i]
del.ind <- which(variabel==del)

# Antal Vertices
Number.of.vertices  <- length(del.ind)
# Antal edges
Number.of.edges     <- sum(degree(net)[del.ind])
# Average degree
Average.degree      <- round(Number.of.edges/Number.of.vertices, 1)
# Part density i 1000
Part.density        <- round(Number.of.edges/((Number.of.vertices*(vcount(net)-1)/2))*1000, 1)
# Percentage of isolates
Percentage.isolates <- round(sum(degree(net)[del.ind] == 0)/Number.of.vertices * 100, 1)
# Percentage of part in largest component
com               <- clusters(net)
largest.cluster   <- which.max(com$csize)
del.largest.com   <- com$membership[del.ind] == largest.cluster
largest.com       <- com$membership == largest.cluster
Percentage.in.largest.component <- round(sum(del.largest.com)/Number.of.vertices * 100, 1)

# Average path length
cc <- largest.com == TRUE & variabel==del & is.na(variabel)==FALSE
sp                  <- shortest.paths(net)[cc, largest.com]
ind.av.sp           <- rowSums(sp)/ncol(sp)
Average.path.length <- round(sum(ind.av.sp)/nrow(sp),1)
# Longest path
Longest.path        <- max(sp)
# Largest number of degrees
Largest.degree <- max(degree(net)[del.ind])
# Largest 2 neighborhoods
Largest.2.neighborhood <- max(neighborhood.size(net, 2)[del.ind])
# Largest 3 neighborhoods
Largest.3.neighborhood <- max(neighborhood.size(net, 3)[del.ind])


del.stat <- c(Number.of.vertices, Number.of.edges, Average.degree, Part.density, Percentage.isolates,
  Percentage.in.largest.component, Average.path.length, Longest.path, Largest.degree,
  Largest.2.neighborhood, Largest.3.neighborhood)


output[,i] <- del.stat
}
colnames(output) <- dele
rownames(output) <- c("Number of vertices", "Number of edges", "Average degree", "Part density (‰)", "Isolates (%)",
                      "Largest component (%)", "Average path length", "Longest path", "Highest degree", "Largest 2. neighborhood", "Largest 3. neighborhood")
return(output)
}

del.beskrivelse(net.virk, omsætning)
del.beskrivelse(net.virk, org$BØRSEN.BRANCHE)
del.beskrivelse(net.virk, ansatte)
del.beskrivelse(net.virk, egenkapital)

### Average degree på størrelse