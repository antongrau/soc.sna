#### 4. Undervisning - Degrees og density!
setwd("~/My Dropbox/R/interlocks/")

library(ggplot2)
library(igraph)

# Download data om indsomhederne
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
adj.ind       <- tabnet %*% t(tabnet)
diag(adj.ind) <- 0 

net.ind       <- graph.adjacency(adj.ind, mode="undirected", weighted=TRUE)

##################################################
############### Plot #############################
source("gplot2.r")
layout <- layout.fruchterman.reingold(net.ind)
gplot(net.ind, vertex.coord=layout, text.alpha=0)

##################################################
############### Degrees ##########################

degree.ind    <- sort(degree(net.ind), decreasing=TRUE)
plot(degree.ind)
table(degree.ind)
as.matrix(head(degree.ind, 20))

deg <- degree(net.ind)
n <- gplot(net.ind, vertex.coord=layout, vertex.fill=deg ,text.alpha=0, vertex.size=4, edge.alpha=0.1)
n + scale_fill_gradient2(high = "red")

totaldegree     <- sum(degree.ind)
totaldegree
average.degree  <- totaldegree/nrow(adj.ind)
average.degree

######################################
######## Neighborhoods ###############

n1               <- neighborhood.size(net.ind, 1)-1
n1

n2               <- neighborhood.size(net.ind, 2)-1
n2

grannis.g.factor <- sum(n2-n1)/sum(n1)
grannis.g.factor

names(n2)        <- rownames(adj.ind)
as.matrix(head(sort(n2, decreasing=TRUE), 20))
as.matrix(head(sort(n2-n1, decreasing=TRUE), 20))
n3               <- neighborhood.size(net.ind, 3)-1

names(n3)        <- rownames(adj.ind)
as.matrix(head(sort(n3, decreasing=TRUE), 20))
plot(sort(n2, decreasing=TRUE))
plot(sort(n3, decreasing=TRUE))
n6               <- neighborhood.size(net.ind, 6)-1
plot(sort(n6, decreasing=TRUE))

#######################################################
################# Density #############################

# Average path
plot(path.length.hist(net.ind)$res) # Wauv den er normalfordelt!
average.path.length(net.ind)

# Density
L <- ecount(net.ind) # Antallet af edges
L
n <- vcount(net.ind) # Antallet af vertices
n
L/(n*(n-1)/2)
graph.density(net.ind) # Det samme uden matematik!

# Den største component
com               <- clusters(net.ind)
largest.cluster   <- which.max(com$csize)
largest.component <- com$membership == largest.cluster
table(com$csize)
adj.ind           <- adj.ind[largest.component, largest.component]
net.ind           <- graph.adjacency(adj.ind, mode="undirected", weighted=TRUE)

#################################################################################
################################# Centralitet ###################################

# Den relative centralitet - 
cent.deg      <- centralization.degree(net.ind)
str(cent.deg)
cent.deg$centralization       # Antallet af degrees i det bedst forbundne punkt sat i forhold til alle forbindelser
cent.deg$theoretical_max      # Det teoretiske maximale antal degrees for hele netværket

## Closeness
close <- closeness(net.ind)
str(close)
as.matrix(head(sort(close, decreasing=TRUE), 40))
as.matrix(tail(sort(close, decreasing=TRUE), 40))
plot(sort(close, decreasing=TRUE))

plot(sort(closeness.estimate(net.ind, cutoff=1), decreasing=TRUE))
as.matrix(head(sort(closeness.estimate(net.ind, cutoff=1), decreasing=TRUE), 20))
plot(sort(closeness.estimate(net.ind, cutoff=2), decreasing=TRUE))
plot(sort(closeness.estimate(net.ind, cutoff=3), decreasing=TRUE))
plot(sort(closeness.estimate(net.ind, cutoff=4), decreasing=TRUE))
plot(sort(closeness.estimate(net.ind, cutoff=5), decreasing=TRUE))
plot(sort(closeness.estimate(net.ind, cutoff=6), decreasing=TRUE))
plot(sort(closeness.estimate(net.ind, cutoff=7), decreasing=TRUE))
plot(sort(closeness.estimate(net.ind, cutoff=8), decreasing=TRUE))
as.matrix(head(sort(closeness.estimate(net.ind, cutoff=8), decreasing=TRUE), 20))
plot(sort(closeness.estimate(net.ind, cutoff=9), decreasing=TRUE))
plot(sort(closeness.estimate(net.ind, cutoff=10), decreasing=TRUE))
plot(sort(closeness.estimate(net.ind, cutoff=11), decreasing=TRUE))
plot(sort(closeness.estimate(net.ind, cutoff=12), decreasing=TRUE))
plot(sort(closeness.estimate(net.ind, cutoff=13), decreasing=TRUE))
plot(sort(closeness.estimate(net.ind, cutoff=14), decreasing=TRUE))
plot(sort(closeness.estimate(net.ind, cutoff=15), decreasing=TRUE))

## betweenness
between <- betweenness(net.ind)
str(between)
as.matrix(head(sort(between, decreasing=TRUE), 40))
as.matrix(tail(sort(between, decreasing=TRUE), 40))
plot(sort(between, decreasing=TRUE))

plot(sort(betweenness.estimate(net.ind, cutoff=1), decreasing=TRUE))
plot(sort(betweenness.estimate(net.ind, cutoff=2), decreasing=TRUE))
as.matrix(head(sort(betweenness.estimate(net.ind, cutoff=2), decreasing=TRUE), 20))
plot(sort(betweenness.estimate(net.ind, cutoff=3), decreasing=TRUE))
plot(sort(betweenness.estimate(net.ind, cutoff=4), decreasing=TRUE))
plot(sort(betweenness.estimate(net.ind, cutoff=5), decreasing=TRUE))
plot(sort(betweenness.estimate(net.ind, cutoff=6), decreasing=TRUE))
plot(sort(betweenness.estimate(net.ind, cutoff=7), decreasing=TRUE))
plot(sort(betweenness.estimate(net.ind, cutoff=8), decreasing=TRUE))
as.matrix(head(sort(betweenness.estimate(net.ind, cutoff=8), decreasing=TRUE), 20))
plot(sort(betweenness.estimate(net.ind, cutoff=9), decreasing=TRUE))
plot(sort(betweenness.estimate(net.ind, cutoff=10), decreasing=TRUE))
plot(sort(betweenness.estimate(net.ind, cutoff=11), decreasing=TRUE))
plot(sort(betweenness.estimate(net.ind, cutoff=12), decreasing=TRUE))
plot(sort(betweenness.estimate(net.ind, cutoff=13), decreasing=TRUE))
plot(sort(betweenness.estimate(net.ind, cutoff=14), decreasing=TRUE))
plot(sort(betweenness.estimate(net.ind, cutoff=15), decreasing=TRUE))


### Eigenvector centrality
eig  <- evcent(net.ind)
str(eig)
as.matrix(head(sort(eig$vector, decreasing=TRUE), 30))
plot(sort(eig$vector, decreasing=TRUE))

### Closeness og Betweenness
set <- data.frame(close, between)
ggplot(set, aes(x=close, y=between)) + geom_point(aes(color=eig$vector)) + geom_smooth()


###################################################################
########################## Regeringen #############################

# Download datasættet
# Windows
download.file("https://raw.github.com/antongrau/soc.sna/master/edgelist.csv", destfile="edgelist.csv")

# Mac/Linux
download.file("https://raw.github.com/antongrau/soc.sna/master/edgelist.csv", destfile="edgelist.csv", method="curl")

data <- read.csv(file="edgelist.csv", sep=",", fileEncoding="latin1") 

# Her fjerner vi tomme rækker i data
navn  <- data$NAVN
org   <- data$ORG
a     <- navn==""
navn  <- navn[a==FALSE, drop=TRUE]
org   <- org[a==FALSE, drop=TRUE]
ind.org <- data.frame(navn, org)
colnames(ind.org) <- c("navn", "org")
affiliation           <- as.matrix(affiliation)

# Så laves adjacency matricen for individer
adjacency.individ     <- affiliation%*%t(affiliation)

# Her slettes vi diagonalen i begge matricer
diag(adjacency.individ)       <- 0

# Så laver vi et netværksobjekt for individer
graph.individ        <- graph.adjacency(adjacency.individ, mode="undirected", weighted=TRUE)

# Degree centralitet
cent.deg      <- centralization.degree(graph.individ)
str(cent.deg)
cent.deg$centralization       # Antallet af degrees i det bedst forbundne punkt sat i forhold til alle forbindelser
cent.deg$theoretical_max      # Det teoretiske maximale antal degrees for hele netværket
sort(degree(graph.individ), decreasing=TRUE)


## Closeness
close <- closeness(graph.individ)
as.matrix(sort(close, decreasing=TRUE))
plot(sort(close, decreasing=TRUE))

## Betweenness 
between <- betweenness(graph.individ, weights=E(graph.individ)$weight)
as.matrix(sort(between, decreasing=TRUE))
plot(sort(close, decreasing=TRUE))


layout <- layout.fruchterman.reingold(graph.individ)
gplot(graph.individ, layout, vertex.size=close, vertex.fill=close)  + scale_fill_gradient2(high = "red")
gplot(graph.individ, layout, vertex.size=between, vertex.fill=between)  + scale_fill_gradient2(high = "red")