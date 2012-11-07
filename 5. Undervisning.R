#### 5. Undervisning - Cliques
setwd("~/My Dropbox/R/interlocks/")

# Hvis du ikke har installeret cluster pakken så kør:
install.packages("cluster")

library(ggplot2)
library(cluster)
library(igraph)


# Download data om indsomhederne
# Windows
download.file("https://raw.github.com/antongrau/soc.sna/master/Organisation_BIQ_top.csv", destfile="Organisation_BIQ_top.csv")
download.file("https://raw.github.com/antongrau/soc.sna/master/Relation_BIQ_top.csv", destfile="Relation_BIQ_top.csv")
download.file("https://raw.github.com/antongrau/soc.sna/master/soc.sna.R", destfile="soc.sna.r")
download.file("https://raw.github.com/antongrau/soc.sna/master/net.inner.R", destfile="net.inner.R")

# Mac og Linux
download.file("https://raw.github.com/antongrau/soc.sna/master/Organisation_BIQ_top.csv", destfile="Organisation_BIQ_top.csv", method="curl")
download.file("https://raw.github.com/antongrau/soc.sna/master/Relation_BIQ_top.csv", destfile="Relation_BIQ_top.csv", method="curl")
download.file("https://raw.github.com/antongrau/soc.sna/master/soc.sna.R", destfile="soc.sna.r", method="curl")
download.file("https://raw.github.com/antongrau/soc.sna/master/net.inner.R", destfile="net.inner.R", method="curl")

source("soc.sna.R")
load("net.inner.R")
net         <- net.eig
navne       <- V(net)$name


##### Cliques

klike       <- cliques(net, min=3)         #  Kliker af minimum 3's størrelse
length(klike)                              #  Her er antallet af kliker
clique.number(net)                         #  Finder størrelsen på den største klike
stor.klike  <- largest.cliques(net)        #  Finder den største klike
navne[stor.klike[[1]]]                     #  Hvem er det? Nykredit???

max.klike   <- maximal.cliques(net)        #  De maximale kliker

##### Andre algoritmer

?communities
<- fastgreedy.community(net)


##### Social Circles

total.3     <- circles(net, neighborhood=3, mode="total")     # Her 

total.clust <- agnes(total.3)                                 # Her finder vi klyngen
plot(total.clust)
klynger     <- cutree(total.clust, k=5)
table(klynger)
network.by.variable(net, klynger)

klynger     <- cutree(total.clust, k=10)
table(klynger)
network.by.variable(net, klynger)
del <- net - which(klynger != 6)
gplot(del)
del <- net - which(klynger != 10)
gplot(del)

klynger     <- cutree(total.clust, k=14)
table(klynger)
network.by.variable(net, klynger)
del <- net - which(klynger != 4)
gplot(del)

klynger     <- cutree(total.clust, k=20)
table(klynger)
network.by.variable(net, klynger)
del         <- net - which(klynger != 4)
gplot(del)

del         <- net - which(klynger != 1)
gplot(del, edge.alpha=0.1)

sort(degree(net)[klynger==1])                 # Hvem er inderkredsen?


central     <- vector(length=length(klynger))
central[klynger == 1] <- "Inder kredsen"
central[klynger == 5] <- "Banker og fagforeninger"
central[klynger == 4] <- "Prospects"
central[central == FALSE] <- "Ikke central"
table(central)

gplot(net, edge.alpha=0.1, vertex.fill=as.factor(central), text.alpha=0, vertex.size=3)


################### JUNK

network.by.variable(net, central)






