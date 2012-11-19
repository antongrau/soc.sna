##### 6. Undervisning - plots
setwd("~/My Dropbox/R/interlocks/")

library(ggplot2)
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
load("inderkredsen")
rel         <- read.csv("Relation_BIQ_top.csv", sep="|", encoding="UTF-8")

#########################################################################
# Her plotter vi med min funktion der bruger ggplot2
gplot(net.inderkreds)
gplot # Hvis vi skriver navnet på funktionen får vi en smule hjælp til hvilke kommandoer den kan tage
gplot(net.inderkreds, text.colour="red", vertex.color="Purple", vertex.size=6, text.alpha=0.5, vertex.shape=3, edge.color="orange", edge.alpha=0.1)

l <- layout.kamada.kawai(net.inderkreds)
gplot(net.inderkreds, vertex.coord=l, text.alpha=0.5)
l <- layout.circle(net.inderkreds)
gplot(net.inderkreds, vertex.coord=l, text.alpha=0, edge.alpha=0.1)
l <- layout.grid(net.inderkreds)
gplot(net.inderkreds, vertex.coord=l, text.alpha=0, edge.alpha=0.1)
?layout.fruchterman.reingold

E(net.inderkreds)$weight

gplot(net.inderkreds, edge.alpha=E(net.inderkreds)$weight) # Her plotter vi en edge egenskab
# Nu plotter vi en vertex egenskab
gplot(net.inderkreds, vertex.fill=degree(net.inderkreds), vertex.size=5, edge.alpha=0.1)
p <- gplot(net.inderkreds, vertex.fill=betweenness(net.inderkreds), vertex.size=5, edge.alpha=0.1)
p + scale_fill_gradient(low="white", high="red") 

####
# Nu knytter vi en egenskab ved individet fra en anden matrice til vores vertex

# Er han direktør?
# Først navnene på individerne i vores netværk 
navne       <- V(net.inderkreds)$name
# Kun relationer fra folk i inderkredsen
a           <- rel$NAVN %in% navne
rel.i       <- rel[rel$NAVN %in% navne,]
# Er han adm. direktør?
post        <- rel.i$post
table(post)
rel.d      <- rel.i[post == "Adm. Direktør",]
navn.d     <- as.factor(as.character(rel.d$NAVN))
navn.d     <- levels(navn.d)
navn.d               # Her har vi en liste med navne for hvem det gælder at de er Adm. Direktører

direktør   <- vector(length=length(navne))
direktør[navne %in% navn.d] <- "Adm. Direktør"  # Her vælger vi at alle der har et "direktør navn" skal have et bestemt udfald.

# Nu skal han plottes!
gplot(net.inderkreds, vertex.fill=direktør, edge.alpha=0.1, vertex.size=5)

# Nu vil vi vide noget om netværket på baggrund af de adm. direktører
network.by.variable(net.inderkreds, variabel=direktør)

# Hvem er det?
who(net.inderkreds, "Peter Gæmelke", relation.matrix=rel)
who(net.inderkreds, "Anders Christen Obel")










