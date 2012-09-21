##########################
#### Regeringseksemplet

# Først installerer vi de nødvendige pakker
# Rstudio vil bede dig om at vælge repository. Vælg fx. Aalborg og set det som default.
install.packages("igraph")
install.packages("ggplot2")

# Så indlæser vi de nødvendige pakker:
library(igraph)

# Vælg dit working directory
# I mit tilfælde er det:
# setwd("~/My Dropbox/R/interlocks")
# Indsæt dit eget eller arbejd i dit standard bibliotek

# Download datasættet
download.file("https://raw.github.com/antongrau/soc.sna/master/edgelist.csv", destfile="edgelist.csv")
data <- read.csv(file="edgelist.csv", sep=",", fileEncoding="latin1") 

# Her fjerner vi tomme rækker i data
navn  <- data$NAVN
org   <- data$ORG
a     <- navn==""
navn  <- navn[a==FALSE, drop=TRUE]
org   <- org[a==FALSE, drop=TRUE]
ind.org <- data.frame(navn, org)
colnames(ind.org) <- c("navn", "org")

# Først laves Case affiliation matricen:
affiliation           <- table(ind.org)
affiliation           <- as.matrix(affiliation)

# Så laves adjacency matricen for individer
adjacency.individ     <- affiliation%*%t(affiliation)

# Så laves adjacency matricen for organisationer
adjacency.organisation        <- t(affiliation)%*%affiliation 

# Her slettes vi diagonalen i begge matricer
diag(adjacency.organisation)  <- 0
diag(adjacency.individ)       <- 0

# Så laver vi et netværksobjekt for individer
graph.individ        <- graph.adjacency(adjacency.individ, mode="undirected", weighted=TRUE)

# Nu laver vi et netværksobjekt for organisationer
graph.organisation   <- graph.adjacency(adjacency.organisation, mode="undirected", weighted=TRUE)

# Standardplot for individer
plot.igraph(graph.individ, vertex.label=V(graph.individ)$name,layout=layout.fruchterman.reingold, edge.color="black",edge.width=E(graph.individ)$weight)

# Standardplot for organisationer
plot.igraph(graph.organisation, vertex.label=V(graph.organisation)$name,layout=layout.fruchterman.reingold, edge.color="black",edge.width=E(graph.organisation)$weight)


#################################################################
####  Fancy plots!                                           ####
#################################################################


# Først henter vi koden til plot funktionen
download.file("https://raw.github.com/antongrau/soc.sna/master/gplot.R", destfile="gplot.R")
source("gplot.R")
library(ggplot2)


# Så laver vi fancy plot for individerne
gplot(graph.individ, vertex.coord=layout.kamada.kawai(graph.individ))
# Et til!
gplot(graph.individ, vertex.coord=layout.fruchterman.reingold(graph.individ), edge.colour="red", edge.alpha=0.2, text.size=6)


# Fancy plots for organisationerne
gplot(graph.organisation, vertex.coord=layout.kamada.kawai(graph.organisation))
# Et til!
gplot(graph.organisation, vertex.coord=layout.fruchterman.reingold(graph.organisation), edge.colour="blue", edge.alpha=0.2 )





