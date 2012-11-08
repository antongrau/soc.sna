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
rel         <- read.csv("Relation_BIQ_top.csv", sep="|", encoding="UTF-8")



##### Cliques

klike       <- cliques(net, min=3)         #  Kliker af minimum 3's størrelse
length(klike)                              #  Her er antallet af kliker
clique.number(net)                         #  Finder størrelsen på den største klike
stor.klike  <- largest.cliques(net)        #  Finder den største klike
navne[stor.klike[[1]]]                     #  Hvem er det? Nykredit???

max.klike   <- maximal.cliques(net, min=3)        #  De maximale kliker


i <- 2
j <- 3



clique.overlap <- function(max.klike){
  
klike.overlap <- function(max.klike, klikenr){
  
klike1 <- max.klike[[klikenr]]
row <- vector(length=length(max.klike))
for( j in 1:length(max.klike)){
klike2 <- max.klike[[j]]

if(length(klike1)>=length(klike2)){
  lille <- klike2
  stor  <- klike1
  }else{
    lille <- klike1
    stor  <- klike2
}

overlap <- sum(lille %in% stor) / length(lille)

row[j] <- round(overlap * 100)
}
return(row)
}

result <- matrix(nrow=length(max.klike), ncol=length(max.klike))
for(i in 1:length(max.klike)){
result[i,] <- klike.overlap(max.klike, i) 
}
return(result)
}

################################

forbind.kliker <- function(result2, max.klike){
  
smeltkliker <- function(result2, klikenr, max.klike){
kliken <- max.klike[[klikenr]]
merge <- max.klike[which(result2[klikenr,] == TRUE)]
if(length(merge)!=0){
for (i in 1:length(merge)){
merge[[i]]  <- unique(c(merge[[i]], kliken))
}
}
return(merge)
}

sammenlagte <- list()
for (i in 1:nrow(result2)){
saml  <- smeltkliker(result2, i, max.klike)
sammenlagte <- c(sammenlagte, saml)
}
return(sammenlagte)
}

result1         <- clique.overlap(max.klike)
result2         <- result1>= 80
diag(result2)   <- FALSE
max(unlist(lapply(max.klike,length)))
sam1            <- forbind.kliker(result2, max.klike)
max(unlist(lapply(sam1,length)))
samres2         <- clique.overlap(sam1) 
samres3         <- samres2 >= 80
diag(samres3)   <- FALSE
sam4            <- forbind.kliker(samres3, sam1)
max(unlist(lapply(sam4,length)))

samres4         <- clique.overlap(sam4) 
samres4         <- samres4 >= 80
diag(samres4)   <- FALSE
sam5            <- forbind.kliker(samres4, sam4)
max(unlist(lapply(sam4,length)))


##### Andre algoritmer

?communities
fast <- fastgreedy.community(net)
mem <- membership(fast)
table(mem)
sort(degree(net)[mem==2])

##### Social Circles

who(net, "Lars Nørby Johansen")

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
sort(degree(del))

central     <- vector(length=length(klynger))
central[klynger == 1] <- "Inder kredsen"
central[klynger == 5] <- "Banker og fagforeninger"
central[klynger == 4] <- "Prospects"
central[central == FALSE] <- "Ikke central"
table(central)

gplot(net, edge.alpha=0.1, vertex.fill=as.factor(central), text.alpha=0, vertex.size=4)


net.inner <- net - which(klynger != 1)
save(net.inner, file="net.inner")

################### JUNK

### 
transitivity(net, type="undirected")
transitivity(net.ind, type="undirected")

barab <- barabasi.game(535)
transitivity(barab)

w <- watts.strogatz.game(1,535,5, 0.05)
transitivity(w)

a <- graph.density(net)

rand <- erdos.renyi.game(535,a )
V(rand)$name <- 1:535
gplot(rand)
b <- graph.density(net.ind)


transitivity(erdos.renyi.game(6036,b ))


lay <- layout.fruchterman.reingold(barab)
V(barab)
gplot(barab, lay)

