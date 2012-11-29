
###################################################################################################################################
################################### Describe network by variable ##################################################################
setwd("~/My Dropbox/R/interlocks")
load("corporate.interlocks")
library(igraph)


org <- org[order(org$ORG_NAVN),]

variabel <-cut(org$OMSÆTNING.10, breaks=quantile(org$OMSÆTNING.10, na.rm=TRUE), include.lowest=TRUE, labels= c("1. kvartil: Mindst", "2. kvartil", "3. kvartil", "4. kvartil: Størst" ))

describe.network <- function(graph, variabel, org.data){
  
  #ALLE
    between       <- betweenness(graph)
    neighborhood.size.3 <- neighborhood.size(graph, 3)
    degrees       <- degree(graph)
    core.com      <- clusters(graph) 
    core.com.mem  <-  core.com$membership==which.max(core.com$csize)
  
  nvertex.all                 <- vcount(graph)
  nedges.all                  <- ecount(graph)
  percentage.in.largest.com   <- sum(core.com.mem)/nvertex.all * 100
  Average.degree              <- sum(degrees)/nvertex.all
  Average.betweenness         <- sum(between)/nvertex.all
  Average.3.neighborhood.size <- sum(neighborhood.size.3)/nvertex.all

  result.matrix <- as.data.frame(matrix(nrow = 6, ncol=1+nlevels(variabel)))
  res.all       <- c(nvertex.all, nedges.all, percentage.in.largest.com, Average.degree, Average.betweenness, Average.3.neighborhood.size)
  result.matrix[,1] <- res.all
  
  levels.variabel <- levels(variabel)
  
  # Del
  for( i in 1:nlevels(variabel)){
  graph.part               <- graph - which(variabel!=levels.variabel[i])
  part.ind                 <- which(variabel==levels.variabel[i]) 
  between.part             <- between[part.ind]
  neighborhood.size.3.part <- neighborhood.size.3[part.ind]
  degrees.part             <- degrees[part.ind]
  core.com.mem.part        <- core.com.mem[part.ind]
  
  nvertex.part                     <- vcount(graph.part)
  nedges.part                      <- ecount(graph.part)
  percentage.in.largest.com.part   <- sum(core.com.mem.part)/nvertex.part * 100
  Average.degree.part              <- sum(degrees.part)/nvertex.part
  Average.betweenness.part         <- sum(between.part)/nvertex.part
  Average.3.neighborhood.size.part <- sum(neighborhood.size.3.part)/nvertex.part
  
  res.part       <- c(nvertex.part, nedges.part, percentage.in.largest.com.part, Average.degree.part, Average.betweenness.part, Average.3.neighborhood.size.part)
  result.matrix[,i+1] <- res.part  
  }  
    
    colnames(result.matrix) <- c("All", levels.variabel)
    rownames(result.matrix) <- c("Corporations", "Ties", "% in central component", "Average degree", "Average betweeness", "Average 3rd neighborhoodsize")
  
    round(result.matrix, 1)
  }

tab <- describe.network(net.org, variabel, org)
write.csv(tab, file="tab.csv")
tab <- describe.network(net.org, org$BØRSEN.BRANCHE, org)
write.csv(tab, file="tab.csv")



  
  