
###################################################################################################################################
################################### Describe network by variable ##################################################################
setwd("~/My Dropbox/R/interlocks")
load("corporate.interlocks")
library(igraph)


describe.network <- function(
  graph    = net.org
  variable = org$BØRSEN.BRANCHE
  diagonal = diagonal.org
  
  
Antal virksomheder:
Antal edges:
# Antal direktører:
Average degree:
Average betweenness:
Largest component:
% isolates:
% af medlemmer i core component:
% af core component:

    between     <- betweenness(graph)
    
    
    #ALLE
  nvertex.all <- vcount(graph)
  nedges.all  <- ecount(graph)
  #ndirectors  <- 
  
  
    
    
    
    