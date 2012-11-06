####### Del.beskrivelse

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
  rownames(output) <- c("Number of vertices", "Number of edges", "Average degree", "Part density (o/oo)", "Isolates (%)",
                        "Largest component (%)", "Average path length", "Longest path", "Highest degree", "Largest 2. neighborhood", "Largest 3. neighborhood")
  return(output)
# Net er et igraph object
# Variabel er en factor i samme længde og orden som den adjacency matrice net er lavet fra
  }

#########################################################################################################
################################# Cluster beskrivelse ###################################################
net <- net.eig
variabel <- k4
i <- 1

cluster.beskrivelse <- function(net, variabel){
  variabel <- as.factor(variabel)
  dele <- levels(variabel)
  output <- matrix(nrow=20, ncol=length(dele)) # Output matrix
  for ( i in 1:length(dele)){
    del <- dele[i]
    del.ind <- which(variabel==del)
    del.not <- which(variabel!=del)
    net.del             <- net - del.not
    
    # Antal Vertices
    Number.of.vertices  <- length(del.ind)
    # Antal edges
    Number.of.edges     <- sum(degree(net)[del.ind])
    # Average degree
    Average.degree      <- round(Number.of.edges/Number.of.vertices, 1)
    # Part density i 1000
    Part.density        <- round(Number.of.edges/((Number.of.vertices*(vcount(net)-1)/2))*1000, 1)
    # Clusters in part
    Number.of.clusters.in.del  <- clusters(net.del)$no
      
    # Average path length total network
    sp                  <- shortest.paths(net)
    ind.av.sp           <- rowSums(sp)[del.ind]/ncol(sp)
    Average.path.length <- round(sum(ind.av.sp)/length(del.ind),1)
    # Average path length within group
    sp.del                  <- shortest.paths(net)[del.ind,del.ind]
    ind.av.sp.del           <- rowSums(sp.del)/length(del.ind)
    Average.path.length.del <- round(sum(ind.av.sp.del)/length(del.ind),1)
    
    # Longest path within group
    Longest.path.del    <- max(sp.del)
      
    # Largest number of degrees
    Largest.degree <- max(degree(net)[del.ind])
    # Largest degree in part
    Largest.degree.del <-max(degree(net.del))
    # Largest 2 neighborhoods
    Largest.2.neighborhood <- max(neighborhood.size(net, 2)[del.ind])
    # Largest 3 neighborhoods
    Largest.3.neighborhood <- max(neighborhood.size(net, 3)[del.ind])
    
    # Average closeness whole network * 10000
    Average.closeness.network    <- round(sum(closeness(net)[del.ind])/length(del.ind) * 10000, 1)
    # Average closeness part
    Average.closeness.part       <- round(sum(closeness(net.del))/length(del.ind) * 10000, 1)
    # Average betweenness whole network
    Average.betweenness.network  <- round(sum(betweenness(net)[del.ind])/length(del.ind))
    # Average betweeness part
    Average.betweenness.part     <- round(sum(betweenness(net.del))/length(del.ind))
    # Maximum betweeness whole network
    Maximum.betweenness          <- max(betweenness(net)[del.ind])
    # Maximum closeness whole network * 10000
    Maximum.closeness            <- round(max(closeness(net)[del.ind]) * 10000, 1)
    # Average eigenvector centrality * 1000
    Average.eigen.network        <- round(sum(evcent(net)$vector[del.ind])/length(del.ind) * 1000, 1)
    # Maximum eigenvector centrality
    Maximum.eigen                <- round(max(evcent(net)$vector[del.ind])* 1000, 1)
    
    del.stat <- c(Number.of.vertices, Number.of.edges, Average.degree, Part.density, Number.of.clusters.in.del,
                  Average.path.length, Average.path.length.del, Longest.path.del, Largest.degree, Largest.degree.del,
                  Largest.2.neighborhood, Largest.3.neighborhood,
                  Average.closeness.network, Average.closeness.part, Maximum.closeness,
                  Average.betweenness.network, Average.betweenness.part, Maximum.betweenness,
                  Average.eigen.network, Maximum.eigen)
    
    
    output[,i] <- round(del.stat, 1)
  }
  colnames(output) <- dele
  rownames(output) <- c("Number of vertices", "Number of edges", "Average degree", "Part density (o/oo)", "Number of clusters in part",
                        "Average path length", "Average path length in part", "Longest path in part", "Highest degree", "Highest degree in part",
                        "Largest 2. neighborhood", "Largest 3. neighborhood",
                        "Average closeness", "Average closeness in part", "Maximum closeness",
                        "Average betweeness", "Average betweenness in part", "Maximum betweenness",
                        "Average eigencentrality", "Maximum eigencentrality")
  return(output)
  # Net er et igraph object
  # Variabel er en factor i samme længde og orden som den adjacency matrice net er lavet fra
}






