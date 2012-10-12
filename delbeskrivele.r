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
  rownames(output) <- c("Number of vertices", "Number of edges", "Average degree", "Part density (‰)", "Isolates (%)",
                        "Largest component (%)", "Average path length", "Longest path", "Highest degree", "Largest 2. neighborhood", "Largest 3. neighborhood")
  return(output)
# Net er et igraph object
# Variabel er en factor i samme længde og orden som den adjacency matrice net er lavet fra
  }