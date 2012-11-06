#### Functions for social network analysis

############################################################
####### Plotting:

### Problemer
# Vi kan ikke have mere end 1 af hver slags skala - det er særligt irriterende med size, 
# fordi den skal bruges til både edges og vertices.
# Hadley siger at han synes det er en dårlig ide at have mere end en scale... træls!
# En mulig løsning er at dele scales op imellem edges og vertices 
# edges alpha, linetype og colour
# vertex: size, fill, shape
# Text

# Der skal være assigned en vægt til netværket...


# graph <- net
# vertex.coord <- lay
# vertex.color="black"
# vertex.fill="grey60"
# vertex.shape=21
# vertex.size=3
# vertex.alpha=1
# edge.color="black"
# edge.alpha=0.2
# text.size=3
# text.colour="black"
# text.alpha=1
# edge.line="twodash"
# edge.size=1

gplot <- function(graph, vertex.coord=NULL,
                  vertex.color="black", vertex.fill="grey60", vertex.shape=21, vertex.size=3, vertex.alpha=1,
                  edge.color="black", edge.alpha=0.2, edge.size=1, edge.line="solid",
                  text.size=3, text.colour="black", text.alpha=1){
  
  rownames(vertex.coord)  <- V(graph)$name
  vertex.coord            <- as.data.frame(vertex.coord, rownames(vertex.coord))
  colnames(vertex.coord)  <- c("x", "y")
  
  e.l <- data.frame(get.edgelist(graph))
  e.l <- cbind(e.l, E(graph)$weight)
  
  mat <- as.data.frame(matrix(ncol=5, nrow=nrow(e.l)))
  colnames(mat) <- c("start.x", "start.y", "slut.x", "slut.y", "weight")
  for(i in (1:nrow(e.l))){
    start       <- as.character(e.l[i,1])
    start.coord <- vertex.coord[rownames(vertex.coord)==start,]
    slut        <- as.character(e.l[i,2])
    slut.coord  <- vertex.coord[rownames(vertex.coord)==slut,]
    mat[i,1:2]  <- start.coord
    mat[i,3:4]  <- slut.coord
    mat[i,5]    <- e.l[i,3]
  }
  
  ## The vertex matrix
  vertex.coord$id             <- rownames(vertex.coord)
  
  # Fill
  if(length(vertex.fill)==1){
    vertex.coord$vertex.fill  <- as.factor(rep(1, nrow(vertex.coord)))
  }else{
    vertex.coord$vertex.fill <- vertex.fill
  }
  # Shape
  if(length(vertex.shape)==1){
    vertex.coord$vertex.shape  <- as.factor(rep(1, nrow(vertex.coord)))
  }else{
    vertex.coord$vertex.shape <- vertex.shape
  }
  # Size
  if(length(vertex.size)==1){
    vertex.coord$vertex.size  <- rep(1, nrow(vertex.coord))
  }else{
    vertex.coord$vertex.size <- vertex.size
  }
  
  ## Edges
  edge.mat                <- as.data.frame(mat)
  # Color
  if(length(edge.color)==1){
    edge.mat$edge.color  <- as.factor(rep(1, nrow(edge.mat)))
  }else{
    edge.mat$edge.color <- edge.color
  }
  # Alpha
  if(length(edge.alpha)==1){
    edge.mat$edge.alpha  <- rep(1, nrow(edge.mat))
  }else{
    edge.mat$edge.alpha <- edge.alpha
  }
  # Linetype
  if(length(edge.line)==1){
    edge.mat$edge.line  <- as.character(rep(edge.line, nrow(edge.mat)))
  }else{
    edge.mat$edge.line <- edge.line
  }
  
  
  ## Plotting
  pnet <- ggplot()
  
  # Text
  pnet <- pnet + geom_text(data=vertex.coord, aes(x=x, y=y, label=id), size=text.size, alpha=text.alpha, color=text.colour)
  
  # Vertex
  pnet <- pnet + geom_point(data=vertex.coord, aes(x=x, y=y, size=vertex.size, fill=vertex.fill, shape=vertex.shape), alpha=vertex.alpha, color=vertex.color)
  # Fill
  if (length(vertex.fill)==1) pnet <- pnet + scale_fill_manual(values=as.character(vertex.fill), guide="none")
  # Shape
  if (length(vertex.shape)==1) pnet <- pnet + scale_shape_manual(values=vertex.shape, guide="none")
  # Size
  if (length(vertex.size)==1) pnet <- pnet + scale_size(range=c(vertex.size,vertex.size), guide="none")
  
  
  # Edges
  pnet <- pnet + geom_segment(aes(x=start.x, y=start.y, xend=slut.x, yend = slut.y, color=edge.color, alpha=edge.alpha, linetype=as.character(edge.line)), data=edge.mat, size=edge.size)
  # Color
  if (length(edge.color)==1) pnet <- pnet + scale_colour_manual(values=as.character(edge.color), guide="none")
  # Alpha
  if (length(edge.alpha)==1) pnet <- pnet + scale_alpha(range=c(edge.alpha, edge.alpha), guide="none")
  # Linetype
  if (length(edge.line)==1) pnet <- pnet + scale_linetype_identity(guide="none")
  # Theme
  pnet + theme_bw()
  
  # This function plots igraph objects. It requires a graph object from igraph,
  # and a vertex.coord object from a 
  # The following vertex attributes can be mapped by variables of proper length:
  # Vertex.size, vertex.fill and vertex.shape
  # The following edge attributes can be mapped by variables of proper length:
  # edge.alpha, edge.linetype and edge.color
  # All other attributes can only take one value
  
}





###############################################################################
######## Analysis


network.by.variable <- function(net, variabel){
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







######## Overlapping Social Circles by Alba and Kadushin

circles <- function(graph, neighborhood=2, mode="total"){
  n2 <- neighborhood(graph, order=neighborhood)
  
  ###
  individual.hoodoverlap <- function(n2, individual, result=1){
    hood <- n2[[individual]]
    res <- vector(length=length(n2))
    for (j in 1:length(n2)){
      hood2 <- n2[[j]]
      # Andel af egne forbindelser man deler med hood2  
      hood.size          <- length(hood) #-1
      hood2.size         <- length(hood2) #-1
      hood.overlap       <- sum(hood %in% hood2)
      hood.total.size    <- hood.size + hood2.size - hood.overlap # NB er det her korrekt!
      
      overlap.total      <- hood.overlap/hood.total.size
      overlap.own        <- hood.overlap/hood.size 
      overlap.other      <- hood.overlap/hood2.size
      ind.res <- c(overlap.total, overlap.own, overlap.other, hood.total.size, hood.overlap)
      
      res[j]       <- ind.res[result]
    }
    return(res)
  }
  
  ############# Resultater
  if (identical(mode, "total")==TRUE){
    circle.mat <- matrix(nrow=length(n2), ncol=length(n2))
    
    pb <- txtProgressBar(min = 0, max = length(n2), style=3)
    
    for (i in 1:length(n2)){
      circle.mat[,i] <- individual.hoodoverlap(n2, i, result=1)
      setTxtProgressBar(pb, i, label=paste( round(i/length(n2)*100, 0), "% ready!"))
    }
    close(pb)
  }
  
  if (identical(mode, "own")==TRUE){
    circle.mat <- matrix(nrow=length(n2), ncol=length(n2))
    
    pb <- txtProgressBar(min = 0, max = length(n2), style=3)
    
    for (i in 1:length(n2)){
      circle.mat[,i] <- individual.hoodoverlap(n2, i, result=2)
      setTxtProgressBar(pb, i, label=paste( round(i/length(n2)*100, 0), "% ready!"))
    }
    close(pb)
  }
  
  if (identical(mode, "other")==TRUE){
    circle.mat <- matrix(nrow=length(n2), ncol=length(n2))
    
    pb <- txtProgressBar(min = 0, max = length(n2), style=3)
    
    for (i in 1:length(n2)){
      circle.mat[,i] <- individual.hoodoverlap(n2, i, result=3)
      setTxtProgressBar(pb, i, label=paste( round(i/length(n2)*100, 0), "% ready!"))
    }
    close(pb)
  }
  
  if (identical(mode, "overlap")==TRUE){
    circle.mat <- matrix(nrow=length(n2), ncol=length(n2))
    
    pb <- txtProgressBar(min = 0, max = length(n2), style=3)
    
    for (i in 1:length(n2)){
      circle.mat[,i] <- individual.hoodoverlap(n2, i, result=5)
      setTxtProgressBar(pb, i, label=paste( round(i/length(n2)*100, 0), "% ready!"))
    }
    close(pb)
  }
  
  rownames(circle.mat) <- V(graph)$name
  colnames(circle.mat) <- V(graph)$name
  
  return(circle.mat)
}