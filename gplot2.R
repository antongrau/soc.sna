### Problemer
# Vi kan ikke have mere end 1 af hver slags skala - det er særligt irriterende med size, 
# fordi den skal bruges til både edges og vertices.
# Hadley siger at han synes det er en dårlig ide at have mere end en scale... træls!
# En mulig løsning er at dele scales op imellem edges og vertices 
# edges alpha, linetype og colour
# vertex: size, fill, shape
# Text



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