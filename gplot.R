gplot <- function(graph, vertex.coord=NULL, vertex.color="black", vertex.fill="grey60", vertex.shape=21, vertex.size=3, vertex.alpha=1,
                  edge.colour="black", edge.alpha=0.2,
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
  edge.mat                <- mat
  vertex.coord$id         <- rownames(vertex.coord)
    
  pnet <- ggplot()
  pnet <- pnet + geom_point(data=vertex.coord, aes(x=x, y=y), colour=vertex.color, shape=vertex.shape, fill="grey60",  size=vertex.size, alpha=vertex.alpha)
  pnet <- pnet + geom_segment(aes(x=start.x, y=start.y, xend=slut.x, yend = slut.y, size=weight), data=edge.mat, colour=edge.colour, alpha=edge.alpha)
  pnet <- pnet + geom_text(data=vertex.coord, aes(x=x, y=y, label=id), size=text.size, alpha=text.alpha, color=text.colour)
  pnet + theme_bw()
}