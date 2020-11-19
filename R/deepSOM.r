#' Process a complete Self-Organizing Map analysis
#'
#' @family Self-Organizing Map (SOM)
#'
#' @param X A feature data set.
#' @param xdim x-dimension of the grid.
#' @param ydim y-dimension of the grid.
#' @param topo Topology of the grid: \code{rectangular} or \code{hexagonal}.
#' @param neighbourhood.fct The type of neighbourhood function: \code{bubble} or \code{gaussian}.
#' @param alpha Learning rate, a vector of two numbers indicating the amount of change.
#' @param radius The radius of the neighborhood.
#' @param distance.method The distance measure to be used.
#' @param agglomeration.method The agglomeration method to be used.
#' @param cluster The number of clusters.
#' @param colour.palette A palette with colour codes for the different clusters.
#'
#' @return The fitted SOM model.
#' @export
#' 
#' @seealso \code{\link[kohonen]{unit.distances}}, \code{\link[kohonen]{supersom}}, \code{\link[kohonen]{plot.kohonen}},
#'   \code{\link[stats]{dist}}, \code{\link[stats]{hclust}}.
#'
#' @examples
fit.SOM <- function(X, xdim = NULL, ydim = NULL, 
                    topo = c("rectangular", "hexagonal"), 
                    neighbourhood.fct = c("bubble", "gaussian"),
                    alpha = c(0.05, 0.01),
                    radius = 1,
                    distance.method = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
                    agglomeration.method = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"),
                    cluster = 3,
                    colour.palette = c('#FF0000', '#00FF00', '#0000FF')) {
  X <- as.ANN.matrix(X) # rebuild dataset as matrix
  
  if (is.null(xdim) && is.null(ydim)) {
    xdim <- floor(sqrt(NROW(X)))
    ydim <- xdim
  }
  
  topo <- match.arg(topo) # use first argument value for topology
  neighbourhood.fct <- match.arg(neighbourhood.fct) # use first argument value for neighbourhood function
  
  # Create grid
  som.grid <- kohonen::somgrid(xdim = xdim, ydim = ydim, topo = topo, neighbourhood.fct = neighbourhood.fct)
  som.model <- kohonen::som(X, grid = som.grid, alpha = alpha, radius = radius)
  
  # Training progress: x-axis = number of iterations, y-axis = mean distance to closest unit
  plot(som.model, type = "changes")
  
  # Counts within grid units (output units): how many input samples are mapped to each grid unit?
  plot(som.model, type = "count")
  plot(som.model, type = "mapping")
  # The following mapping schema indicates to which grid unit each individual input sample was assigned
  # The counting of the mapping starts at the bottom left, continues to the right, jumps up at the end of the row and starts with the left unit again
  # The first number is the number of the grid unit to which the first record is assigned. The second number is the number of the grid unit to which the second record is assigned, and so on
  print(som.model$unit.classif)
  print(table(som.model$unit.classif)) # The total number of assigned input samples per each grid unit
  
  # Codebook vector: vector of the weights of a grid unit
  print(som.model$codes)
  plot(som.model, type = "codes", palette.name = rainbow)
  
  # Neighbourhood distances
  plot(som.model, type = "dist.neighbours")
  # Map quality
  plot(som.model, type = "quality")
  
  # Create clusters on grid
  # Hierarchical clustering of codebook vectors
  distance.method <- match.arg(distance.method) # use first argument value for distance method
  agglomeration.method <- match.arg(agglomeration.method) # use first argument value for linkage algorithm
  som.cluster <- cutree(hclust(dist(getCodes(som.model), method = distance.method), method = agglomeration.method), k = cluster)
  # Show the map/grid with different colours for every cluster
  plot(som.model, type = "mapping", bgcol = colour.palette[som.cluster])
  add.cluster.boundaries(som.model, som.cluster)
  # Show the same plot with graphical illustrations for the codebook vectors
  plot(som.model, type = "codes", bgcol = colour.palette[som.cluster])
  add.cluster.boundaries(som.model, som.cluster)
  
  return(som.model)
}