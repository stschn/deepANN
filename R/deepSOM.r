#' @title Process a complete Self-Organizing Map analysis
#' @description
#'
#' @family Self-Organizing Map (SOM)
#'
#' @param x A feature data set.
#' @param xdim x-dimension of the grid.
#' @param ydim y-dimension of the grid.
#' @param topo Topology of the grid: \code{rectangular} or \code{hexagonal}.
#' @param neighbourhood.fct The type of neighbourhood function: \code{bubble} or \code{gaussian}.
#' @param alpha Learning rate, a vector of two numbers indicating the amount of change.
#' @param radius The radius of the neighborhood.
#' @param distance_method The distance measure to be used.
#' @param agglomeration_method The agglomeration method to be used.
#' @param cluster The number of clusters.
#' @param colour_palette A palette with colour codes for the different clusters.
#'
#' @return The fitted SOM model.
#'
#' @examples
#'   som <- fit_SOM()
#'   # The following mapping schema indicates to which grid unit each individual input sample was assigned.
#'   # The counting of the mapping starts at the bottom left, continues to the right, jumps up at the end of the row and starts with the left unit again.
#'   # The first number is the number of the grid unit to which the first record is assigned. The second number is the number of the grid unit to which the second record is assigned, and so on.
#'   som$unit.classif
#'   table(som$unit.classif) # The total number of assigned input samples per each grid unit
#'   # Codebook vector
#'   som$codes
#'
#' @seealso \code{\link[kohonen]{unit.distances}}, \code{\link[kohonen]{supersom}}, \code{\link[kohonen]{plot.kohonen}},
#'   \code{\link[stats]{dist}}, \code{\link[stats]{hclust}}.
#'
#' @export
fit_SOM <- function(x, xdim = NULL, ydim = NULL,
                    topo = c("rectangular", "hexagonal"),
                    neighbourhood.fct = c("bubble", "gaussian"),
                    alpha = c(0.05, 0.01),
                    radius = 1,
                    distance_method = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
                    agglomeration_method = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"),
                    cluster = 3L,
                    colour_palette = c('#FF0000', '#00FF00', '#0000FF')) {
  x <- data.matrix(x) # rebuild dataset as matrix

  if (is.null(xdim) && is.null(ydim)) {
    xdim <- floor(sqrt(NROW(x)))
    ydim <- xdim
  }

  topo <- match.arg(topo) # use first argument value for topology
  neighbourhood.fct <- match.arg(neighbourhood.fct) # use first argument value for neighbourhood function

  # Create grid
  grid <- kohonen::somgrid(xdim = xdim, ydim = ydim, topo = topo, neighbourhood.fct = neighbourhood.fct)
  sommodel <- kohonen::som(x, grid = grid, alpha = alpha, radius = radius)

  # Training progress: x-axis = number of iterations, y-axis = mean distance to closest unit
  plot(sommodel, type = "changes")

  # Counts within grid units (output units): how many input samples are mapped to each grid unit?
  plot(sommodel, type = "count")
  plot(sommodel, type = "mapping")
  # The following mapping schema indicates to which grid unit each individual input sample was assigned.
  # The counting of the mapping starts at the bottom left, continues to the right, jumps up at the end of the row and starts with the left unit again.
  # The first number is the number of the grid unit to which the first record is assigned. The second number is the number of the grid unit to which the second record is assigned, and so on.
  #cat(sommodel$unit.classif)
  #cat(table(sommodel$unit.classif)) # The total number of assigned input samples per each grid unit

  # Codebook vector: vector of the weights of a grid unit
  #cat(sommodel$codes)
  plot(sommodel, type = "codes", palette.name = rainbow)

  # Neighbourhood distances
  plot(sommodel, type = "dist.neighbours")
  # Map quality
  plot(sommodel, type = "quality")

  # Create clusters on grid
  # Hierarchical clustering of codebook vectors
  distance_method <- match.arg(distance_method) # use first argument value for distance method
  agglomeration_method <- match.arg(agglomeration_method) # use first argument value for linkage algorithm
  somcluster <- stats::cutree(stats::hclust(dist(kohonen::getCodes(sommodel), method = distance_method), method = agglomeration_method), k = cluster)
  # Show the map/grid with different colours for every cluster
  plot(sommodel, type = "mapping", bgcol = colour_palette[somcluster])
  kohonen::add.cluster.boundaries(sommodel, somcluster)
  # Show the same plot with graphical illustrations for the codebook vectors
  plot(sommodel, type = "codes", bgcol = colour_palette[somcluster])
  kohonen::add.cluster.boundaries(sommodel, somcluster)

  return(sommodel)
}
