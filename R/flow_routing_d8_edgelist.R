#' wrap_flow_routing_d8_edgelist
#'
#' Compute downstream pixel indices from flow directions.
#' 
#' @param DEM GRIDobj
#' 
#' Digital elevation model
#' 
#' @param bc numeric array or matrix, optional
#' 
#' Boundary conditions for sink filling. `bc` should match the shape of the DEM.
#' Values of 1 indicate pixels that should be fixed to their values in the
#' original DEM and values of 0 indicate pixels that should be filled.
#' 
#' @param hybrid logical, optional
#' 
#' Should hybrid reconstruction algorithm be used to fill sinks? Defaults to
#' True. Hybrid reconstruction is faster but requires additional memory be
#' allocated for a queue.
#'
#' @import terra
#'
#' @return A list containing vectors of source and target pixels for each edge.
#' 
#' @export

flow_routing_d8_edgelist <- function(DEM,
                                     bc=NULL,
                                     hybrid=TRUE){
  
  # Compute inputs from raw DEM
  SD <- flow_routing_d8_carve(DEM, bc=bc, hybrid=hybrid)
  nodes <- get_grid_data(SD$source)
  directions <- get_grid_data(SD$direction)
  
  # Compute flow routing using libtopotoolbox
  outputs <- single(length(nodes$z))
  results <- .C("wrap_flow_routing_d8_edgelist",
                edge_countR = integer(1),
                sourceR = as.integer(outputs), # ptrdiff_t
                targetR = as.integer(outputs), # ptrdiff_t
                nodeR = as.integer(nodes$z), # ptrdiff_t
                directionR = as.integer(directions$z), # uint8_t
                dimsR = as.integer(nodes$dims), # ptrdiff_t
                NAOK = TRUE)
  
  # Write outputs
  return(list("source" = results$sourceR[1:results$edge_countR],
              "target" = results$targetR[1:results$edge_countR]))
}