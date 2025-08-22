#' wrap_flow_routing_d8_carve
#'
#' Compute the flow routing using the D8 algorithm with carving for flat areas.
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

#' @import terra
#'
#' @return A list containing two terra::SpatRaster representing the source cells
#' for flow routing (source) and the flow direction for each grid cell (direction).
#' 
#' @export

flow_routing_d8_carve <- function(DEM,
                                  bc=NULL,
                                  hybrid=TRUE){
  
  # Prepare inputs for flow routing based on raw DEM
  DEMf <- fillsinks(DEM, bc=bc, hybrid=hybrid)
  FLATS <- identifyflats(DEMf)
  DIST <- gwdt(DEM)
  
  demf <- get_grid_data(DEMf)
  dist <- get_grid_data(DIST)
  flats <- get_grid_data(FLATS)
  
  # Compute flow routing using libtopotoolbox
  outputs <- single(length(demf$z))
  results <- .C("wrap_flow_routing_d8_carve",
                sourceR = as.integer(outputs), # ptrdiff_t
                directionR = as.integer(outputs), # uint8_t
                demR = as.single(demf$z), # float
                distR = as.single(dist$z), # float
                flatsR = as.integer(flats$z), # uint32_t
                dimsR = as.integer(demf$dims), # ptrdiff_t
                NAOK = TRUE)
  
  # Write outputs into SpatRaster
  source <- DEMf
  terra::values(source) <- results$sourceR
  direction <- DEMf
  terra::values(direction) <- results$directionR
  
  return(list("source" = source,
              "direction" = direction))
}
