#' Create instance of a FLOWobj
#' 
#' @description
#' The constructor for the FlowObject. Takes a SpatRaster from terra as input,
#' computes flow direction information and saves them as an FlowObject.
#' 
#' @param DEM GRIDobj
#' 
#' The GridObject that will be the basis of the computation.
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
#' @return FLOWobj
#' 
#' An object containing vectors of source and target pixels for each edge and
#' an empty SpatRaster storing the spatial information of the original DEM.
#' 
#' @details
#' Large intermediate arrays are created during the initialization process,
#' which could lead to issues when using very large DEMs.
#' 
#' @import terra
#' 
#' @export

FLOWobj <- function(DEM,
                    bc=NULL,
                    hybrid=TRUE) {
  # Input checks
  if (!inherits(DEM, "GRIDobj") && !inherits(DEM, "SpatRaster")) {
    stop("DEM must be either a GRIDobj or a SpatRaster from terra.")
  }
  
  # Compute flow directions
  edgelist <- flow_routing_d8_edgelist(DEM, bc=bc, hybrid=hybrid)
  
  # Store spatial metadata
  DEMpty <- terra::rast(nrows = terra::nrow(DEM),
                        ncols = terra::ncol(DEM),
                        ext = terra::ext(DEM),
                        crs = terra::crs(DEM),
                        nlyrs = terra::nlyr(DEM))
  
  # Create list representation of FLOWobj
  FD <- list(sources = edgelist$source,
             targets = edgelist$target,
             #type = , # currently topotoolboxr only computes single flow directions
             raster = DEMpty)
  
  # Assign class and return FLOWobj
  class(FD) <- "FLOWobj"
  return(FD)
}