#' Create instance of a FLOWobj
#' 
#' @description
#' The constructor for the FlowObject. Takes a GRIDobj from topotoolboxr or a 
#' SpatRaster from terra as input, computes flow direction information and
#' saves them as an FlowObject.
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
#' @examples
#' \dontrun{
#' DEM <- terra::rast(system.file("ex/elev.tif",package="terra"))
#' DEM <- terra::project(DEM,"EPSG:32632",res=90.0)
#' FD <- FLOWobj(DEM)
#' }
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
  
  # Initial computations
  dem <- get_grid_data(DEM)
  output <- single(length(dem$z)) # Preallocate output for all computations
  restore_nans <- FALSE
  
  # fillsinks
  if (!is.null(bc)){ ## Check requirements for user boundary conditions
    if((is.matrix(bc)|is.array(bc))&!all(bc %in% c(0,1))) {
      # Check if bc is (matrix OR array) AND all values either 0 or 1
      stop("`bc` must be either NULL, a matrix or an array containing 0s and 1s")
    }
    if(all(dem$dims != c(ncol(bc), nrow(bc)))){
      stop("`DEM` and `bc` dimensions do not match")
    }
    bc <- t(bc)
  } else { # If the user does not provide bcs they are manually created
    bc <- matrix(0,dem$dims[1],dem$dims[2])
    # Missing data and borders are fixed to their original value
    nans <- is.na(dem$z)
    
    bc[nans] <- 1
    bc[1,] <- 1
    bc[,1] <- 1
    bc[nrow(bc),] <- 1
    bc[,ncol(bc)] <- 1
    
    dem$z[nans] <- -Inf
    restore_nans <- TRUE
  }
  ## Check requirements for hybrid (algorithm choice)
  if (!is.logical(hybrid)){
    stop("`hybrid` has to be boolean")
  }
  
  ## Fillsink computation
  if (hybrid){
    results <- .C("wrap_fillsinks_hybrid",
                  outputR=as.single(output), # float
                  as.single(dem$z), # float
                  as.integer(bc), # uint8_t
                  as.integer(dem$dims), # ptrdiff_t
                  NAOK = TRUE
                  )$outputR
  } else {
    results <- .C("wrap_fillsinks",
                  outputR=as.single(output), # float
                  as.single(dem$z), # float
                  as.integer(bc), # uint8_t
                  as.integer(dem$dims), # ptrdiff_t
                  NAOK = TRUE
                  )$outputR
  }
  filled <- results
  
  if (restore_nans) {
    dem$z[nans] <- NA
    filled[nans] <- NA
  }

  # identifyflats
  results <- .C("wrap_identifyflats",
                outputR=as.integer(output), # int32_t
                as.single(filled), # float
                as.integer(dem$dims), # ptrdiff_t
                NAOK = TRUE)$outputR
  flats <- results
  
  # gwdt_computecosts
  results <- .C("wrap_gwdt_computecosts",
                costsR = as.single(output),
                flatsR = as.integer(flats),
                original_demR = as.single(dem$z),
                filled_demR = as.single(filled),
                dimsR = as.integer(dem$dims),
                NAOK = TRUE)$costsR
  
  # gwdt
  results <- .C("wrap_gwdt",
                distR = as.single(output), # float
                costsR = as.single(results), # float
                flatsR = as.integer(flats), # int32_t
                dimsR = as.integer(dem$dims), # ptrdiff_t
                NAOK = TRUE)$distR
  
  # flow_routing_d8_carve
  results <- .C("wrap_flow_routing_d8_carve",
                sourceR = as.integer(output), # ptrdiff_t
                directionR = as.integer(output), # uint8_t
                demR = as.single(filled), # float
                distR = as.single(results), # float, results = distR from gwdt
                flatsR = as.integer(flats), # uint32_t
                dimsR = as.integer(dem$dims), # ptrdiff_t
                NAOK = TRUE)

  # Write output relevant for FLOWobj into SpatRaster
  NODE <- DEM
  terra::values(NODE) <- results$sourceR
  DIRECTION <- DEM
  terra::values(DIRECTION) <- results$directionR
  
  # flow_routing_d8_edgelist
  results <- .C("wrap_flow_routing_d8_edgelist",
                edge_countR = integer(1),
                sourceR = as.integer(output), # ptrdiff_t
                targetR = as.integer(output), # ptrdiff_t
                nodeR = as.integer(results$sourceR), # ptrdiff_t
                directionR = as.integer(results$directionR), # uint8_t
                dimsR = as.integer(dem$dims), # ptrdiff_t
                NAOK = TRUE)
  
  # Store spatial metadata
  DEMpty <- terra::rast(nrows = terra::nrow(DEM),
                        ncols = terra::ncol(DEM),
                        ext = terra::ext(DEM),
                        crs = terra::crs(DEM),
                        nlyrs = terra::nlyr(DEM))
  
  # Create list representation of FLOWobj
  FD <- list(source = results$sourceR[1:results$edge_countR],
             target = results$targetR[1:results$edge_countR],
             direction = DIRECTION,
             stream = NODE,
             #type = , # currently topotoolboxr only computes single flow directions
             raster = DEMpty)
  
  # Assign class and return FLOWobj
  class(FD) <- "FLOWobj"
  return(FD)
}
