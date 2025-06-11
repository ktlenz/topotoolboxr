#' wrap_identifyflats
#'
#' This will make the identifyflats function available to R from the libtotopotoolbox subdirectory
#' 
#' @param dem Input of type Spatrast from Terra
#'
#' @import terra
#'
#' @return A Spatrast
#' @export

identifyflats <- function(dem) {
  # Extract input data
  d <- get_grid_data(dem)
  
  # Handle NaN values
  log_nans <- is.na(d$z)
  d$z[log_nans] <- min(d$z, na.rm = T) - 999
  
  # Compute flats using libtopotoolbox
  output <- integer(length(d$z))
  result <- .C("wrap_identifyflats",outputR=as.integer(output),as.single(d$z),as.integer(d$dims))$outputR
  
  # Write results into SpatRaster
  flats <- dem
  terra::values(flats) <- result
  #flats_pre_nan <- flats
  #terra::values(flats)[log_nans] <- 0 # Replace NaNs
  
  return(flats)
}
