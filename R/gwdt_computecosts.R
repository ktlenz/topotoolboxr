#' wrap_gwdt_computecosts
#'
#' This function makes the libopotoolbox cost computation for the gray-weighted 
#' distance transform available in R
#' 
#' @param flats 
#' @param original_dem (terra::SpatRaster) Raw digital elevation model 
#' @param filled_dem (terra::SpatRaster) Processed DEM
#'
#' @import terra
#'
#' @return List containing costs and connected components
#' @export

gwdt_computecosts <- function(flats, original_dem, filled_dem){
  fl <- get_grid_data(flats)
  dr <- get_grid_data(original_dem)
  df <- get_grid_data(filled_dem)
  
  # Check dims
  if (!identical(fl$dims, dr$dims) || !identical(fl$dims, df$dims)) {
    stop("All input grids must have the same dimensions.")
  }
  
  # Compute costs using libtopotoolbox
  costs <- single(length(dr$z))
  conncompsR <- integer(length(dr$z))
  results <- .C("wrap_gwdt_computecosts",
                costsR=as.single(costs),conncompsR=as.integer(conncompsR),
                as.single(fl$z),as.single(dr$z),as.single(df$z),
                as.integer(fl$dims))

  # Write results into SpatRaster
  costs <- flats
  terra::values(costs) <- results$costs
  comps <- flats
  terra::values(comps) <- results$conncomps
  
  return(list("Costs" = costs,
              "Conncomps" = comps))
}