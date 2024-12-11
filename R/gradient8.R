#' wrap_gradient8
#'
#' This will make the Gradient8 funktion available to R from the libtotopotoolbox subdirectory
#' 
#' @param dem Input of type Spatrast from Terra
#' @param use_mp Future feature that will allow to parallelize the code
#'
#' @import terra
#'
#' @return A Spatrast
#' @export

gradient8 <- function(dem,use_mp=0) {
    
    d <- get_grid_data(dem) # Extract input data
    output <- single(length(d$z)) #create output array
    result <- .C("wrap_gradient8",outputR=as.single(output),as.single(d$z),as.single(d$cellsize),as.integer(use_mp), as.integer(d$dims))$outputR

    G <- dem # copy Spatrast
    terra::values(G) <- result #update Spatrast values
    
    return(G)
}
