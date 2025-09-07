#' Generic function for computing dimensions
#' 
#' @description
#' `get_dims` computes the dimensions of an object in the correct order for
#' libtopotoolbox.
#' 
#' @param x matrix or array or SpatRaster or GRIDobj or FLOWobj
#' 
#' Object for which to compute the dimensions
#' 
#' @return numeric vector
#' 
#' Dimensions of the object
#' 
#' @export
get_dims <- function(x) {
  UseMethod("get_dims")
}

#' Matrix dimensions
#' 
#' @description
#' `get_dims.matrix` computes the dimensions of a matrix in the correct order
#' for libtopotoolbox.
#' 
#' @param x matrix
#' 
#' Matrix for which to compute dimensions
#' 
#' @return numeric vector
#' 
#' Dimensions of the matrix
#' 
#' @export
get_dims.matrix <- function(x){
  return(as.integer(c(ncol(x), nrow(x))))
}

#' Array dimensions
#' 
#' @description
#' `get_dims.array` computes the dimensions of an array in the correct order for
#' libtopotoolbox.
#' 
#' @param x array
#' 
#' Array for which to compute dimensions
#' 
#' @return numeric vector
#' 
#' Dimensions of the array
#' 
#' @export
get_dims.array <- function(x){
  return(c(ncol(x), nrow(x)))
}

#' SpatRaster dimensions
#' 
#' @description
#' `get_dims.SpatRaster` computes the dimensions of a SpatRaster in the correct
#' order for libtopotoolbox.
#' 
#' @param x SpatRaster
#' 
#' SpatRaster for which to compute dimensions
#' 
#' @return numeric vector
#' 
#' Dimensions of the SpatRaster
#' 
#' @note
#' This function serves as a bridge until the GRIDobj is implemented.
#'
#' @import terra
#' 
#' @export
get_dims.SpatRaster <- function(x){
  return(c(terra::ncol(x), terra::nrow(x)))
}

#' Generic function for unravel_index
#' 
#' @description
#' `unravel_index` converts flat indices into a grid grid indices.
#' 
#' @param TTobj FLOWobj or STREAMobj
#' 
#' Object from which the array dimensions are computed
#' 
#' @param idxs vector or matrix or array
#' 
#' One-dimensional indices to convert into grid indices
#' 
#' @return n x 2 matrix
#' 
#' Row and column indices of each pixel in the corresponding array
#' 
#' @note
#' `unravel_index` currently only supports the FLOWobj.
#' 
#' @export
unravel_index <- function(TTobj, idxs) {
  UseMethod("unravel_index")
}

#' Generic function for source_indices
#' 
#' @description
#' `source_indices` uses `unravel_index` to compute the grid indices of
#' source pixels of a TopoToolbox object.
#' 
#' @param TTobj FLOWobj or STREAMobj
#' TopoToolbox object containing the one-dimensional source indices
#' 
#' @return n x 2 matrix
#' 
#' Row and column indices of each source pixel in the grid.
#' 
#' @note
#' `source_indices` currently only supports the FLOWobj.
#' 
#' @export
source_indices <- function(TTobj) {
  UseMethod("source_indices")
}

#' Generic function for target_indices
#' 
#' @description
#' `target_indices` uses `unravel_index` to compute the grid indices of target
#' pixels of a TopoToolbox object.
#' 
#' @param TTobj FLOWobj or STREAMobj
#' TopoToolbox object containing the one-dimensional target indices
#' 
#' @return n x 2 matrix
#' 
#' Row and column indices of each target pixel in the grid
#' 
#' @note
#' `target_indices` currently only supports the FLOWobj.
#' 
#' @export
target_indices <- function(TTobj) {
  UseMethod("target_indices")
}

#' Generic function for ezgetnal
#' 
#' @description
#' `ezgetnal` retrieves a node attribute list for a TopoToolbox object.
#' 
#' @param TTobj FLOWobj or STREAMobj
#' 
#' The TopoToolbox object for which to extract the node attribute list.
#' 
#' @param k GRIDobj | matrix | array | float
#' 
#' The object from which node values will be extracted. If `k` is a `GridObject`
#'  or a `matrix` or an `array` with the same shape as the underlying DEM of
#'  this `TTobj`, the node values will be extracted from the grid by indexing.
#'  If `k` is an array with the same shape as the node attribute list,
#'  `ezgetnal` returns a copy of `k`. If `k` is a scalar value, `ezgetnal`
#'  returns an array of the right shape filled with `k`.
#' 
#' @export
ezgetnal <- function(TTobj, k) {
  UseMethod("ezgetnal")
}
