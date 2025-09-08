#' Check whether two objects are aligned
#' 
#' @description
#' `validate_alignment` checks that the two objects have the same
#' `shape` attribute and, if coordinate information is available, the
#' same coordinate system given by the attributes `bounds`, `crs`
#' and `transform`.
#' 
#' @param x1 matrix or array or GRIDobj or FLOWobj or STREAMobj
#' 
#' The first object to check
#' 
#' @param x2 matrix or array or GRIDobj or FLOWobj or STREAMobj
#' 
#' The second object to check
#' 
#' @return logical
#' 
#' True if the two objects are aligned, False otherwise
#' 
#' @examples
#' \dontrun{
#' DEM <- terra::rast(matrix(1:25, 5, 5), crs="EPSG:25833")
#' FD <- FLOWobj(DEM)
#' print(validatealignment(DEM, FD))
#' }
#' 
#' @note
#' `validatealignment` currently only handles arrays, matrices and FLOWobj.
#' 
#' @import terra
#' 
#' @export

validatealignment <- function(x1, x2) {
  # Check whether x1 and x2 contains a SpatRaster and get position in object
  idx_r1 <- which(sapply(x1, inherits, "SpatRaster"))[1]
  idx_r2 <- which(sapply(x2, inherits, "SpatRaster"))[1]

  if (!is.na(idx_r1) & !is.na(idx_r2)){
    # If both objects contain SpatRasters: validate with terra::compareGeom()
    check <- terra::compareGeom(x1[[idx_r1]],
                                x2[[idx_r2]])
  } else {
    # Else: validate dimensions match
    check <- all(get_dims(x1) == get_dims(x2))
  }
  return(check)
}