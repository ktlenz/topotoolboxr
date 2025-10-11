#' Create instance of a GRIDobj
#' 
#' @description
#' GRIDobj creates an instance of the grid class, which basically builds on 
#' the \code{SpatRaster} structure.
#' 
#' 
#' Coordinate reference systems must be provided in PROJ-string nomenclature. 
#' See the official PROJ website for further information, examples and an 
#' introduction: https://proj.org/en/stable/usage/quickstart.html.
#' 
#' The function will stop if the resolution of the imported data set in x 
#' and y direction is not identical. In that case, consider manual import of 
#' the data set via \code{terra::rast()} and resampling to identical  
#' resolution (cf. \code{terra::resample()}).
#' 
#' DEM = GRIDobj(Z) creates a GRIDobj of a numeric matrix \code{Z} and a 
#' default resolution of \code{1} m.
#' 
#' DEM = GRIDobj(Z, cellsize) creates a GRIDobj from the elevations stored in 
#' the matrix Z and \code{cellsize} is a positive value defining the spatial 
#' resolution. 
#' 
#' DEM = GRIDobj(Z, cellsize, crs) creates a GRIDobj from the elevations stored
#' in the matrix Z, \code{cellsize} is a positive value defining the spatial 
#' resolution and \code{crs} is a character defining the coordinate reference
#' system.
#' 
#' DEM = GRIDobj(Z, x, y) creates a GRIDobj from the coordinate matrices
#' or vectors X and Y and the matrix Z. The elements of Z refer to the
#' elevation of each pixel.
#' 
#' DEM = GRIDobj("path/to/file") creates a GRIDobj by reading the file
#' leveraging the \code{terra::rast()} function.
#' 
#' DEM = GRIDobj(NULL) creates an empty instance of GRIDobj. The Matlab 
#' equivalent is \code{GRIDobj([])}.
#' 
#' DEM = GRIDobj(GRIDobj or FLOWobj or STREAMobj, class) creates an
#' instance of GRIDobj with all common properties (e.g., spatial
#' referencing) inherited from another instance of a FLOWobj, GRIDobj 
#' or STREAMobj class. DEM.Z is set to all zeros where class can be
#' integer classes or double or single. By default, class is double.
#' 
#' DEM = GRIDobj(GRIDobj or FLOWobj or STREAMobj, Z) creates an
#' instance of GRIDobj with all common properties (e.g., spatial
#' referencing) inherited from another instance of a FLOWobj, GRIDobj 
#' or STREAMobj class. The second input argument Z is written to DEM.Z.
#' 
#' @param x character | SpatRaster | matrix | array | GRIDobj | FLOWobj
#' 
#' character: Path to a .tif file to read as a GRIDobj
#' 
#' SpatRaster: A SpatRaster object from terra to transform into a GRIDobj
#' 
#' matrix | array: A numeric or logical matrix to transform into a GRIDobj
#' 
#' GRIDobj | FLOWobj: A TTobj for which to copy common properties into a GRIDobj
#' 
#' @param \dots Further arguments, either:
#' cellsize: float or integer scalar representing the cellsize of the grid for GRIDobj(Z, cs)
#' 
#' x: integer or float vector representing x coordinates for GRIDobj(Z, x, y)
#' 
#' y: integer or float vector representing y coordinates for GRIDobj(Z, x, y)
#' 
#' @returns GRIDobj
#' Returns a GRIDobj containing the SpatRaster object and the file path.
#' 
#' @note
#' Note that while throughout this help text GRIDobj is associated with
#' gridded digital elevation models, instances of GRIDobj can contain
#' other gridded, single band, datasets such as flow accumulation grids, 
#' gradient grids etc.
#' 
#' @export

GRIDobj <- function(x, ...) {
  args <- list(...)
  if(is.null(x)) { # Case 1: GRIDobj(NULL)
    raster <- terra::rast(
      xmin = 0, xmax = 1,
      ymin = 0, ymax = 1,
      vals = matrix(0),
      crs = '',
      resolution = c(1,1),
      names = ''
    )
    path = ''
  } else if (is.character(x)) { # Case 2: GRIDobj("path/to/file")
    # Validate whether a proper path was provided
    if (!file.exists(x)) stop("The provided filepath does not exist.")
    
    # Load file as terra SpatRaster and store path attribute
    raster <- terra::rast(x) 
    if (terra::nlyr(raster) > 1) stop("topotoolboxr only supports single layer rasters.")
    if (!identical(terra::xres(raster), terra::yres(raster))) stop("x and y resolutions must be identical.")
    if (terra::is.lonlat(raster)) stop("Coordinate reference system is not projected.")
    path <- x
    
  } else if (inherits(x, "SpatRaster")) { # Case 3: GRIDobj(SpatRaster)
    # Store GRIDobj information
    raster <- x
    if (terra::nlyr(raster) > 1) stop("topotoolboxr only supports single layer rasters.")
    if (!identical(terra::xres(raster), terra::yres(raster))) stop("x and y resolutions must be identical.")
    if (terra::is.lonlat(raster)) stop("Coordinate reference system is not projected.")
    path <- ''
  } else if (is.numeric(x)) { # Case 4: GRIDobj(Z, ...)
    # Case 4.1 + 4.2: GRIDobj(Z) or GRIDobj(Z, cs)
    if (length(args) == 0) {
      dims <- get_dims(x)
      # SpatRaster without georeference information is created
      raster <- terra::rast(
        xmin = 0, xmax = dims[1],
        ymin = 0, ymax = dims[2],
        crs = '',
        resolution = c(1, 1), # Cellsize set to 1
        names = ''
      )
      terra::values(raster) <- c(t(x)) # Transform matrix into vector with row-major order
      path <- ''
    } else if (length(args) == 1 & is.numeric(args[[1]])) {
      dims <- get_dims(x)
      cs <- args[[1]]
      # SpatRaster without georeference information is created
      raster <- terra::rast(
        xmin = 0, xmax = dims[1] * cs,
        ymin = 0, ymax = dims[2] * cs,
        crs = '',
        resolution = c(cs, cs),
        names = ''
      )
      terra::values(raster) <- c(t(x)) # Transform matrix into vector with row-major order
      path <- ''
    } else if (length(args) == 2 & is.character(args[[2]])) { # Case 4.3: GRIDobj(Z, cs, crs)
      dims <- get_dims(x)
      # SpatRaster without georeference information is created
      raster <- terra::rast(
        xmin = 0, xmax = dims[1] * args[[1]],
        ymin = 0, ymax = dims[2] * args[[1]],
        crs = args[[2]],
        resolution = c(args[[1]], args[[1]]),
        names = ''
      )
      terra::values(raster) <- c(t(x)) # Transform matrix into vector with row-major order
      path <- ''
    } else if (length(args) == 2 & is.numeric(args[[1]]) & is.numeric(args[[2]])) { # Case 4.4: GRIDobj(Z, x, y)
      # Retrieve coordinates and check whether they match
      x_coords <- args[[1]]
      y_coords <- args[[2]]
      if (!identical(get_dims(x), c(length(x_coords), length(y_coords)))) stop("Dimensions of Z and lengths of x and y do not match.")
      
      # Determine cellsize
      dx <- unique(diff(x_coords))
      dy <- unique(diff(y_coords))
      if (length(dx) != 1 | length(dy) != 1 | !identical(dx, dy)) stop("TopoToolbox requires identical resolution in x and y direction.")
      cs <- dx
      
      # Create SpatRaster
      raster <- terra::rast(
        xmin = min(x_coords-cs/2), xmax = max(x_coords+cs/2),
        ymin = min(y_coords-cs/2), ymax = max(y_coords+cs/2),
        crs = '',
        resolution = c(cs, cs),
        names = ''
      )
      terra::values(raster) <- c(t(x)) # Transform matrix into vector with row-major order
      path <- ''
    }
  } else if (inherits(x, "GRIDobj")){ # Case 5: GRIDobj(GRIDobj, ...)
    raster <- x$raster
    # Determine whether class was specified
    if (length(args) == 0) { # Case 5.1: GRIDobj(TTobj)
      terra::values(raster) <- single(length(terra::values(raster)))
      cl <- 'single' # No class provided, set to single
    } else { # Case 5.2: GRIDobj(TTobj, Z)
      cl <- args[[1]]
    }
    
    # Case 5.3: GRIDobj(TTobj)
    if (is.character(cl)) {
      if(!(cl %in% c('single', 'double', 'integer', 'int', 'logical', 'boolean', 'bool'))) stop("Invalid class character provided.")
      if (cl %in% c('integer', 'int')) {
        terra::values(raster) <- integer(length(terra::values(raster)))
      } else if (cl == 'double') {
        terra::values(raster) <- double(length(terra::values(raster)))
      } else if (cl %in% c('logical', 'boolean', 'bool')) {
        terra::values(raster) <- logical(length(terra::values(raster)))
      }
    } else if (is.matrix(cl) || is.array(cl)) {
      if (!validatealignment(raster, args[[1]])) stop("Size of TopoToolbox object and input matrix does not match.")
      terra::values(raster) <- c(t(args[[1]])) # Transform matrix into vector with row-major order
    }
    
    # Set other GRIDobj attributes
    path <- ''

  } else stop("Invalid input provided")
  
  # Create and return the GRIDobj
  G <- list(
    'raster' = raster,
    'path' = path
  )
  class(G) <- "GRIDobj"
  return(G)
}

#' Get the cell values
#' 
#' @description
#' `get_values.GRIDobj` retrieves the cell values of a GRIDobj in row-major order.
#' 
#' @param x GRIDobj
#' @return numeric vector
#'
#' @export
get_values.GRIDobj <- function(x) {
  return(c(terra::values(x$raster)))
}

#' Get the dimensions
#' 
#' @description
#' `get_dims.GRIDobj` retrieves the  dimensions of the GRIDobj in the correct
#' order for libtopotoolbox.
#' 
#' @param x GRIDobj
#' @return numeric vector
#' 
#' Dimensions of the GRIDobj
#' 
#' @export
get_dims.GRIDobj <- function(x){
  return(c(terra::ncol(x$raster), terra::nrow(x$raster)))
}

#' Get the cellsize
#' 
#' @description
#' `get_cellsize.GRIDobj` retrieves the cellsize of the GRIDobj.
#' 
#' @param x GRIDobj
#' @return numeric scalar
#' 
#' Cellsize of the GRIDobj
#'
#' @export
get_cellsize.GRIDobj <- function(x){
  return(terra::xres(x$raster))
}

#' Get the extent of a GRIDobj
#' 
#' @description
#' `get_extent.GRIDobj` retrieves the extent of the GRIDobj.
#' 
#' @param x GRIDobj
#' @return named numeric vector
#' 
#' xmin, xmax, ymin and ymax of the GRIDobj
#' 
#' @export
get_extent.GRIDobj <- function(x){
  return(as.vector(terra::ext(x$raster)))
}

#' Change the projection
#' 
#' @description
#' `reproject` changes the projection of a GRIDobj.
#' 
#' @param x GRIDobj
#' @param y character | GRIDobj
#' @param ... Further arguments
#' @return GRIDobj `x` with projeciton from `y`
#' 
#' See also \link[terra]{project}
#' 
#' @export
reproject <- function(x, y, ...) {
  if (terra::is.lonlat(y)) stop("Coordinate reference system of y is not projected.")
  if (inherits(y, "GRIDobj")) {
    x$raster <- terra::project(x$raster, y$grid, ...)
  } else {
    x$raster <- terra::project(x$raster, y, ...)
  }
  return(x)
}

#' Plot a GRIDobj
#' 
#' @description
#' `plot.GRIDobj` plots the values of a GRIDobj.
#' 
#' @param x GRIDobj
#' @param ... Further arguments
#' 
#' See also \link[graphics]{plot}
#' 
#' @export
#' @method plot GRIDobj
plot.GRIDobj <- function(x, ...) {
  terra::plot(x$raster, ...)
}

#' Print key variables and statistics of the GRIDobj
#' 
#' @description
#' `info` retrieves metadata of the GRIDobj and computes key statistics, e.g.
#' mean, standard deviation, ...
#' 
#' @param x GRIDobj
#' 
#' @export
info <- function(x) {
  cat("Name:", terra::names(x$raster), "\n")
  cat("Path:", x$path, "\n")
  dims <- dim(x)
  cat("Rows:", dims[2], "\n")
  cat("Cols:", dims[1], "\n")
  cat("Cellsize:", get_cellsize(x), "\n")
  cat("Extent:", paste(get_extent(x), collapse = " "), "(xmin, xmax, ymin, ymax)\n")
  cat("CRS:", terra::crs(x$raster), "\n")
  cat("Z values:\n")
  Z <- get_values(x)
  q <- stats::quantile(Z, probs = c(0.25, 0.5, 0.75), na.rm = T)
  cat("- Mean:", base::mean(Z, na.rm=T), "\n")
  cat("- Sd:", stats::sd(Z, na.rm=T), "\n")
  cat("- Minimum:", base::min(Z, na.rm=T), "\n")
  cat("- 1st Qu.:", q[1], "\n")
  cat("- Median:", q[2], "\n")
  cat("- 3rd Qu.:", q[3], "\n")
  cat("- Maximum:", base::max(Z, na.rm=T), "\n")
}
