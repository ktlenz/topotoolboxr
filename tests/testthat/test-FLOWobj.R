test_that("test-FLOWobj.R test the proper generation of a FLOWobj", {
  # Create reference features for synthetic DEMs
  funnel <- t(matrix(1,5,5)*c(3,2,1,2,3))*c(3,2,1,2,3)
  funnel[funnel > 6] <- 6
  slope <- t(matrix(6,5,5) + c(1,2,3,4,5)) + c(2,1,0,1,2) - 2
  DEMm <- cbind(funnel, slope)
  # Check improper input handling
  expect_error(FLOWobj(DEMm))
  # Check proper input handling
  DEM <- terra::rast(DEMm, crs = "EPSG:25833")
  expect_no_error(FLOWobj(DEM))
  expect_no_message(FLOWobj(DEM))
  # Check handling of missing values
  DEMm[1,1] <- NA
  DEMm[2,6] <- NA
  DEMm[3,3] <- NA
  DEMm[3,10] <- NA
  DEM <- terra::rast(DEMm, crs = "EPSG:25833")
  expect_no_error(FLOWobj(DEM))
  # Check known outputs
  FD <- FLOWobj(DEM)
  expect_true(inherits(FD, "FLOWobj"))
  expect_true(inherits(FD$raster, "SpatRaster"))
  expect_equal(FD$source,
               c(49, 48, 47, 46, 45, 44, 43, 41, 42, 40,
                 39, 38, 37, 36, 35, 34, 33, 31, 30, 19,
                 28, 14, 11, 10, 20,  9, 18, 27,  8, 17,
                 26,  7,  6, 16, 25, 24,  5,  4, 13,  3,
                 1,  2))
  expect_equal(FD$target,
               c(38, 37, 36, 35, 44, 33, 42, 42, 32, 31,
                 28, 27, 26, 25, 24, 24, 32, 32, 20, 28,
                 27, 24, 12, 20, 21, 18, 27, 26, 17, 26,
                 25, 16, 16, 25, 24, 23,  4, 13, 23,  2,
                  2, 12))
  # Check handling of provided boundary conditions
  bc <- matrix(0,dim(DEMm)[1],dim(DEMm)[2])
  bc[is.na(DEMm)] <- 1
  bc[1,] <- 1
  bc[,1] <- 1
  bc[dim(bc)[1],] <- 1
  bc[,dim(bc)[2]] <- 1
  expect_no_error(FLOWobj(DEM, bc))
  expect_error(FLOWobj(DEM, t(bc)))
  # Check outputs of helper functions
  DEMm <- matrix(1:9, 3, 3)
  DEMm[1,1] <- NA
  DEMm[3,3] <- NA
  DEM <- terra::rast(DEMm, crs="EPSG:25833")
  FD <- FLOWobj(DEM)
  expect_equal(source_indices(FD),
               matrix(c(2,1,2,0,1,2,1,1,0,2,0,1),
                      nrow=6, ncol=2, byrow=T))
  expect_equal(target_indices(FD),
               matrix(c(2,0,1,0,1,1,1,0,0,1,1,0),
                      nrow=6, ncol=2, byrow=T))
})