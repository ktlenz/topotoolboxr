test_that("test-FLOWobj.R test the proper generation of a FLOWobj", {
  # Create an artificial DEM and test basic functionalities
  DEM <- t(matrix(1, 5, 5) * c(3,2,1,2,3)) * c(3,2,1,2,3)
  expect_error(FLOWobj(DEM))
  DEM <- terra::rast(DEM, crs="EPSG:25833")
  expect_no_error(FLOWobj(DEM))
  expect_no_message(FLOWobj(DEM))
  FD <- FLOWobj(DEM)
  expect_true(inherits(FD, "FLOWobj"))
  expect_true(inherits(FD$raster, "SpatRaster"))
})