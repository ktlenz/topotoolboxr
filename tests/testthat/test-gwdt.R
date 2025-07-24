test_that("test-gwdt.R creates reference DEM and computes known distances.", {
  DEM <- matrix(1,nrow=7,ncol=5)*1:7
  DEM[2:6,c(2, 4)] = 1
  DEM[7,3] = NA
  DEM <- terra::rast(DEM,crs="EPSG:25833")
  expect_equal(round(as.vector(unique(terra::values(gwdt(DEM)))),1), c(0, 1.0, 1.1))
  expect_no_message(gwdt(DEM))
  expect_no_error(gwdt(DEM))
})