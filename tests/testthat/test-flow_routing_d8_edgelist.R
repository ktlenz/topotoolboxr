test_that("test-flow_routing_d8_edgelist.R creates a reference DEM and tests known sources, targets and missing data handling.", {
  # Create reference features for synthetic DEMs
  funnel <- t(matrix(1,5,5)*c(3,2,1,2,3))*c(3,2,1,2,3)
  funnel[funnel > 6] <- 6
  slope <- t(matrix(6,5,5) + c(1,2,3,4,5)) + c(2,1,0,1,2) - 2
  DEMm <- cbind(funnel, slope)
  # Check improper input handling
  expect_error(flow_routing_d8_edgelist(DEMm))
  # Check proper input handling
  DEM <- terra::rast(DEMm,crs="EPSG:25833")
  expect_no_error(flow_routing_d8_edgelist(DEM))
  expect_no_message(flow_routing_d8_edgelist(DEM))
  expect_equal(flow_routing_d8_edgelist(DEM)$source,
               FLOWobj(DEM)$source)
  expect_equal(flow_routing_d8_edgelist(DEM)$target,
               FLOWobj(DEM)$target)
  # Checking missing data handling
  DEMm[1,1] <- NA
  DEMm[2,6] <- NA
  DEMm[3,3] <- NA
  DEMm[3,10] <- NA
  DEM <- terra::rast(DEMm, crs = "EPSG:25833")
  expect_no_error(flow_routing_d8_edgelist(DEM))
  expect_no_message(flow_routing_d8_edgelist(DEM))
  expect_equal(flow_routing_d8_edgelist(DEM)$source,
               FLOWobj(DEM)$source)
  expect_equal(flow_routing_d8_edgelist(DEM)$target,
               FLOWobj(DEM)$target)
})