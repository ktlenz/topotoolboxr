test_that("test-flow_accumulation checks proper inputs, outputs and results for synthetic data", {
    # Create reference features for synthetic DEMs
    funnel <- t(matrix(1,5,5)*c(3,2,1,2,3))*c(3,2,1,2,3)
    funnel[funnel > 6] <- 6
    slope <- t(matrix(6,5,5) + c(1,2,3,4,5)) + c(2,1,0,1,2) - 2
    DEMm <- cbind(funnel, slope)
    DEM <- terra::rast(DEMm, crs = "EPSG:25833")
    # Check improper input handling
    expect_error(flow_accumulation(DEM))
    # Check proper input handling
    FD <- FLOWobj(DEM)
    expect_no_error(flow_accumulation(FD))
    expect_no_message(flow_accumulation(FD))
    # Check handling of missing values
    DEMm[1,1] <- NA
    DEMm[2,6] <- NA
    DEMm[3,3] <- NA
    DEMm[3,10] <- NA
    DEM <- terra::rast(DEMm, crs = "EPSG:25833")
    expect_no_error(FLOWobj(DEM))
    # Check known outputs
    FD <- FLOWobj(DEM)
    FA <- flow_accumulation(FD)
    expect_true(inherits(FA, "SpatRaster")) # swap for GRIDobj when implemented
    expect_equal(
        c(terra::values(FA)),
        c(1, 1, 3, 1, 2, 1, 1, 1, 1, 1, 1, 1, 5, 3, 1, 1, 3, 2, 2, 1, 3, 4, 1, 28, 24,
        19, 13, 8, 3, 1, 1, 2, 9, 3, 1, 2, 2, 2, 2, 1, 1, 1, 3, 1, 2, 1, 1, 1, 1, 1))
})