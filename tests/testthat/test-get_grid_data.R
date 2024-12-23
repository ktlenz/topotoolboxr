test_that("get_grid_data loads the elev.tif from terra and produces our desired list of z, cellsize and dims values", {
  load_rast<-terra::rast(system.file("ex/elev.tif",package="terra"))
  load_rast<-terra::project(load_rast,"epsg:32632",res=90.0)
  expect_no_message(get_grid_data(load_rast))
  expect_no_error(get_grid_data(load_rast))
  expect_visible(get_grid_data(load_rast))
  expect_named(get_grid_data(load_rast),c("z","cellsize","dims"))
})
