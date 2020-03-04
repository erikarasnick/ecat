interstate.lines.sf <- readRDS('./data-raw/interstate_lines_sf.rds')
highway.lines.sf <- readRDS('./data-raw/highway_lines_sf.rds')
bus.route.lines.sf <- readRDS('./data-raw/bus_route_lines_sf.rds')
ec_ts <- readRDS("./data-raw/ec_ts.rds")

# elevation.raster <- readRDS('./data-raw/elevation_raster.rds')
# grid <- sf::st_read('./data-raw/prediction_grid_1km/prediction_grid_1km.shp')
#e_crop <- raster::crop(elevation.raster, raster::extent(grid)) %>%
#  raster::mask(grid)
# saveRDS(e_crop, "./data-raw/elevation_raster_trimmed.rds")

elevation.raster <- readRDS('./data-raw/elevation_raster_trimmed.rds')

usethis::use_data(elevation.raster,
                  interstate.lines.sf,
                  highway.lines.sf,
                  bus.route.lines.sf,
                  ec_ts,
                  internal = TRUE,
                  overwrite = TRUE)
