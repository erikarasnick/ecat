elevation.raster <- readRDS('./data-raw/elevation_raster.rds')
interstate.lines.sf <- readRDS('./data-raw/interstate_lines_sf.rds')
highway.lines.sf <- readRDS('./data-raw/highway_lines_sf.rds')
bus.route.lines.sf <- readRDS('./data-raw/bus_route_lines_sf.rds')
ec_ts <- readRDS("./data-raw/ec_ts.rds")

usethis::use_data(elevation.raster,
                  interstate.lines.sf,
                  highway.lines.sf,
                  bus.route.lines.sf,
                  ec_ts,
                  internal = TRUE,
                  overwrite = TRUE)
