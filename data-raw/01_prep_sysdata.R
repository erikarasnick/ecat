elevation.raster <- readRDS('elevation_raster.rds')
interstate.lines.sf <- readRDS('interstate_lines_sf.rds')
highway.lines.sf <- readRDS('highway_lines_sf.rds')
bus.route.lines.sf <- readRDS('bus_route_lines_sf.rds')
ec_ts <- readRDS("ec_2_sites.rds")

usethis::use_data(elevation.raster,
                  interstate.lines.sf,
                  highway.lines.sf,
                  bus.route.lines.sf,
                  ec_ts,
                  internal = TRUE)
