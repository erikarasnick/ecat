get_elevation <- function(locations) {
  locations <- locations %>%
    st_transform(st_crs(elevation.raster))
  elevation <- raster::extract(elevation.raster,
                               locations)
  return(elevation)
}

get_truck_traffic <- function(locations, lines.shapefile, buffer.radius=400) {
  locations <- locations %>%
    st_transform(st_crs(lines.shapefile)) %>%
    dplyr::group_by(id, old_lat, old_lon) %>%
    tidyr::nest()
  buffer <- purrr::map(locations$data, ~st_buffer(.x, dist=buffer.radius/0.3048006096, nQuadSegs=1000))
  suppressWarnings(intersect <- purrr::map(buffer, ~st_intersection(.x, lines.shapefile)))
  intersect <- purrr::map(intersect, ~st_drop_geometry(.x))
  truck.total <- purrr::map_dbl(intersect, ~dplyr::if_else((nrow(.x)==0), 0, mean(.x$ADT_TRUCK, na.rm=TRUE)))
  return(truck.total)
}

get_line_length <- function(locations,lines.shapefile,buffer.radius=100) {
  locations <- locations %>%
    st_transform(st_crs(lines.shapefile)) %>%
    dplyr::group_by(id, old_lat, old_lon) %>%
    tidyr::nest()
  buffer <- purrr::map(locations$data, ~st_buffer(.x, dist=buffer.radius/0.3048006096, nQuadSegs=1000))
  suppressWarnings(crop.buffer <- purrr::map(buffer, ~st_intersection(.x, lines.shapefile)))
  lengths <- list()
  crop.buffer.overlap <- list()
  for (i in 1:length(crop.buffer)) {
    if (purrr::is_empty(crop.buffer[[i]]$geometry)) {
      lengths[[i]] <- 0
    } else {
      crop.buffer.overlap[[i]] <- st_intersection(crop.buffer[[i]])
      lengths[[i]] <- st_length(crop.buffer.overlap[[i]])
    }
  }
  unique.lengths <- purrr::map(lengths, ~unique(.x))
  length.total <- purrr::map_dbl(unique.lengths, ~sum(.x))
  length.total <- length.total * 0.3048006096
  return(length.total)
}
