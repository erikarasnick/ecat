#' @import sf
#' @import dplyr
#' @importFrom purrr map
#' @importFrom purrr map_dbl

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
    group_by(id, old_lat, old_lon) %>%
    tidyr::nest()
  buffer <- map(locations$data, ~st_buffer(.x, dist=buffer.radius/0.3048006096, nQuadSegs=1000))
  suppressWarnings(intersect <- map(buffer, ~st_intersection(.x, lines.shapefile)))
  intersect <- map(intersect, ~st_drop_geometry(.x))
  truck.total <- map_dbl(intersect, ~if_else((nrow(.x)==0), 0, mean(.x$ADT_TRUCK, na.rm=TRUE)))
  return(truck.total)
}

get_line_length <- function(locations,lines.shapefile,buffer.radius=100) {
  locations <- locations %>%
    st_transform(st_crs(lines.shapefile)) %>%
    group_by(id, old_lat, old_lon) %>%
    tidyr::nest()
  buffer <- map(locations$data, ~st_buffer(.x, dist=buffer.radius/0.3048006096, nQuadSegs=1000))
  suppressWarnings(crop.buffer <- map(buffer, ~st_intersection(.x, lines.shapefile)))
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
  unique.lengths <- map(lengths, ~unique(.x))
  length.total <- map_dbl(unique.lengths, ~sum(.x))
  length.total <- length.total * 0.3048006096
  return(length.total)
}
