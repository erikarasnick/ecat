#' Calculate ECAT exposure estimates at specific locations.
#'
#' \code{calculate_ecat()} uses a land use regression model developed by Dr. Patrick Ryan
#'     based on ambient air sampling in Cincinnati, OH between 2001 and 2005 to estimate
#'     exposure to elemental carbon attributable to traffic (ECAT) at point locations in
#'     the area specified by latitude and longitude. The model predictors include elevation,
#'     truck traffic within 400 meters, and length of bus routes within 100 meters. Returned
#'     ECAT values are in micrograms per cubic meter.
#'
#' @param locations Data.frame with columns 'id', 'lat', and 'lon' at minimum.
#' @param return.LU.vars When \code{return.LU.vars = TRUE}, the land use predictors used
#'     to generate the ECAT values are also returned.
#'
#' @return If \code{return.LU.vars = FALSE}, a numeric vector of ECAT estimates (ug/m3)
#'     is returned. If \code{return.LU.vars = TRUE}, the \code{locations} data.frame with
#'     additional columns for ECAT values and the land use predictors used
#'     to generate the ECAT values is returned.
#'
#' @examples
#' my_data <- data.frame(id = 1:3,
#'     lat = c(39.19674, 39.12731,	39.28765),
#'     lon = c(-84.58260, -84.52700, -84.51017))
#'
#' ecat_est <- calculate_ecat(my_data, return.LU.vars = FALSE)
#' ecat_est <- calculate_ecat(my_data, return.LU.vars = TRUE)
#'
#' @
#'
#' @export

calculate_ecat <- function(locations, return.LU.vars=FALSE) {
  if(!"id" %in% colnames(locations)) {stop("locations dataframe must have a column called 'id'")}
  if(!"lat" %in% colnames(locations)) {stop("locations dataframe must have a column called 'lat'")}
  if(!"lon" %in% colnames(locations)) {stop("locations dataframe must have a column called 'lon'")}

  missing <- locations %>%
    dplyr::filter(is.na(lat), is.na(lon)) %>%
    dplyr::summarize(n=dplyr::n())

  if (missing$n > 0) {warning(paste0(missing$n, " observations were missing lat/lon coordinates and will be excluded."))}

  locations <- locations %>%
    dplyr::filter(!is.na(lat), !is.na(lon)) %>%
    dplyr::mutate(old_lat = lat, old_lon = lon) %>%
    st_as_sf(coords=c('lon', 'lat'), crs=4326) %>%
    dplyr::mutate(elevation = get_elevation(.),
           highway.truck.traffic = get_truck_traffic(.,lines.shapefile=highway.lines.sf,buffer.radius=400),
           interstate.truck.traffic = get_truck_traffic(.,lines.shapefile=interstate.lines.sf, buffer.radius=400),
           bus.route.length = get_line_length(., lines.shapefile=bus.route.lines.sf, buffer.radius=100),
           truck400 = interstate.truck.traffic + highway.truck.traffic,
           elevatnew = elevation / 1000,
           br_km = bus.route.length / 1000,
           br_km = ifelse(br_km == 0, 0.01, br_km),
           logbr = log10(br_km),
           truck400s = truck400 / 1000,
           truck400s = ifelse(truck400s == 0, 0.01, truck400s),
           logtruck = log10(truck400s),
           log_ecat = .34408 - (.85107 * elevatnew) + (.04448 * logbr) + (.03968 * logtruck),
           ecat = 10^log_ecat)

  out <- locations %>%
    st_drop_geometry() %>%
    dplyr::select(id, lat = old_lat, lon = old_lon,
           elevation, highway.truck.traffic,
           interstate.truck.traffic, bus.route.length, ecat)

  if (return.LU.vars == FALSE) {
    out <- out$ecat
  }

  return(out)
  warning("The `raster` package has been attached to the global environment, masking dplyr::select()")
}

#' Calculate temporal scaling factors based on EPA measurements of EC.
#'
#' \code{calculate_scaling_factors()} constructs temporal scaling factors based on measurements
#'     of elemental carbon (EC) recorded by the EPA in the Cincinnati area. These scaling factors are
#'     the average EC measured over the provided number of \code{days_prior} up to the provided
#'     \code{dates}, divided by the average EC recorded over the ECAT ambient air sampling period (2001 to 2005).
#'     Scaling factors can be multiplied by ECAT estimates from \code{calculate_ecat()} to adjust for
#'     temporal variability in ECAT in the Cincinnati area over time.
#'
#' @param dates Vector of class \code{Date}. See \code{\link{as.Date}} for help converting
#'     a character vector to a Date vector.
#' @param days_prior The number of days prior to each date to average over. Must
#'    be an integer.
#'
#' @return A numeric vector of temporal scaling factors the same length
#'     as \code{dates}.
#'
#' @details EPA data in this package is available from November 9, 2001
#'     through November 28, 2018. Scaling factors that attempt to average over
#'     EC measured on dates outside this range will not be calculated. In addition,
#'     it is important to be mindful of the frequency of EC measurements recorded by the EPA
#'     when choosing a value for \code{days_prior}. Note that EC was measured every 6 days
#'     through the end of 2010, and every 3 days starting in 2011.
#'
#' @examples
#' my_dates <- c("2010-01-08", "2012-06-08", "2010-01-09", "2015-04-09", "2010-01-10")
#'
#' \dontrun{
#' class(my_dates)  # character vector
#' scaling1m <- calculate_scaling_factors(my_dates, days_prior = 30)
#' }
#'
#' my_dates <- as.Date(my_dates)
#' class(my_dates)  # Date vector
#' scaling1m <- calculate_scaling_factors(my_dates, days_prior = 30)
#' @export

calculate_scaling_factors <- function(dates, days_prior) {
  if (class(dates) != "Date") {
    stop("Vector is not of class 'Date'. See ?as.Date() to convert.")
  }

  early_dates <- which(dates - lubridate::days(days_prior) < min(ec_ts$date))
  if (length(early_dates) > 0) {
    warning(paste0("Scaling factor(s) for ", length(early_dates),
                   " of your dates were not computed due to lack of EPA data before ",
                   min(ec_ts$date)))
  }

  # precalculated average ec through end of 2005
  denom <- 0.6715289

  dates <- tibble::tibble(date = dates) %>%
    dplyr::mutate(date_prior = date - lubridate::days(days_prior)) %>%
    dplyr::mutate(monthly_mean = purrr::map2_dbl(date, date_prior,
                                   ~mean(dplyr::filter(ec_ts, date >= .y, date <= .x)$EC, na.rm=TRUE)),
           scaling_factor = monthly_mean/denom)

  return(dates$scaling_factor)
}

#' Calculate temporally scaled ECAT exposure estimates at specific locations.
#'
#' \code{calculate_scaled_ecat()} is a wrapper function that estimates ECAT exposures
#'     at provided locations by calling \code{calculate_ecat()}, then temporally scales those
#'     estimates using scaling factors computed by calling \code{calculate_scaling_factors()}.
#'     This function is particularly useful for calculating exposures at the same locations on
#'     different dates.
#'
#' @param locations Data.frame with columns 'id', 'lat','lon', and 'date' at minimum.
#' @param days_prior The number of days prior to each date to average over. Must
#'    be an integer.
#'
#' @return a numeric vector of ECAT estimates (ug/m3).
#'
#' @examples
#' my_data <- data.frame(id = rep(1,3),
#'     lat = c(39.19674, 39.19674,	39.19674),
#'     lon = c(-84.58260, -84.58260, -84.58260),
#'     date = c("2010-01-08", "2012-06-08", "2015-04-09"))
#'
#' ecat_scaled <- calculate_scaled_ecat(my_data, months_prior = 1)
#' @export

calculate_scaled_ecat <- function(locations, days_prior) {
  unique_locations <- locations %>%
    dplyr::filter(!is.na(lat), !is.na(lon),
           !(duplicated(lat) & duplicated(lon)))

  ecat_unadj <- calculate_ecat(unique_locations, return.LU.vars = TRUE)
  ecat_unadj <- dplyr::left_join(locations, ecat_unadj, by=c("id", "lat", "lon"))
  ecat_unadj <- ecat_unadj$ecat

  scaling_factors <- calculate_scaling_factors(dates = locations$date,
                                               days_prior = days_prior)

  ecat_adj <- ecat_unadj * scaling_factors
  return(ecat_adj)
  warning("The `raster` package has been attached to the global environment, masking dplyr::select()")
}
