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
#' @return ECAT will be missing if point is outside the 7-county area (OH: Hamilton, Butler, Warren, Clermont; KY: Campbell, Kenton, Boone)
#'
#' @references Ryan, P.H., G.K. LeMasters, P. Biswas, L. Levin, S. Hu, M. Lindsey, D.I. Bernstein, J. Lockey, M. Villareal,
#' G.K. Khurana Hershey, and S.A. Grinshpun. 2007. "A Comparison of Proximity and Land Use Regression Traffic Exposure Models
#' and Wheezing in Infants." Environmental Health Perspectives 115(2): 278-284.  \url{https://doi.org/10.1289/ehp.9480}
#'
#' @examples
#' my_data <- data.frame(id = 1:3,
#'     lat = c(39.19674, 39.12731,	39.28765),
#'     lon = c(-84.58260, -84.52700, -84.51017))
#'
#' ecat_est <- calculate_ecat(my_data, return.LU.vars = FALSE)
#' ecat_est <- calculate_ecat(my_data, return.LU.vars = TRUE)
#' @export

calculate_ecat <- function(locations, return.LU.vars=FALSE) {
  if(!"id" %in% colnames(locations)) {stop("locations dataframe must have a column called 'id'")}
  if(!"lat" %in% colnames(locations)) {stop("locations dataframe must have a column called 'lat'")}
  if(!"lon" %in% colnames(locations)) {stop("locations dataframe must have a column called 'lon'")}
  if("ecat" %in% colnames(locations)) {stop("locations dataframe must not already have a column called 'ecat'")}

  orig <- locations

  missing <- locations[is.na(locations$lat) | is.na(locations$lon) ,]

  if (nrow(missing) > 0) {warning(paste0(missing$n,
                                     " observations were missing lat/lon coordinates and will be excluded."))}

  locations <- locations[!is.na(locations$lat) & !is.na(locations$lon) ,]
  locations <- locations[!(duplicated(locations$lat) & duplicated(locations$lon)) ,]
  locations$old_lat <- locations$lat
  locations$old_lon <- locations$lon
  locations <- sf::st_as_sf(locations, coords=c('old_lon', 'old_lat'), crs=4326)

  locations$elevation <- get_elevation(locations)
  locations$highway.truck.traffic <- get_truck_traffic(locations, lines.shapefile=highway.lines.sf, buffer.radius=400)
  locations$interstate.truck.traffic <- get_truck_traffic(locations, lines.shapefile=interstate.lines.sf, buffer.radius=400)
  locations$bus.route.length <- get_line_length(locations, lines.shapefile=bus.route.lines.sf, buffer.radius=100)
  locations$truck400 <- locations$interstate.truck.traffic + locations$highway.truck.traffic
  locations$elevatnew <- locations$elevation / 1000
  locations$br_km <- locations$bus.route.length / 1000
  locations$br_km <- ifelse(locations$br_km == 0, 0.01, locations$br_km)
  locations$logbr <- log10(locations$br_km)
  locations$truck400s <- locations$truck400 / 1000
  locations$truck400s <- ifelse(locations$truck400s == 0, 0.01, locations$truck400s)
  locations$logtruck <- log10(locations$truck400s)
  locations$log_ecat <- .34408 - (.85107 * locations$elevatnew) + (.04448 * locations$logbr) + (.03968 * locations$logtruck)
  locations$ecat <- 10^locations$log_ecat

  out <- sf::st_drop_geometry(locations)
  out <- locations[c("id", "lat", "lon", "elevation", "highway.truck.traffic",
                     "interstate.truck.traffic", "bus.route.length", "ecat")]

  out <- dplyr::left_join(orig, out, by = c('lat', 'lon'))

  if (return.LU.vars == FALSE) {
    out <- out$ecat
  }

  return(out)
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
#' @param dates A data.frame with 2 columns called 'start_date' and 'end_date' at minimum.
#'     Both columns must be of class \code{Date}. See \code{\link{as.Date}} for help converting
#'     a character vector to a Date vector.
#'
#' @return A numeric vector of temporal scaling factors.
#'
#' @details EPA data in this package is available from November 9, 2001
#'     through May 30, 2019. Scaling factors that attempt to average over
#'     EC measured on dates outside this range will not be calculated. In addition,
#'     it is important to be mindful of the frequency of EC measurements recorded by the EPA.
#'     Note that EC was measured every 6 days through the end of 2010, and every 3 days starting in 2011.
#'     If there are less than 4 measurements of EC between the start_date and end_date, the scaling
#'     factor will not be calculated and NA will be returned.
#'
#' @examples
#' my_dates <- data.frame(start_date = c("2010-01-08", "2012-06-08", "2010-01-09",
#'                                       "2015-04-09", "2010-01-10"),
#'                        end_date = c("2010-02-08", "2012-07-08", "2010-02-09",
#'                                     "2015-05-09", "2010-02-10"))
#'
#' \dontrun{
#' class(my_dates$start_date)  # character vector
#' scaling1m <- calculate_scaling_factors(my_dates)
#' }
#'
#' my_dates$start_date <- as.Date(my_dates$start_date)
#' my_dates$end_date <- as.Date(my_dates$end_date)
#' scaling1m <- calculate_scaling_factors(my_dates)
#' @export

calculate_scaling_factors <- function(dates) {
  if(!"start_date" %in% colnames(dates)) {stop("The dates dataframe must have a column called 'start_date'.")}
  if(!"end_date" %in% colnames(dates)) {stop("The dates dataframe must have a column called 'end_date'.")}

  if (class(dates$start_date) != "Date" | class(dates$end_date) != "Date") {
    stop("Start and end dates are not of class 'Date'. See ?as.Date() to convert.")
  }

  early_dates <- which(dates$start_date < min(ec_ts$date))
  late_dates <- which(dates$end_date > max(ec_ts$date))
  if (length(early_dates) > 0) {
    warning(paste0(length(early_dates), " scaling factor(s) were not computed due to lack of EPA data before ",
                   min(ec_ts$date)))
  }

  if (length(late_dates) > 0) {
    warning(paste0(length(late_dates), " scaling factor(s) were not computed due to lack of EPA data after ",
                   max(ec_ts$date)))
  }

  # precalculated average ec through end of 2005
  denom <- 0.6715289

  dates$monthly_mean <- purrr::map2_dbl(dates$start_date, dates$end_date,
                                        ~{ecats <- dplyr::filter(ec_ts, date >= .x, date <= .y)$EC
                                        # if (length(ecats) < 4) warning("Less than 4 measurements recorded between start and end dates. Returning NA. Consider increasing date range.",
                                        #                                call. = FALSE)
                                        ifelse(length(ecats) < 4, NA, mean(ecats, na.rm=TRUE))
                                        })

  dates$scaling_factor <- ifelse(dates$start_date < min(ec_ts$date) | dates$end_date > max(ec_ts$date),
                                   NA, dates$monthly_mean/denom)

  dates <- dates[!names(dates) == 'monthly_mean']

  return(dates$scaling_factor)
}

#' Calculate temporally scaled ECAT exposure estimates at specific locations.
#'
#' \code{add_scaled_ecat()} is a wrapper function that estimates ECAT exposures
#'     at provided locations by calling \code{calculate_ecat()}, then temporally scales those
#'     estimates using scaling factors computed by calling \code{calculate_scaling_factors()}.
#'     This function is particularly useful for calculating exposures at the same locations on
#'     different dates.
#'
#' @param locations Data.frame with columns 'id', 'lat','lon', 'start_date', and 'end_date' at minimum.
#'
#' @return A numeric vector of ECAT estimates (ug/m3).
#'
#' @references Ryan, P.H., G.K. LeMasters, P. Biswas, L. Levin, S. Hu, M. Lindsey, D.I. Bernstein, J. Lockey, M. Villareal,
#' G.K. Khurana Hershey, and S.A. Grinshpun. 2007. "A Comparison of Proximity and Land Use Regression Traffic Exposure Models
#' and Wheezing in Infants." Environmental Health Perspectives 115(2): 278-284.  \url{https://doi.org/10.1289/ehp.9480}
#'
#' @examples
#' my_data <- data.frame(id = rep(1,3),
#'     lat = c(39.19674, 39.19674,	39.19674),
#'     lon = c(-84.58260, -84.58260, -84.58260),
#'     start_date = as.Date(c("2010-01-08", "2012-06-08", "2015-04-09")),
#'     end_date = as.Date(c("2010-02-08", "2012-07-08", "2015-05-09")))
#'
#' ecat_scaled <- add_scaled_ecat(my_data)
#' @export

add_scaled_ecat <- function(locations) {
  ecat_unadj <- calculate_ecat(locations, return.LU.vars = FALSE)

  scaling_factors <- calculate_scaling_factors(dates = locations)

  ecat_adj <- ecat_unadj * scaling_factors

  return(ecat_adj)
}



