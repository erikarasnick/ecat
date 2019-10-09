
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ecat

The goal of the ecat package is to easily and reproducibly assess
exposure to elemental carbon attributable to traffic (ECAT) at specific
locations in and around Cincinnati, Ohio. The package calculates
predictions of ECAT exposure from a land use regression model developed
by Dr. Patrick Ryan based on ambient air sampling in Cincinnati, OH
between 2001 and 2005. The model predictors include elevation, truck
traffic within 400 meters, and length of bus routes within 100 meters.

Additionally, these ECAT exposures can be adjusted to account for the
temporal variation associated with changing ECAT levels in the area over
time. Scaling factors are constructed using measurements of elemental
carbon (EC) recorded by the EPA in the Cincinnati area. These scaling
factors are the average EC measured over a time period of interest
(e.g., gestation, the month leading up to date of hospitilization, etc)
divided by the average EC recorded over the ECAT ambient air sampling
period (2001 to 2005). Scaling factors are then applied to ECAT
estimates from the land use model.

The figure below demonstrates the increased variability when an estimate
of ECAT at one location is used as an annual average versus when that
annual average is scaled to monthly averages.

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

## Reference

Ryan, P.H., G.K. LeMasters, P. Biswas, L. Levin, S. Hu, M. Lindsey, D.I.
Bernstein, J. Lockey, M. Villareal, G.K. Khurana Hershey, and S.A.
Grinshpun. 2007. “A Comparison of Proximity and Land Use Regression
Traffic Exposure Models and Wheezing in Infants.” Environmental Health
Perspectives 115(2): 278-284. <https://doi.org/10.1289/ehp.9480>

## Installation

ecat is hosted on GitHub; install with:

``` r
remotes::install_github('erikarasnick/ecat')
```

## Example Usage

``` r
library(ecat)
library(tidyverse)
```

### Example 1: Using `calculate_ecat()` and `calculate_scaling_factors()` and manually applying scaling factors to ECAT estimates.

``` r
d <- tibble::tribble(
  ~id,         ~lon,        ~lat,
    809089L, -84.69127387, 39.24710734,
    813233L, -84.47798287, 39.12005904,
    814881L, -84.47123583,  39.2631309,
    799697L, -84.41741798, 39.18541228,
    799698L, -84.41395064, 39.18322447
  )

my_dates <- data.frame(start_date = as.Date(c("2010-01-08", "2012-06-08", "2010-01-09", "2015-04-09", "2010-01-10")),
                       end_date = as.Date(c("2010-02-08", "2012-07-08", "2010-02-09", "2015-05-09", "2010-02-10")))

d %>% 
  mutate(ecat = calculate_ecat(. , return.LU.vars = FALSE), 
         scaling_factors = calculate_scaling_factors(my_dates), 
         scaled_ecat = ecat * scaling_factors)
#> # A tibble: 5 x 6
#>       id   lon   lat  ecat scaling_factors scaled_ecat
#>    <int> <dbl> <dbl> <dbl>           <dbl>       <dbl>
#> 1 809089 -84.7  39.2 0.302           0.989       0.299
#> 2 813233 -84.5  39.1 0.740           1.05        0.779
#> 3 814881 -84.5  39.3 0.479           0.945       0.452
#> 4 799697 -84.4  39.2 0.355           1.09        0.388
#> 5 799698 -84.4  39.2 0.339           0.945       0.320
```

### Example 2: Using the `calculate_scaled_ecat()` wrapper function to automatically apply scaling factors to ECAT estimates.

A common use case is calculating monthly exposures. For example, the
data below represents a common structure. There are 2 unique ids, and
the locations remain constant while the dates increase by one month.

``` r
d <- tibble::tribble(
  ~id,         ~lon,        ~lat,        ~date,
    809089L, -84.69127387, 39.24710734, as.Date("2010-01-08"),
    809089L, -84.69127387, 39.24710734, as.Date("2010-02-08"),
    809089L, -84.69127387, 39.24710734, as.Date("2010-03-08"),
    799697L, -84.41741798, 39.18541228, as.Date("2010-01-10"),
    799697L, -84.41741798, 39.18541228, as.Date("2012-02-10")
  )
```

We want to scale the ecat exposures between these dates, but we need
`start_date` and `end_date` columns.

``` r
d <- d %>% 
  rename(start_date = date) %>% 
  group_by(id) %>% 
  mutate(end_date = lead(start_date)) %>% 
  filter(!is.na(end_date)) %>% 
  ungroup()
```

``` r
d %>% 
  mutate(scaled_ecat = calculate_scaled_ecat(.))
#> # A tibble: 3 x 6
#>       id   lon   lat start_date end_date   scaled_ecat
#>    <int> <dbl> <dbl> <date>     <date>           <dbl>
#> 1 809089 -84.7  39.2 2010-01-08 2010-02-08       0.299
#> 2 809089 -84.7  39.2 2010-02-08 2010-03-08       0.264
#> 3 799697 -84.4  39.2 2010-01-10 2012-02-10       0.370
```
