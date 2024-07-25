
# ggfields

## Overview

<!-- badges: start -->

[![R-CMD-check](https://github.com/pepijn-devries/ggfields/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pepijn-devries/ggfields/actions/workflows/R-CMD-check.yaml)
![cranlogs](https://cranlogs.r-pkg.org/badges/ggfields)
[![version](https://www.r-pkg.org/badges/version/ggfields)](https://CRAN.R-project.org/package=ggfields)
[![cran
checks](https://badges.cranchecks.info/worst/ggfields.svg)](https://cran.r-project.org/web/checks/check_results_ggfields.html)
[![csquares status
badge](https://pepijn-devries.r-universe.dev/badges/ggfields)](https://pepijn-devries.r-universe.dev/ggfields)
<!-- badges: end -->

<img src="man/figures/logo.png" align="right" alt = "logo" class = "pkgdown-hide" />

Add vector field layers to your `ggplot2::ggplot()`. Although it has
similarities with `ggplot2::geom_spoke()`, `ggfields` offers some
distinct features:

- The `radius` aesthetic is mapped to a scale and therefore can be added
  to the guides (see `vignette("radius_aes")`).
- Not only `data.frame`s are supported, but also geometric data
  (`sf::st_sf()` and `stars::st_as_stars()`).
- Corrects angles for displayed aspect ratio or coordinate system (see
  `vignette("angle_correction")`).

## Installation

> Get CRAN version

``` r
install.packages("ggfields")
```

> Get development version from r-universe

``` r
install.packages("ggfields", repos = c("https://pepijn-devries.r-universe.dev", "https://cloud.r-project.org"))
```

## Adding vector fields to a map

The example below shows how seawater current data can be added to a map:

``` r
library(ggplot2)
#> Warning: package 'ggplot2' was built under R version 4.3.3
library(ggfields)
library(ggspatial) ## For annotating with Open Street Map
```

``` r
data(seawatervelocity)

ggplot() +
  ggspatial::annotation_map_tile(
    alpha      = 0.25,
    cachedir   = tempdir()) +
  geom_fields(
    data       = seawatervelocity,
    aes(radius = as.numeric(v),
        angle  = as.numeric(angle),
        colour = as.numeric(v)),
    max_radius = grid::unit(0.7, "cm")) +
  labs(colour  = "v[m/s]",
       radius  = "v[m/s]") +
  scale_radius_binned() +
  scale_colour_viridis_b(guide = guide_bins())
```

<img src="man/figures/README-map-1.svg" width="50%" />

## Simple data.frames

Vector arrows can also be added to simple plots with `x` and `y` data:

``` r
## First generate some arbitrary data to plot:
n  <- 10
df <- data.frame(x = seq(0, 100, length.out = n), y = rnorm(n),
                 ang = seq(0, 2*pi, length.out = n))
df$len <- 2 + df$y + rnorm(n)/4

ggplot(df, aes(x = x, y = y)) +
  geom_line() +
  geom_fields(aes(angle = ang, radius = len), .angle_correction = NULL)
```

<img src="man/figures/README-dataframe-1.svg" width="70%" />
