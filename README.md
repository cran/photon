
<!-- README.md is generated from README.Rmd. Please edit that file -->

# photon

<!-- badges: start -->

[![R-CMD-check](https://github.com/JsLth/photon/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JsLth/photon/actions/workflows/R-CMD-check.yaml)
[![R-hub](https://github.com/jslth/photon/actions/workflows/rhub.yaml/badge.svg)](https://github.com/jslth/photon/actions/workflows/rhub.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/photon)](https://CRAN.R-project.org/package=photon)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Codecov test
coverage](https://codecov.io/gh/JsLth/photon/branch/main/graph/badge.svg)](https://app.codecov.io/gh/JsLth/photon?branch=main)
[![CodeFactor](https://www.codefactor.io/repository/github/jslth/photon/badge/main)](https://www.codefactor.io/repository/github/jslth/photon/overview/main)
<!-- badges: end -->

`{photon}` is a simple interface and setup manager of the
[photon](https://photon.komoot.io) OpenStreetMap geocoder. It features
unstructured, structured, and reverse geocoding. The package allows
requests to the public API but shines at setting up local instances to
enable high-performance offline geocoding.

## Installation

To install the package from CRAN:

``` r
install.packages("photon")
```

You can install the development version of photon from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("jslth/photon")
```

## Usage

When loading `{photon}`, the package assumes that you want send
geocoding requests to the public photon API. If you want to change this,
you can use the workhorse function `new_photon()`. Otherwise, you can
directly start geocoding.

``` r
library(photon)
places <- c("Paris", "Beijing", "Sao Paolo", "Kinshasa")

cities1 <- geocode(places, limit = 1, layer = "city")
cities1
#> Simple feature collection with 4 features and 12 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -46.63338 ymin: -23.55065 xmax: 110.7344 ymax: 48.8535
#> Geodetic CRS:  WGS 84
#> # A tibble: 4 × 13
#>     idx osm_type  osm_id country osm_key countrycode osm_value name  state type 
#>   <int> <chr>      <dbl> <chr>   <chr>   <chr>       <chr>     <chr> <chr> <chr>
#> 1     1 R         7.15e4 France  place   FR          city      Paris Ile-… city 
#> 2     2 N         4.52e9 China   place   CN          town      Beij… Shan… city 
#> 3     3 R         2.98e5 Brazil  place   BR          municipa… São … São … city 
#> 4     4 R         3.88e5 Democr… bounda… CD          administ… Kins… Kins… city 
#> # ℹ 3 more variables: extent <list>, county <chr>, geometry <POINT [°]>
```

Reverse geocoding means taking point geometries and returning their
addresses or place names.

``` r
cities2 <- reverse(cities1$geometry, limit = 1, layer = "city")
cities2
#> Simple feature collection with 4 features and 12 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -46.63338 ymin: -23.55065 xmax: 110.7344 ymax: 48.8535
#> Geodetic CRS:  WGS 84
#> # A tibble: 4 × 13
#>     idx osm_type  osm_id country osm_key countrycode osm_value name  state type 
#>   <int> <chr>      <dbl> <chr>   <chr>   <chr>       <chr>     <chr> <chr> <chr>
#> 1     1 R         7.15e4 France  place   FR          city      Paris Ile-… city 
#> 2     2 N         4.52e9 China   place   CN          town      Beij… Shan… city 
#> 3     3 R         2.98e5 Brazil  place   BR          municipa… São … São … city 
#> 4     4 R         3.88e5 Democr… bounda… CD          administ… Kins… Kins… city 
#> # ℹ 3 more variables: extent <list>, county <chr>, geometry <POINT [°]>
```

``` r
all.equal(cities1, cities2)
#> [1] TRUE
```

## Offline geocoding

`{photon}` is designed to facilitate offline geocoding. `new_photon()`
can install photon locally. The following code would install and start
photon covering the country of Germany in the current working directory.

``` r
photon <- new_photon(path = "./photon", country = "Germany")
photon$start()
```

## Related packages

- The [`{photon}`](https://github.com/rCarto/photon) package by Timothée
  Giraud interfaces photon but does not allow the setup of local
  instances and was abandoned a while ago.
- The [`{revgeo}`](https://CRAN.R-project.org/package=revgeo) package by
  Michael Hudecheck implements reverse geocoding using (among others)
  photon.
- The [`{tidygeocoder}`](https://jessecambon.github.io/tidygeocoder/)
  and [`{nominatimlite}`](https://dieghernan.github.io/nominatimlite/)
  packages implement general (OSM) geocoding using web APIs.
