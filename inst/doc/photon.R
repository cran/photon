## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
library(photon)
options(photon_movers = FALSE)

## ----public_api, eval=FALSE---------------------------------------------------
#  new_photon()
#  #> <photon>
#  #>   Type   : remote
#  #>   Server : https://photon.komoot.io/

## ----geocode, eval=FALSE------------------------------------------------------
#  cities1 <- geocode(c("Sanaa", "Caracas"), osm_tag = ":city")
#  cities1
#  #> Simple feature collection with 2 features and 12 fields
#  #> Geometry type: POINT
#  #> Dimension:     XY
#  #> Bounding box:  xmin: -66.9146 ymin: 10.50609 xmax: 44.20588 ymax: 15.35386
#  #> Geodetic CRS:  WGS 84
#  #> # A tibble: 2 × 13
#  #>     idx osm_type   osm_id country osm_key countrycode osm_value name  county state type  extent            geometry
#  #>   <int> <chr>       <int> <chr>   <chr>   <chr>       <chr>     <chr> <chr>  <chr> <chr> <list>         <POINT [°]>
#  #> 1     1 N          2.58e8 Yemen   place   YE          city      Sana… At Ta… Aman… dist… <lgl>  (44.20588 15.35386)
#  #> 2     2 R          1.12e7 Venezu… place   VE          city      Cara… Munic… Capi… city  <dbl>  (-66.9146 10.50609)

## ----reverse, eval=FALSE------------------------------------------------------
#  cities2 <- reverse(cities1, osm_tag = ":city")
#  cities2
#  #> Simple feature collection with 2 features and 12 fields
#  #> Geometry type: POINT
#  #> Dimension:     XY
#  #> Bounding box:  xmin: -66.9146 ymin: 10.50609 xmax: 44.20588 ymax: 15.35386
#  #> Geodetic CRS:  WGS 84
#  #> # A tibble: 2 × 13
#  #>     idx osm_type   osm_id country osm_key countrycode osm_value name  county state type  extent            geometry
#  #>   <int> <chr>       <int> <chr>   <chr>   <chr>       <chr>     <chr> <chr>  <chr> <chr> <list>         <POINT [°]>
#  #> 1     1 N          2.58e8 Yemen   place   YE          city      Sana… At Ta… Aman… dist… <lgl>  (44.20588 15.35386)
#  #> 2     2 R          1.12e7 Venezu… place   VE          city      Cara… Munic… Capi… city  <dbl>  (-66.9146 10.50609)

## ----compare, eval=FALSE------------------------------------------------------
#  all.equal(cities1, cities2)
#  #> [1] TRUE

## ----local_photon, eval=FALSE-------------------------------------------------
#  path <- file.path(tempdir(), "photon")
#  photon <- new_photon(path, country = "Samoa")
#  #> ℹ java version "22" 2024-03-19
#  #> ℹ Java(TM) SE Runtime Environment (build 22+36-2370)
#  #> ℹ Java HotSpot(TM) 64-Bit Server VM (build 22+36-2370, mixed mode, sharing)
#  #> ✔ Successfully downloaded photon 0.5.0. [7s]
#  #> ✔ Successfully downloaded search index. [590ms]
#  #> • Version: 0.5.0
#  #> • Coverage: Samoa
#  #> • Time: latest

## ----start, eval=FALSE--------------------------------------------------------
#  photon$start()
#  #> Running java -jar photon-0.5.0.jar -listen-ip 0.0.0.0 -listen-port 2322
#  #> 2024-10-25 17:04:26,912 [main] WARN  org.elasticsearch.node.Node - version [5.6.16-SNAPSHOT] is a pre-release version of Elasticsearch and is not suitable for production
#  #> ✔ Photon is now running. [11s]
#  
#  photon$proc
#  #> PROCESS 'java', running, pid 22744.

## ----is_ready, eval=FALSE-----------------------------------------------------
#  photon$is_ready()
#  #> [1] TRUE

## ----stop, eval=FALSE---------------------------------------------------------
#  photon$stop()

## ----benchmark1, eval=FALSE---------------------------------------------------
#  # offline geocoding
#  bench::mark(geocode("Apai", limit = 1), iterations = 25)$median
#  #> [1] 17.1ms

## ----benchmark2, eval=FALSE---------------------------------------------------
#  # online geocoding
#  new_photon()
#  bench::mark(geocode("Apai", limit = 1), iterations = 25)$median
#  #> [1] 1.05s

## ----purge, eval=FALSE--------------------------------------------------------
#  photon$purge()
#  #> ℹ Purging an instance kills the photon process and removes the photon directory.
#  #> Continue? (y/N/Cancel) y

