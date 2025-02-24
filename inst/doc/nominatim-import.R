## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
library(photon)

## ----eval=FALSE---------------------------------------------------------------
# opts <- cmd_options(
#   e = "PBF_URL=https://download.geofabrik.de/australia-oceania/samoa-latest.osm.pbf",
#   e = "NOMINATIM_PASSWORD=MNdtC2*pP#aMbe",
#   e = "FREEZE=true",
#   p = "8080:8080",
#   p = "5432:5432",
#   name = "nominatim",
#   "mediagis/nominatim:4.4",
#   use_double_hyphens = TRUE
# )
# 
# nominatim <- process$new("docker", c("run", opts))

## ----eval=FALSE---------------------------------------------------------------
# library(RPostgres)
# db <- dbConnect(Postgres(), password = "MNdtC2*pP#aMbe", user = "nominatim")
# dbGetInfo(db)
# #> $dbname
# #> [1] "nominatim"
# #>
# #> $host
# #> [1] "localhost"
# #>
# #> $port
# #> [1] "5432"
# #>
# #> $username
# #> [1] "nominatim"
# #>
# #> $protocol.version
# #> [1] 3
# #>
# #> $server.version
# #> [1] 140013
# #>
# #> $db.version
# #> [1] 140013
# #>
# #> $pid
# #> [1] 604
# 
# dbDisconnect(db)

## ----eval=FALSE---------------------------------------------------------------
# dir <- file.path(tempdir(), "photon")
# photon <- new_photon(dir, overwrite = TRUE)
# #> ℹ java version "22" 2024-03-19
# #> ℹ Java(TM) SE Runtime Environment (build 22+36-2370)
# #> ℹ Java HotSpot(TM) 64-Bit Server VM (build 22+36-2370, mixed mode, sharing)
# #> ✔ Successfully downloaded photon 0.6.2. [8.2s]
# #> ℹ No search index downloaded! Download one or import from a Nominatim database.
# #> • Version: 0.6.2
# 
# photon$import(host = "localhost", password = "MNdtC2*pP#aMbe")
# #> 2024-10-24 23:07:35,904 [main] WARN  org.elasticsearch.node.Node - version [5.6.16-SNAPSHOT] is a pre-release version of Elasticsearch and is not suitable for production
# #> 2024-10-24 23:07:43,326 [main] INFO  de.komoot.photon.elasticsearch.Server - Started elastic search node
# #> 2024-10-24 23:07:43,326 [main] INFO  de.komoot.photon.App - Make sure that the ES cluster is ready, this might take some time.
# #> 2024-10-24 23:07:43,905 [main] INFO  de.komoot.photon.App - ES cluster is now ready.
# #> 2024-10-24 23:07:45,299 [main] INFO  de.komoot.photon.App - Starting import from nominatim to photon with languages: en,fr,de,it
# #> 2024-10-24 23:07:45,300 [main] INFO  de.komoot.photon.nominatim.NominatimConnector - Start importing documents from nominatim (global)
# #> 2024-10-24 23:07:59,080 [main] INFO  de.komoot.photon.nominatim.ImportThread - Finished import of 2085 photon documents.
# #> 2024-10-24 23:07:59,080 [main] INFO  de.komoot.photon.App - Imported data from nominatim to photon with languages: en,fr,de,it

## ----eval=FALSE---------------------------------------------------------------
# photon$start()
# #> 2024-10-24 23:26:46,360 [main] WARN  org.elasticsearch.node.Node - version [5.6.16-SNAPSHOT] is a pre-release version of Elasticsearch and is not suitable for production
# #> ✔ Photon is now running. [11.1s]

## ----eval=FALSE---------------------------------------------------------------
# geocode("Apia", limit = 3)
# #> Simple feature collection with 3 features and 13 fields
# #> Geometry type: POINT
# #> Dimension:     XY
# #> Bounding box:  xmin: -171.7631 ymin: -13.83613 xmax: -171.7512 ymax: -13.82611
# #> Geodetic CRS:  WGS 84
# #> # A tibble: 3 × 14
# #>     idx osm_type     osm_id country osm_key city        street     countrycode osm_value name  state type  extent
# #>   <int> <chr>         <int> <chr>   <chr>   <chr>       <chr>      <chr>       <chr>     <chr> <chr> <chr> <list>
# #> 1     1 W        1322127938 Samoa   place   NA          NA         WS          city      Apia  Tuam… city  <dbl>
# #> 2     1 W         723300892 Samoa   landuse Matautu Tai NA         WS          harbour   Apia… Tuam… other <dbl>
# #> 3     1 W         666117780 Samoa   tourism Levili      Levili St… WS          attracti… Apia… Tuam… house <dbl>
# #> # ℹ 1 more variable: geometry <POINT [°]>

## ----eval=FALSE---------------------------------------------------------------
# # set opensearch = TRUE to use OpenSearch photon
# photon <- new_photon(dir, opensearch = TRUE, quiet = TRUE)
# 
# # set structured = TRUE to enable structured geocoding
# photon$import(host = "localhost", password = "MNdtC2*pP#aMbe", structured = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# photon$start()
# has_structured_support()
# #> [1] TRUE

## ----eval=FALSE---------------------------------------------------------------
# place_data <- data.frame(
#   housenumber = c(NA, "77C", NA),
#   street = c("Falealilli Cross Island Road", "Main Beach Road", "Le Mafa Pass Road"),
#   state = c("Tuamasaga", "Tuamasaga", "Atua")
# )
# 
# structured(place_data)
# #> Simple feature collection with 3 features and 14 fields
# #> Geometry type: POINT
# #> Dimension:     XY
# #> Bounding box:  xmin: -171.7759 ymin: -14.04544 xmax: -171.451 ymax: -13.8338
# #> Geodetic CRS:  WGS 84
# #> # A tibble: 3 × 15
# #>     idx osm_type    osm_id country osm_key  city      countrycode osm_value name    state type  extent housenumber street
# #>   <int> <chr>        <int> <chr>   <chr>    <chr>     <chr>       <chr>     <chr>   <chr> <chr> <list> <chr>       <chr>
# #> 1     1 W        319147189 Samoa   highway  Siumu Uta WS          primary   Faleal… Tuam… stre… <dbl>  NA          NA
# #> 2     2 W        569855981 Samoa   building Apia      WS          yes       NA      Tuam… house <dbl>  77C         Main …
# #> 3     3 W         40681149 Samoa   highway  Lalomanu  WS          primary   Main S… Ātua  stre… <dbl>  NA          NA
# #> # ℹ 1 more variable: geometry <POINT [°]>

