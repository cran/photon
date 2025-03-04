#' Unstructured geocoding
#' @description
#' Geocode arbitrary text strings. Unstructured geocoding is more flexible but
#' generally less accurate than \link[=structured]{structured geocoding}.
#'
#' @param texts Character vector of a texts to geocode.
#' @param limit Number of results to return. A maximum of 50 results can be
#' returned for a single search term. Defaults to 1. When more than a single
#' text is provided but limit is greater than 1, the results can be uniquely
#' linked to the input texts using the \code{idx} column in the output.
#' @param lang Language of the results. If \code{"default"}, returns the results
#' in local language.
#' @param bbox Any object that can be parsed by \code{\link[sf]{st_bbox}}.
#' Results must lie within this bbox.
#' @param osm_tag Character string giving an
#' \href{https://wiki.openstreetmap.org/wiki/Tags}{OSM tag} to filter the
#' results by. See details.
#' @param layer Character string giving a layer to filter the results by.
#' Can be one of \code{"house"}, \code{"street"}, \code{"locality"},
#' \code{"district"}, \code{"city"}, \code{"county"}, \code{"state"},
#' \code{"country"}, or \code{"other"}.
#' @param locbias Numeric vector of length 2 or any object that can be coerced
#' to a length-2 numeric vector (e.g. a list or \code{sfg} object). Specifies a
#' location bias for geocoding in the format \code{c(lon, lat)}. Geocoding
#' results are biased towards this point. The radius of the bias is controlled
#' through \code{zoom} and the weight of place prominence through
#' \code{location_bias_scale}.
#' @param locbias_scale Numeric vector specifying the importance of prominence
#' in \code{locbias}. A higher prominence scale gives more weight to important
#' places. Possible values range from 0 to 1. Defaults to 0.2.
#' @param zoom Numeric specifying the radius for which the \code{locbias} is
#' effective. Corresponds to the zoom level in OpenStreetMap. The exact relation
#' to \code{locbias} is \eqn{0.25\text{ km} \cdot 2^{(18 - \text{zoom})}}.
#' Defaults to 16.
#' @param latinize If \code{TRUE} sanitizes search terms in \code{texts} by
#' converting their encoding to \code{"latin1"} using \code{\link{latinize}}.
#' This can be helpful if the search terms contain certain symbols (e.g. fancy
#' quotes) that photon cannot handle properly. Defaults to \code{TRUE} as
#' \code{latinize} is very conservative and should usually not cause any
#' problems.
#' @param progress If \code{TRUE}, shows a progress bar for longer queries.
#'
#' @returns An sf dataframe or tibble containing the following columns:
#'
#' \itemize{
#'  \item{\code{idx}: Internal ID specifying the index of the \code{texts}
#'  parameter.}
#'  \item{\code{osm_type}: Type of OSM element, one of N (node), W (way),
#'  R (relation), or P (polygon).}
#'  \item{\code{osm_id}: OpenStreetMap ID of the matched element.}
#'  \item{\code{country}: Country of the matched place.}
#'  \item{\code{city}: City of the matched place.}
#'  \item{\code{osm_key}: OpenStreetMap key.}
#'  \item{\code{countrycode}: ISO2 country code.}
#'  \item{\code{housenumber}: House number, if applicable.}
#'  \item{\code{postcode}: Post code, if applicable.}
#'  \item{\code{locality}: Locality, if applicable.}
#'  \item{\code{street}: Street, if applicable.}
#'  \item{\code{district}: District name, if applicable.}
#'  \item{\code{osm_value}: OpenStreetMap tag value.}
#'  \item{\code{name}: Place name.}
#'  \item{\code{type}: Layer type as described for the \code{layer} parameter.}
#'  \item{\code{extent}: Boundary box of the match.}
#' }
#'
#' @details
#' Filtering by OpenStreetMap tags follows a distinct syntax explained on
#' \url{https://github.com/komoot/photon}. In particular:
#'
#' \itemize{
#'  \item{Include places with tag: \code{key:value}}
#'  \item{Exclude places with tag: \code{!key:value}}
#'  \item{Include places with tag key: \code{key}}
#'  \item{Include places with tag value: \code{:value}}
#'  \item{Exclude places with tag key: \code{!key}}
#'  \item{Exclude places with tag value: \code{:!value}}
#' }
#'
#' @export
#'
#' @examples
#' \donttest{# an instance must be mounted first
#' photon <- new_photon()
#'
#' # geocode a city
#' geocode("Berlin")
#'
#' # return more results
#' geocode("Berlin", limit = 10)
#'
#' # return the results in german
#' geocode("Berlin", limit = 10, lang = "de")
#'
#' # limit to cities
#' geocode("Berlin", layer = "city")
#'
#' # limit to European cities
#' geocode("Berlin", bbox = c(xmin = -71.18, ymin = 44.46, xmax = 13.39, ymax = 52.52))
#'
#' # search for museums in berlin
#' geocode("Berlin", osm_tag = "tourism:museum")
#'
#' # search for touristic attractions in berlin
#' geocode("Berlin", osm_tag = "tourism")
#'
#' # search for anything but tourism
#' geocode("Berlin", osm_tag = "!tourism")
#'
#' # use location biases to match Berlin, IL instead of Berlin, DE
#' geocode("Berlin", locbias = c(-100, 40), locbias_scale = 0.1, zoom = 7, osm_tag = "place")
#'
#' # latinization can help normalize search terms
#' geocode("Luatuanu\u2019u", latinize = FALSE) # fails
#' geocode("Luatuanu\u2019u", latinize = TRUE)  # works}
geocode <- function(texts,
                    limit = 1,
                    lang = "en",
                    bbox = NULL,
                    osm_tag = NULL,
                    layer = NULL,
                    locbias = NULL,
                    locbias_scale = NULL,
                    zoom = NULL,
                    latinize = TRUE,
                    progress = interactive()) {
  assert_vector(texts, "character")
  assert_vector(limit, "numeric", size = 1, null = TRUE)
  assert_vector(lang, "character", size = 1)
  assert_vector(osm_tag, "character", null = TRUE)
  assert_vector(layer, "character", null = TRUE)
  assert_vector(locbias_scale, "numeric", size = 1, null = TRUE)
  assert_vector(zoom, "numeric", size = 1, null = TRUE)
  assert_range(locbias_scale, min = 0, max = 1, than = FALSE)
  assert_flag(progress)
  assert_flag(latinize)
  progress <- progress && globally_enabled("photon_movers")

  locbias <- format_locbias(locbias)
  bbox <- format_bbox(bbox)
  query <- unique(texts)
  gids <- group_id(texts, query)
  if (latinize) query <- latinize(query)
  options <- list(env = environment())

  if (progress) {
    cli::cli_progress_bar(name = "Geocoding", total = length(query))
  }

  iter <- list(q = query, i = seq_len(length(query)))
  geocoded <- .mapply(iter, MoreArgs = options, FUN = geocode_impl)
  as_sf(rbind_list(fit_original(geocoded[gids])))
}


geocode_impl <- function(q, i, env, progress) {
  if (env$progress) cli::cli_progress_update(.envir = env)
  query_photon(
    endpoint = "api",
    q = q,
    limit = env$limit,
    lang = env$lang,
    bbox = env$bbox,
    osm_tag = env$osm_tag,
    layer = env$layer,
    lon = env$locbias$lon,
    lat = env$locbias$lat,
    location_bias_scale = env$locbias_scale,
    zoom = env$zoom
  )
}


format_bbox <- function(bbox) {
  if (!is.null(bbox)) {
    bbox <- sf::st_bbox(bbox)
    bbox <- paste(bbox, collapse = ",")
  }
  bbox
}


format_locbias <- function(locbias) {
  if (!is.null(locbias)) {
    locbias <- as.numeric(locbias)
    locbias = list(lon = locbias[1], lat = locbias[2])
  }
  locbias
}


fit_original <- function(res) {
  lapply(seq_along(res), augment_response, res)
}


augment_response <- function(i, res) {
  x <- res[[i]]
  n <- nrow(x)
  cbind(idx = rep(i, n + !n), if (n) x else res_proto())
}


res_proto <- function() {
  extent <- list(
    osm_type = NA_character_,
    osm_id = NA_integer_,
    country = NA_character_,
    osm_key = NA_character_,
    countrycode = NA_character_,
    osm_value = NA_character_,
    name = NA_character_,
    type = NA_character_,
    extent = list(rep(NA_real_, 4))
  )
  attr(extent, "row.names") <- 1L
  attr(extent, "class") <- "data.frame"
  sf::st_sf(extent, geometry = sf::st_sfc(sf::st_point(), crs = 4326))
}
