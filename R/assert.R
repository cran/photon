ignore_null <- function() {
  args <- parent.frame()
  if (is.null(args$x) && isTRUE(args$null)) {
    return_from_parent(NULL, .envir = parent.frame())
  }
}


get_caller_name <- function(parent = sys.parent()) {
  deparse(sys.call(parent)[[1]])
}


assert_length <- function(x, len = 1, null = FALSE) {
  ignore_null()
  cond <- length(x) == len
  if (!cond) {
    var <- deparse(substitute(x))
    ph_stop(
      "{.code {var}} must have length {.field {len}}, not {.field {length(x)}}.",
      class = get_caller_name()
    )
  }
}


assert_vector <- function(x, type, null = FALSE) {
  ignore_null()
  cond <- is.atomic(x) && typeof(x) == type
  if (!cond) {
    var <- deparse(substitute(x))
    ph_stop(
      "{.code {var}} must be an atomic vector of type {.cls {type}}, not {.cls {typeof(x)}}.",
      class = get_caller_name()
    )
  }
}


assert_flag <- function(x, null = FALSE) {
  ignore_null()
  cond <- is.logical(x) && !is.na(x)
  if (!cond) {
    var <- deparse(substitute(x))
    ph_stop(
      "{.code {var}} must be a vector consisting only of TRUE or FALSE.",
      class = get_caller_name()
    )
  }
}


assert_dir <- function(x, null = FALSE) {
  ignore_null()
  cond <- is.character(x) && file.exists(x) && file.info(x)$isdir
  if (!cond) {
    var <- deparse(substitute(x))
    ph_stop(
      "{.code {var}} must be a valid path to an existing directory.",
      class = get_caller_name()
    )
  }
}


assert_url <- function(x, null = FALSE) {
  ignore_null()
  cond <- is.character(x) && is_url(x)
  if (!cond) {
    var <- deparse(substitute(x))
    ph_stop(
      "{.code {var}} must be a valid URL.",
      class = get_caller_name()
    )
  }
}


assert_class <- function(x, class, null = FALSE) {
  ignore_null()
  cond <- inherits(x, class)
  if (!cond) {
    var <- deparse(substitute(x))
    ph_stop(
      "{.code {var}} must be a of class {.cls {class}}, not {.cls {class(x)}}.",
      class = get_caller_name()
    )
  }
}


assert_named <- function(x, names, all = FALSE, null = FALSE) {
  ignore_null()
  cond <- if (all) {
    all(names(x) %in% names)
  } else {
    any(names(x) %in% names)
  }
  if (!cond) {
    var <- deparse(substitute(x))
    names <- cli::cli_vec(names, style = list("vec-last" = ", "))
    ph_stop(
      "{.code {var}} must contain at least one of the following names: {.val {names}}",
      class = get_caller_name()
    )
  }
}


assert_range <- function(x, min, max, than = TRUE) {
  ignore_null()
  cond <- if (than) {
    all(x > min, x < max)
  } else {
    all(x >= min, x <= max)
  }
  if (!cond) {
    var <- deparse(substitute(x))
    ph_stop(
      "{.code {var}} be greater than {min} and lower than {max}, got {.field {x}} instead.",
      class = get_caller_name()
    )
  }
}
