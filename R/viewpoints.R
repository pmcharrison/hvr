#' @export
hvr_viewpoints <- list()

#' @export
new_viewpoint <- function(name, alphabet_size, discrete, f_obs, f_all) {
  checkmate::qassert(name, "S1")
  checkmate::qassert(alphabet_size, "x1")
  checkmate::qassert(discrete, "B1")
  stopifnot(is.function(f_obs))
  stopifnot(is.function(f_all))
  x <- list(
    name = name,
    alphabet_size = as.integer(alphabet_size),
    discrete = discrete,
    f_obs = f_obs,
    f_all = f_all
  )
  class(x) <- c("viewpoint")
  x
}

#' @export
print.viewpoint <- function(x, ...) {
  cat(if (is_discrete(x)) "Discrete " else "Continuous ",
      "viewpoint: '", name(x), "'\n", sep = "")
}

#' Is discrete
#'
#' Checks if a viewpoint is discrete.
#'
#' @param x Viewpoint to check.
#' @return Logical scalar.
#' @rdname is_discrete
#' @export
is_discrete <- function(x, ...) {
  UseMethod("is_discrete")
}

#' @export
#' @rdname is_discrete
is_discrete.viewpoint <- function(x, ...) {
  x$discrete
}

#' @export
is_viewpoint <- function(x) {
  is(x, "viewpoint")
}

#' @export
name <- function(x) {
  stopifnot(is_viewpoint(x))
  x$name
}

register_viewpoint <- function(x) {
  checkmate::qassert(x$name, "S1")
  if (x$name %in% names(hvr_viewpoints))
    stop("viewpoint ", x$name, " already exists")
  hvr_viewpoints[[x$name]] <<- x
  hvr_viewpoints <<- hvr_viewpoints[order(purrr::map_chr(hvr_viewpoints, name))]
  x
}

# is_viewpoint_discrete <- function(viewpoints) {
#   checkmate::qassert(viewpoints, "S")
#   invalid <- !viewpoints %in% names(viewpoints)
#   if (any(invalid))
#     stop("the following viewpoint names are invalid: ",
#          paste(viewpoints[invalid], collapse = ", "))
#   purrr::map_lgl(viewpoints, ~ viewpoints[[.]]$discrete)
# }
