#' Viewpoints
#'
#' A default list of viewpoints for use in viewpoint regression.
#'
#' @docType data
#' @keywords data
#'
#' @export
hvr_viewpoints <- list()

#' New viewpoint
#'
#' Defines a new viewpoint.
#'
#' @param name
#' (Character scalar)
#' Name for the viewpoint (e.g. \code{pc_chord}).
#'
#' @param label
#' (Character scalar)
#' Label for the viewpoint (e.g. "Pitch-class chord").
#'
#' @param alphabet_size
#' For a discrete viewpoint, the number of symbols in the viewpoint's alphabet;
#' for a continuous viewpoint, \code{NA}.
#'
#' @param discrete
#' (Logical scalar)
#' Whether the viewpoint is discrete as opposed to continuous.
#'
#' @param f_obs
#' A function that takes a chord sequence as input and returns
#' an integer vector corresponding to the observed feature value for
#' each chord.
#' This function must accept three arguments, though
#' the function is free to ignore some of these arguments:
#' - \code{chord_ids} - An integer-encoded version of the chord sequence,
#' using the \code{\link[hrep]{pc_chord}} encoding from the \code{hrep} package
#' (see \code{\link[hrep]{encode}}).
#' - \code{chords} - The same chord sequence expressed as a list of
#' \code{\link[hrep]{pc_chord}} objects.
#' - \code{verbose} - (Logical scalar) Whether or not to print
#' computation progress statements.
#'
#' @param f_all
#' A function that takes a chord sequence as input and returns
#' a matrix of feature values corresponding to all possible chords that
#' could have been observed at each point in the chord sequence.
#' In particular, element \code{[i, j]} should correspond to
#' the feature value observed if the chord sequence up to and including the
#' \code{i - 1}th event were to be followed by chord \code{j},
#' where chord \code{j} is encoded using the \code{\link[hrep]{pc_chord}}
#' encoding from the \code{hrep} package.
#' The function should accept the same argments as those listed
#' above for the \code{f_obs} argument.
#'
#' @return
#' A viewpoint object that can be passed to \code{\link{compute_viewpoints}}.
#'
#' @seealso
#' \code{\link{new_static_viewpoint}} provides an efficient way to define
#' viewpoints that do not depend on the preceding chords.
#'
#' @md
#'
#' @export
new_viewpoint <- function(name, label, alphabet_size, discrete, f_obs, f_all) {
  checkmate::qassert(name, "S1")
  checkmate::qassert(label, "S1")
  checkmate::qassert(alphabet_size, "x1")
  checkmate::qassert(discrete, "B1")
  stopifnot(is.function(f_obs))
  stopifnot(is.function(f_all))
  x <- list(
    name = name,
    label = label,
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

#' Is discrete?
#'
#' Checks if a viewpoint is discrete.
#'
#' @param x Viewpoint to check.
#' @return Logical scalar.
#' @rdname is_discrete
#' @export
is_discrete <- function(x) {
  UseMethod("is_discrete")
}

#' @export
#' @rdname is_discrete
is_discrete.viewpoint <- function(x) {
  x$discrete
}

#' Is viewpoint?
#'
#' Checks whether an object is of class \code{viewpoint}.
#'
#' @param x Object to check.
#' @return Logical scalar.
#'
#' @export
is_viewpoint <- function(x) {
  is(x, "viewpoint")
}

#' Get name
#'
#' Gets the name of a viewpoint.
#'
#' @param x A viewpoint.
#'
#' @return Character scalar.
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
