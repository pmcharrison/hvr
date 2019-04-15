#' @include viewpoints.R

get_viewpoint_matrices <- function(chord_ids,
                                   viewpoints = hvr::hvr_viewpoints,
                                   continuations = FALSE,
                                   verbose = TRUE) {
  # if (is.null(names(viewpoints))) stop("viewpoint list should be named")
  # if (anyDuplicated(names(viewpoints))) stop("duplicate viewpoint names are not permitted")

  discrete <- purrr::map_lgl(viewpoints, is_discrete)
  discrete_viewpoints <- viewpoints[discrete]
  continuous_viewpoints <- viewpoints[!discrete]

  list(
    discrete = get_viewpoint_matrix(chord_ids,
                                    discrete_viewpoints,
                                    discrete = TRUE,
                                    continuations = continuations,
                                    verbose = verbose),
    continuous = get_viewpoint_matrix(chord_ids,
                                      continuous_viewpoints,
                                      discrete = FALSE,
                                      continuations = continuations,
                                      verbose = verbose)
  )
}

get_viewpoint_matrix <- function(chord_ids,
                                 viewpoints,
                                 discrete,
                                 continuations,
                                 verbose) {
  checkmate::qassert(discrete, "B1")
  checkmate::qassert(continuations, "B1")
  checkmate::qassert(verbose, "B1")
  stopifnot(all(purrr::map_lgl(viewpoints, is_discrete) == discrete))

  chords <- hrep::decode(hrep::coded_vec(chord_ids, "pc_chord"))
  res <- array(dim = c(length(viewpoints),
                       length(chord_ids),
                       if (continuations) hrep::alphabet_size("pc_chord")),
               data = if (discrete) as.integer(NA) else as.numeric(NA))
  dimnames(res) <- list(purrr::map_chr(viewpoints, name))

  for (i in seq_along(viewpoints)) {
    vpt_i <- viewpoints[[i]]
    if (continuations) {
      res[i, , ] <- vpt_i$f_all(chord_ids = chord_ids,
                                chords = chords,
                                verbose = verbose) %>%
        check_viewpoint_output(discrete,
                               continuations,
                               length(chord_ids),
                               name(vpt_i))
    } else {
      res[i, ] <- vpt_i$f_obs(chord_ids = chord_ids,
                              chords = chords,
                              verbose = verbose) %>%
        check_viewpoint_output(discrete,
                               continuations,
                               length(chord_ids),
                               name(vpt_i))
    }
  }
  stopifnot(if (discrete) is.integer(res) else is.numeric(res))
  class(res) <- c(paste0(if (continuations) "continuation_" else "observed_",
                         "viewpoints"),
                  class(res))
  res
}

check_viewpoint_output <- function(x, discrete, continuations, seq_length, vp_name) {
  if (discrete) {
    if (!is.integer(x))
      stop("discrete viewpoint '", vp_name, "' failed to return an integer output")
  } else {
    if (!is.numeric(x))
      stop("continuous viewpoint '", vp_name, "' failed to return a numeric output")
  }
  if (continuations) {
    if (ncol(x) != 24576L)
      stop("problem with viewpoint '", vp_name, "': ",
           "f_all must return a matrix with 24,576 columns, ",
           "corresponding to the 24,576 possible pc_chords")
  } else {
    if (is.matrix(x) || length(x) != seq_length)
      stop("problem with viewpoint '", vp_name, "': ",
           "f_obs must return a vector with the same length as the input sequence")
  }
  x
}

#' @export
print.observed_viewpoints <- function(x, ...) {
  cat("Viewpoint matrix (observed events only), comprising a ")
  ramify::pprint(x, ...)
}

#' @export
print.continuation_viewpoints <- function(x, ...) {
  cat("Viewpoint array:\n",
      "  ",
      dim(x)[1], " (viewpoints) x ",
      dim(x)[2], " (observed events) x ",
      dim(x)[3], " (possible continuations).\n",
      sep = "")
}
