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
                                verbose = verbose)
    } else {
      res[i, ] <- vpt_i$f_obs(chord_ids = chord_ids,
                              chords = chords,
                              verbose = verbose)
    }
  }
  res
}
