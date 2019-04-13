get_viewpoint_matrices <- function(chord_ids,
                                   viewpoints,
                                   continuations = FALSE) {
  is_discrete <- is_viewpoint_discrete(viewpoints)

  discrete_viewpoints <- viewpoints[is_discrete]
  continuous_viewpoints <- viewpoints[!is_discrete]

  list(
    discrete = get_discrete_viewpoint_matrices(chord_ids,
                                               discrete_viewpoints,
                                               continuations),
    continuous = get_continuous_viewpoint_matrices(chord_ids,
                                                   continuous_viewpoints,
                                                   continuations)
  )
}

viewpoint_type <- function(viewpoints) {
  checkmate::qassert(viewpoints, "S")
  invalid <- !viewpoints %in% names(.viewpoints)
  purrr::map_lgl(viewpoints, ~ .viewpoints[[.]]$discrete)
}

get_viewpoint_matrix <- function(chord_ids,
                                 viewpoints,
                                 continuations = FALSE) {
  chords <- hrep::decode(hrep::coded_vec(sequence, "pc_chord"))
  res <- array(dim = c(length(viewpoints),
                       length(sequence),
                       if (continuations) hrep::alphabet_size("pc_chord")))
  dimnames(res) <- list(viewpoints)

  # tonics <- if (need_tonics(viewpoints)) get_tonics(chords)
  # root_pcs <- if (need_root_pcs(viewpoints)) get_root_pcs(chords)

  for (i in seq_along(viewpoints)) {
    vpt_i <- viewpoints[i]
    res_i <- get_viewpoint(vpt_i)$fun(chord_ids = chord_ids,
                                      chords = chords,
                                      continuations = continuations)
    # tonics = tonics)
    if (continuations)
      res[i, , ] <- res_i else
        res[i, ] <- res_i
  }
  res
}

# need_root_pcs <- function(viewpoints) FALSE
# need_tonics <- function(viewpoints) FALSE

.viewpoints <- list()

get_viewpoint <- function(x) .viewpoints[[x]]

new_viewpoint <- function(label, alphabet_size, discrete, fun) {
  checkmate::qassert(label, "S1")
  checkmate::qassert(alphabet_size, "x1")
  checkmate::qassert(discrete, "B1")
  stopifnot(is.function(fun))
  if (label %in% names(.viewpoints)) stop("viewpoint ", label, " already exists")
  .viewpoints[[label]] <<- list(
    fun = fun,
    alphabet_size = as.integer(alphabet_size),
    discrete = discrete
  )
}
