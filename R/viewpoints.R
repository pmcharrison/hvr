get_viewpoint_matrices <- function(chord_ids,
                                   viewpoints,
                                   continuations = FALSE) {
  is_discrete <- is_viewpoint_discrete(viewpoints)

  discrete_viewpoints <- viewpoints[is_discrete]
  continuous_viewpoints <- viewpoints[!is_discrete]

  list(
    discrete = get_viewpoint_matrix(chord_ids,
                                    discrete_viewpoints,
                                    discrete = TRUE,
                                    continuations = continuations),
    continuous = get_viewpoint_matrix(chord_ids,
                                      continuous_viewpoints,
                                      discrete = FALSE,
                                      continuations = continuations)
  )
}

is_viewpoint_discrete <- function(viewpoints) {
  checkmate::qassert(viewpoints, "S")
  invalid <- !viewpoints %in% names(.viewpoints)
  if (any(invalid))
    stop("the following viewpoint names are invalid: ",
         paste(viewpoints[invalid], collapse = ", "))
  purrr::map_lgl(viewpoints, ~ .viewpoints[[.]]$discrete)
}

get_viewpoint_matrix <- function(chord_ids,
                                 viewpoints,
                                 discrete,
                                 continuations = FALSE) {
  checkmate::qassert(discrete, "B1")
  stopifnot(all(is_viewpoint_discrete(viewpoints) == discrete))

  chords <- hrep::decode(hrep::coded_vec(chord_ids, "pc_chord"))
  res <- array(dim = c(length(viewpoints),
                       length(chord_ids),
                       if (continuations) hrep::alphabet_size("pc_chord")),
               data = if (discrete) as.integer(NA) else as.numeric(NA))
  dimnames(res) <- list(viewpoints)

  # tonics <- if (need_tonics(viewpoints)) get_tonics(chords)
  # root_pcs <- if (need_root_pcs(viewpoints)) get_root_pcs(chords)

  for (i in seq_along(viewpoints)) {
    vpt_i <- viewpoints[i]
    if (continuations) {
      res[i, , ] <- get_viewpoint(vpt_i)$f_all(chord_ids = chord_ids,
                                               chords = chords)
    } else {
      res[i, ] <- get_viewpoint(vpt_i)$f_obs(chord_ids = chord_ids,
                                             chords = chords)
    }
  }
  res
}

.viewpoints <- list()

get_viewpoint <- function(x) .viewpoints[[x]]

new_viewpoint <- function(label, alphabet_size, discrete, f_obs, f_all) {
  checkmate::qassert(label, "S1")
  checkmate::qassert(alphabet_size, "x1")
  checkmate::qassert(discrete, "B1")
  stopifnot(is.function(f_obs))
  stopifnot(is.function(f_all))
  if (label %in% names(.viewpoints)) stop("viewpoint ", label, " already exists")
  .viewpoints[[label]] <<-
    list(
      alphabet_size = as.integer(alphabet_size),
      discrete = discrete,
      f_obs = f_obs,
      f_all = f_all
    )
  .viewpoints <<- .viewpoints[order(names(.viewpoints))]
}
