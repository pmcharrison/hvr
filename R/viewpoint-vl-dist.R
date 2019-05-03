#' @include viewpoints.R
NULL

.min_vl_dists_cache <- list(store = memoise::cache_memory(),
                            counter = 0L) %>% as.environment()

.min_vl_dists <- memoise::memoise(minVL::min_vl_dists,
                                  cache = .min_vl_dists_cache$store)

min_vl_dists <- function(...) {
  cache <- .min_vl_dists_cache
  if (cache$counter > 1000L) memoise::forget(.min_vl_dists)
  cache$counter <- cache$counter + 1L
  .min_vl_dists(...)
}

new_viewpoint(
  name = "vl_dist",
  label = "Voice-leading distance",
  alphabet_size = NA,
  discrete = FALSE,
  f_obs = function(chord_ids, chords, ...) {
    res <- rep(as.numeric(NA), times = length(chord_ids))
    if (length(chord_ids) > 1) {
      for (i in seq(from = 2, to = length(chord_ids))) {
        res[i] <- minVL::min_vl_dist(chords[[i - 1]],
                                     chords[[i]],
                                     preserve_bass = TRUE)
      }
    }
    res
  },
  f_all = function(chord_ids, chords, verbose, ...) {
    res <- matrix(data = as.numeric(NA),
                  ncol = hrep::alphabet_size("pc_chord"),
                  nrow = length(chord_ids))
    if (length(chord_ids) > 1) {
      if (verbose) {
        message("Computing voice-leading distances...")
        pb <- utils::txtProgressBar(
          min = 1, max = length(chord_ids), style = 3)
      }
      for (i in seq(from = 2, to = length(chord_ids))) {
        res[i, ] <- min_vl_dists(chords[i - 1],
                                 hrep::pc_chord_alphabet$by_id,
                                 preserve_bass = TRUE)
        if (verbose) utils::setTxtProgressBar(pb, i)
      }
      if (verbose) close(pb)
    }
    res
  }
) %>% register_viewpoint()
