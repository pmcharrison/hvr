#' @include viewpoints.R
NULL

new_spec_dist_viewpoint <- function(half_life) {
  checkmate::qassert(half_life, "N1(0,)")
  new_viewpoint(
    label = paste0("spec_dist_", half_life),
    alphabet_size = NA,
    discrete = FALSE,
    f_obs = function(chords, chord_ids, ...) {
      spectra <- get_spectra(chord_ids)
      decay_spectra <- specdec::spectral_decay(spectra,
                                               half_life = half_life,
                                               offset = TRUE)
      res <- rep(as.numeric(NA), length = length(chord_ids))
      if (length(chord_ids) > 1L) {
        for (i in seq(from = 2L, to = length(chord_ids))) {
          hrep::cosine_similarity(decay_spectra[[i - 1L]],
                                  spectra[[i]])
        }
      }
      res
    },
    f_all = function(chords, chord_ids, ...) {
      res <- matrix(data = as.numeric(NA),
                    ncol = hrep::alphabet_size("pc_chord"),
                    nrow = length(chord_ids))
      spectra <- get_spectra(chord_ids)
      decay_spectra <- specdec::spectral_decay(spectra,
                                               half_life = half_life,
                                               offset = TRUE)
      if (length(chord_ids) > 1) {
        for (i in seq(from = 2, to = length(chord_ids))) {
          res[i, ] <- cosine_similarities(decay_spectra[[i - 1L]],
                                          hvrmap::map_pc_chord$milne_pc_spectrum)

        }
      }
      res
    }
  )
}

get_spectra <- function(chord_ids) {
  purrr::map(
    chord_ids,
    ~ hrep::.milne_pc_spectrum(hvrmap::map_pc_chord$milne_pc_spectrum[., ])
  )
}

new_spec_dist_viewpoint(half_life = 3)

# .root_ints <- purrr::map(0:11, function(ref_root_pc) {
#   root_pcs <- hvrmap::map_pc_chord$root_pc
#   rel_root_pcs <- (root_pcs - ref_root_pc) %% 12L
#   rel_root_pcs + 1L
# })
#
# root_ints <- function(ref_root_pc) {
#   checkmate::qassert(ref_root_pc, "X1[0,11]")
#   .root_ints[[ref_root_pc + 1L]]
# }
