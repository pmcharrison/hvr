#' @include viewpoints.R
NULL

#' New spectral similarity viewpoint
#'
#' Defines a new spectral similarity viewpoint with an arbitrary half life.
#'
#' The spectral similarity viewpoint takes the pitch-class spectral similarity
#' model of \insertCite{Milne2016;textual}{hrep} and adds an echoic decay
#' inspired by \insertCite{Leman2000;textual}{hvr}.
#'
#' @param half_life (Numeric scalar)
#' Half-life of the decay function, in chords.
#'
#' @references
#' \insertAllCited{}
#'
#' @export
new_spec_sim_viewpoint <- function(half_life) {
  checkmate::qassert(half_life, "N1(0,)")
  new_viewpoint(
    name = paste0("spec_sim_", half_life),
    label = paste0("Spectral similarity (HL = ", half_life, ")"),
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
          res[i] <- hrep::cosine_similarity(decay_spectra[[i - 1L]],
                                            spectra[[i]])
        }
      }
      res
    },
    f_all = function(chords, chord_ids, verbose, ...) {
      res <- matrix(data = as.numeric(NA),
                    ncol = hrep::alphabet_size("pc_chord"),
                    nrow = length(chord_ids))
      spectra <- get_spectra(chord_ids)
      decay_spectra <- specdec::spectral_decay(spectra,
                                               half_life = half_life,
                                               offset = TRUE)
      if (length(chord_ids) > 1) {
        if (verbose) {
          message("Computing spectral similarities (half life = ", half_life, ")...")
          pb <- utils::txtProgressBar(
            min = 1, max = length(chord_ids), style = 3)
        }
        for (i in seq(from = 2, to = length(chord_ids))) {
          res[i, ] <- cosine_similarities(decay_spectra[[i - 1L]],
                                          hvrmap::map_pc_chord$milne_pc_spectrum)
          if (verbose) utils::setTxtProgressBar(pb, i)
        }
        if (verbose) close(pb)
      }
      res
    }
  )
}

get_spectra <- function(chord_ids) {
  purrr::map(
    chord_ids,
    ~ hrep::.milne_pc_spectrum(hvrmap::map_pc_chord$milne_pc_spectrum[., ])
  ) %>%
    hrep::vec("milne_pc_spectrum")
}

new_spec_sim_viewpoint(half_life = 3) %>% register_viewpoint()
