#' @include viewpoints.R
NULL

new_viewpoint(
  name = "bass_int",
  alphabet_size = 12,
  discrete = TRUE,
  f_obs = function(chords, chord_ids, ...) {
    bass_pcs <- hvrmap::map_pc_chord$bass_pc[chord_ids]
    c(as.integer(NA), 1L + (diff(bass_pcs) %% 12L))
  },
  f_all = function(chords, chord_ids, ...) {
    res <- matrix(data = as.integer(NA),
                  ncol = hrep::alphabet_size("pc_chord"),
                  nrow = length(chord_ids))
    if (length(chord_ids) > 1) {
      observed_bass_pcs <- hvrmap::map_pc_chord$bass_pc[chord_ids]
      for (i in seq(from = 2, to = length(chord_ids))) {
        res[i, ] <- bass_ints(ref_bass_pc = observed_bass_pcs[i - 1L])
      }
    }
    res
  }
) %>% register_viewpoint()

.bass_ints <- purrr::map(0:11, function(ref_bass_pc) {
  bass_pcs <- hvrmap::map_pc_chord$bass_pc
  rel_bass_pcs <- (bass_pcs - ref_bass_pc) %% 12L
  rel_bass_pcs + 1L
})

bass_ints <- function(ref_bass_pc) {
  checkmate::qassert(ref_bass_pc, "X1[0,11]")
  .bass_ints[[ref_bass_pc + 1L]]
}
