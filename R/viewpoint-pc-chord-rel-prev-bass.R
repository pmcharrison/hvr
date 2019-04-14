#' @include viewpoints.R
NULL

new_viewpoint(
  label = "pc_chord_rel_prev_bass",
  alphabet_size = hrep::alphabet_size("pc_chord"),
  discrete = TRUE,
  f_obs = function(chords, chord_ids, ...) {
    bass_pcs <- hvr::.map_pc_chord$bass_pc[chord_ids]
    c(as.integer(NA), purrr::map2_int(chord_ids[- 1],
                                      bass_pcs[- length(chord_ids)],
                                      ~ transpose_pc_chord_id(.x, - .y)))
  },
  f_all = function(chords, chord_ids, ...) {
    res <- matrix(data = as.numeric(NA),
                  ncol = hrep::alphabet_size("pc_chord"),
                  nrow = length(chord_ids))
    if (length(chord_ids) > 1) {
      observed_bass_pcs <- hvr::.map_pc_chord$bass_pc[chord_ids]
      for (i in seq(from = 2, to = length(chord_ids))) {
        res[i, ] <- pc_chord_rel_prev_bass(ref_bass_pc = observed_bass_pcs[i - 1L])
      }
    }
    res
  }
)

.pc_chord_rel_prev_bass <- purrr::map(0:11, function(ref_bass_pc) {
  chord_ids <- seq_len(hrep::alphabet_size("pc_chord"))
  transpose_pc_chord_id(chord_ids, - ref_bass_pc)
})

pc_chord_rel_prev_bass <- function(ref_bass_pc) {
  checkmate::qassert(ref_bass_pc, "X1[0,11]")
  .pc_chord_rel_prev_bass[[ref_bass_pc + 1L]]
}
