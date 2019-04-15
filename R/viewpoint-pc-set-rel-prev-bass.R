#' @include viewpoints.R
NULL

new_viewpoint(
  name  = "pc_set_rel_prev_bass",
  alphabet_size = hrep::alphabet_size("pc_set"),
  discrete = TRUE,
  f_obs = function(chords, chord_ids, ...) {
    bass_pcs <- hvrmap::map_pc_chord$bass_pc[chord_ids]
    rel_chord_ids <- purrr::map2_int(chord_ids[- 1L],
                                     bass_pcs[- length(chord_ids)],
                                     ~ hvrmap::transpose_pc_chord_id(.x, - .y))
    c(as.integer(NA),
      hvrmap::map_pc_chord$pc_set_id[rel_chord_ids])
  },
  f_all = function(chords, chord_ids, ...) {
    res <- matrix(data = as.numeric(NA),
                  ncol = hrep::alphabet_size("pc_chord"),
                  nrow = length(chord_ids))
    if (length(chord_ids) > 1) {
      observed_bass_pcs <- hvrmap::map_pc_chord$bass_pc[chord_ids]
      for (i in seq(from = 2, to = length(chord_ids))) {
        res[i, ] <- pc_set_rel_prev_bass(ref_bass_pc = observed_bass_pcs[i - 1L])
      }
    }
    res
  }
) %>% register_viewpoint()

.pc_set_rel_prev_bass <- purrr::map(0:11, function(ref_bass_pc) {
  chord_ids <- seq_len(hrep::alphabet_size("pc_chord"))
  rel_chord_ids <- hvrmap::transpose_pc_chord_id(chord_ids, - ref_bass_pc)
  hvrmap::map_pc_chord$pc_set_id[rel_chord_ids]
})

pc_set_rel_prev_bass <- function(ref_bass_pc) {
  checkmate::qassert(ref_bass_pc, "X1[0,11]")
  .pc_set_rel_prev_bass[[ref_bass_pc + 1L]]
}
