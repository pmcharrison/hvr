#' @include viewpoints.R
NULL

new_viewpoint(
  name = "root_int",
  label = "Root interval",
  alphabet_size = 12,
  discrete = TRUE,
  f_obs = function(chords, chord_ids, ...) {
    root_pcs <- hvrmap::map_pc_chord$root_pc[chord_ids]
    c(as.integer(NA), 1L + (diff(root_pcs) %% 12L))
  },
  f_all = function(chords, chord_ids, ...) {
    res <- matrix(data = as.integer(NA),
                  ncol = hrep::alphabet_size("pc_chord"),
                  nrow = length(chord_ids))
    if (length(chord_ids) > 1) {
      observed_root_pcs <- hvrmap::map_pc_chord$root_pc[chord_ids]
      for (i in seq(from = 2, to = length(chord_ids))) {
        res[i, ] <- root_ints(ref_root_pc = observed_root_pcs[i - 1L])
      }
    }
    res
  }
) %>% register_viewpoint()

.root_ints <- purrr::map(0:11, function(ref_root_pc) {
  root_pcs <- hvrmap::map_pc_chord$root_pc
  rel_root_pcs <- (root_pcs - ref_root_pc) %% 12L
  rel_root_pcs + 1L
})

root_ints <- function(ref_root_pc) {
  checkmate::qassert(ref_root_pc, "X1[0,11]")
  .root_ints[[ref_root_pc + 1L]]
}
