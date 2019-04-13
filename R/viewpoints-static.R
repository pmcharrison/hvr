#' @include viewpoints.R
NULL

new_static_viewpoint <- function(label, alphabet_size, discrete, mapping) {
  checkmate::qassert(mapping, "X[1,)")
  mapping <- as.integer(mapping)
  stopifnot(alphabet_size == max(mapping))
  stopifnot(length(mapping) == hrep::alphabet_size("pc_chord"))
  new_viewpoint(label, alphabet_size, discrete, function(chord_ids, chords, continuations, ...) {
    if (continuations) {
      matrix(rep(mapping, times = length(chord_ids)),
             byrow = TRUE,
             nrow = length(chord_ids))
    } else {
      mapping[chord_ids]
    }
  })
}

new_static_viewpoint("pc_chord",
                     alphabet_size = hrep::alphabet_size("pc_chord"),
                     discrete = TRUE,
                     mapping = seq_len(hrep::alphabet_size("pc_chord")))

new_static_viewpoint("pc_set",
                     hrep::alphabet_size("pc_set"),
                     discrete = TRUE,
                     mapping = hvr::.map_pc_chord$pc_set_id)

new_static_viewpoint("pc_set_rel_root",
                     length(levels(hvr::.map_pc_chord$pc_set_rel_root_id)),
                     discrete = TRUE,
                     mapping = hvr::.map_pc_chord$pc_set_rel_root_id)

new_static_viewpoint("bass_pc",
                     alphabet_size = 12L,
                     discrete = TRUE,
                     mapping = hvr::.map_pc_chord$bass_pc_id)

new_static_viewpoint("root_pc",
                     alphabet_size = 12L,
                     discrete = TRUE,
                     mapping = hvr::.map_pc_chord$root_pc_id)

new_static_viewpoint("bass_pc_rel_root",
                     alphabet_size = 12L,
                     discrete = TRUE,
                     mapping = hvr::.map_pc_chord$bass_pc_rel_root_id)

new_static_viewpoint("pc_chord_type",
                     hrep::alphabet_size("pc_chord_type"),
                     discrete = TRUE,
                     mapping = hvr::.map_pc_chord$pc_chord_type_id)