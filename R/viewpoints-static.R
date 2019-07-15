#' @include viewpoints.R
NULL

new_static_viewpoint <- function(name, label, alphabet_size, discrete, mapping) {
  alphabet_size <- as.integer(alphabet_size)
  checkmate::qassert(alphabet_size, "x1")
  if (discrete) {
    checkmate::qassert(mapping, "X[1,)")
    mapping <- as.integer(mapping)
    stopifnot(alphabet_size == max(mapping))
    stopifnot(length(mapping) == hrep::alphabet_size("pc_chord"))
  }
  new_viewpoint(
    name,
    label,
    alphabet_size,
    discrete,
    f_obs = function(chord_ids, chords, ...) {
      if (is.function(mapping)) mapping <- mapping()
      mapping[chord_ids]
    },
    f_all = function(chord_ids, chords, ...) {
      if (is.function(mapping)) mapping <- mapping()
      matrix(rep(mapping, times = length(chord_ids)),
             byrow = TRUE,
             nrow = length(chord_ids))
    }
  )
}

new_static_viewpoint(
  "pc_chord",
  "PC chord",
  alphabet_size = hrep::alphabet_size("pc_chord"),
  discrete = TRUE,
  mapping = seq_len(hrep::alphabet_size("pc_chord"))
) %>% register_viewpoint()

new_static_viewpoint(
  "pc_set_rel_root",
  "PC set rel. root",
  length(levels(hvrmap::map_pc_chord$pc_set_rel_root_id)),
  discrete = TRUE,
  mapping = hvrmap::map_pc_chord$pc_set_rel_root_id
) %>% register_viewpoint()

new_static_viewpoint(
  "pc_set",
  "PC set",
  hrep::alphabet_size("pc_set"),
  discrete = TRUE,
  mapping = hvrmap::map_pc_chord$pc_set_id
) %>% register_viewpoint()

new_static_viewpoint(
  "bass_pc",
  "Bass PC",
  alphabet_size = 12L,
  discrete = TRUE,
  mapping = hvrmap::map_pc_chord$bass_pc_id
) %>% register_viewpoint()

new_static_viewpoint(
  "root_pc",
  "Root PC",
  alphabet_size = 12L,
  discrete = TRUE,
  mapping = hvrmap::map_pc_chord$root_pc_id
) %>% register_viewpoint()

new_static_viewpoint(
  "bass_pc_rel_root",
  "Bass PC rel. root",
  alphabet_size = 12L,
  discrete = TRUE,
  mapping = hvrmap::map_pc_chord$bass_pc_rel_root_id
) %>% register_viewpoint()

new_static_viewpoint(
  "pc_set_rel_bass", # a.k.a. pc_chord_type
  "PC set rel. bass",
  hrep::alphabet_size("pc_chord_type"),
  discrete = TRUE,
  mapping = hvrmap::map_pc_chord$pc_chord_type_id
) %>% register_viewpoint()

new_static_viewpoint(
  "num_pcs",
  "Number of PCs",
  alphabet_size = NA,
  discrete = FALSE,
  mapping = hvrmap::map_pc_chord$num_pcs
) %>% register_viewpoint()

new_static_viewpoint(
  "hutch_78_roughness",
  "Interference",
  alphabet_size = NA,
  discrete = FALSE,
  mapping = hvrmap::map_pc_chord$hutch_78_roughness
) %>% register_viewpoint()

new_static_viewpoint(
  "har_18_harmonicity",
  "Periodicity/harmonicity",
  alphabet_size = NA,
  discrete = FALSE,
  mapping = hvrmap::map_pc_chord$har_18_harmonicity
) %>% register_viewpoint()
