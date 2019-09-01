#' @include viewpoints.R
NULL

#' New static viewpoint
#'
#' A helper function for defining viewpoints that do not depend
#' on their preceding context.
#'
#' @param name
#' (Character scalar)
#' Name for the viewpoint (e.g. \code{pc_chord}).
#'
#' @param label
#' (Character scalar)
#' Label for the viewpoint (e.g. "Pitch-class chord").
#'
#' @param alphabet_size
#' For a discrete viewpoint, the number of symbols in the viewpoint's alphabet;
#' for a continuous viewpoint, \code{NA}.
#'
#' @param discrete
#' (Logical scalar)
#' Whether the viewpoint is discrete as opposed to continuous.
#'
#' @param mapping
#' An integer vector of length 24,576
#' (the alphabet size of \code{\link[hrep]{pc_chord}}),
#' where the ith element corresponds to the viewpoint value for chord i,
#' with chord i being encoded using the \code{\link[hrep]{pc_chord}}
#' encoding from the \code{hrep} package.
#'
#' @seealso
#' \code{\link{new_viewpoint}} for defining viewpoints that do depend on
#' their contexts.
#'
#' @export
new_static_viewpoint <- function(name, label, alphabet_size, discrete, mapping) {
  alphabet_size <- as.integer(alphabet_size)
  checkmate::qassert(alphabet_size, "x1")
  if (discrete) {
    mapping <- as.integer(mapping)
    checkmate::qassert(mapping, "X[1,)")
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
