get_viewpoint_matrix <- function(chord_ids,
                                 viewpoints,
                                 continuations = FALSE) {
  chords <- hrep::decode(hrep::coded_vec(sequence, "pc_chord"))
  res <- array(dim = c(length(viewpoints),
                       length(sequence),
                       if (continuations) hrep::alphabet_size("pc_chord")))
  dimnames(res) <- list(viewpoints)
  tonics <- if (need_tonics(viewpoints)) get_tonics(chords)
  # roots <- if (need_roots(viewpoints)) get_roots(chords)
  for (i in seq_along(viewpoints)) {
    vpt_i <- viewpoints[i]
    res_i <- get_viewpoint(vpt_i)$fun(chord_ids = chord_ids,
                                      chords = chords,
                                      continuations = continuations,
                                      tonics = tonics)
    if (continuations)
      res[i, , ] <- res_i else
        res[i, ] <- res_i
  }
  res
}

need_roots <- function(viewpoints) FALSE

need_tonics <- function(viewpoints) FALSE

.viewpoints <- list()

get_viewpoint <- function(x) .viewpoints[[x]]

new_viewpoint <- function(label, alphabet_size, fun) {
  checkmate::qassert(label, "S1")
  checkmate::qassert(alphabet_size, "X1")
  stopifnot(is.function(fun))
  if (label %in% names(.viewpoints)) stop("viewpoint ", label, " already exists")
  .viewpoints[[label]] <<- list(
    fun = fun,
    alphabet_size = as.integer(alphabet_size)
  )
}

new_static_viewpoint <- function(label, alphabet_size, mapping) {
  checkmate::qassert(mapping, "X[1,)")
  stopifnot(alphabet_size == max(mapping))
  stopifnot(length(mapping) == hrep::alphabet_size("pc_chord"))
  mapping <- as.integer(mapping)
  new_viewpoint(label, alphabet_size, function(chord_ids, chords, continuations, ...) {
    if (continuations) {
      matrix(rep(mapping, times = length(chord_ids)),
             byrow = TRUE,
             nrow = length(chord_ids))
    } else {
      mapping[chord_ids]
    }
  })
}

init_static_viewpoints <- function() {
  pc_chord_ids <- seq_len(hrep::alphabet_size("pc_chord"))
  pc_chords <- hrep::list_chords("pc_chord")

  new_static_viewpoint("pc_chord",
                       alphabet_size = hrep::alphabet_size("pc_chord"),
                       mapping = pc_chord_ids)

  bass_pcs <- pc_chords %>%
    purrr::map_int(~ as.integer(hrep::get_bass_pc(.)))

  new_static_viewpoint("bass_pc",
                       alphabet_size = 12L,
                       mapping = bass_pcs %>% reindex_pc())

  roots <- parn88::root_by_pc_chord

  new_static_viewpoint("root_pc",
                       alphabet_size = 12L,
                       mapping = roots %>% reindex_pc())

  new_static_viewpoint("bass_pc_rel_root",
                       alphabet_size = 12L,
                       mapping = ((bass_pcs - roots) %% 12L) %>% reindex_pc())

  pc_chord_type_ids <- pc_chords %>%
    hrep::vec("pc_chord") %>%
    hrep::represent("pc_chord_type") %>%
    hrep::encode() %>%
    as.integer()

  new_static_viewpoint("pc_chord_type",
                       hrep::alphabet_size("pc_chord_type"),
                       mapping = pc_chord_type_ids)

  pc_chord_type_rel_root_ids <- pc_chords %>%
    purrr::map2(roots, ~ hrep::edit_bass_pc(.x, .y)) %>%
    hrep::represent("pc_chord_type") %>%
    hrep::encode() %>%
    as.integer()
  # encode these as pitch-class sets instead

  new_static_viewpoint("pc_chord_type_rel_root",
                       hrep::alphabet_size("pc_chord_type"),
                       )

}

reindex_pc <- function(x) {
  ifelse(x == 0L, 12L, x)
}

# new_static_viewpoint("pc_chord", mapping = seq_len(hrep::alphabet_size("pc_chord")))
#
# new_static_viewpoint("bass_pc",
#                      mapping = hrep::list_chords("pc_chord") %>%
#                        purrr::map_int(~ as.integer(hrep::get_bass_pc(.))) %>%
#                        ifelse(. == 0L, 12L, .))

# new_static_viewpoint("bass_pc_ref_root",
#                      mapping = hrep::list_chords("pc_chord") %>%
#                        purrr::)
