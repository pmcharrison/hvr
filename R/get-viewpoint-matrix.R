get_viewpoint_matrix <- function(chord_ids,
                                 viewpoints,
                                 continuations = FALSE) {
  chords <- hrep::decode(hrep::coded_vec(sequence, "pc_chord"))
  res <- array(dim = c(length(viewpoints),
                       length(sequence),
                       if (continuations) hrep::alphabet_size("pc_chord")))
  dimnames(res) <- list(viewpoints)
  tonics <- if (need_tonics(viewpoints)) get_tonics(chords)
  # root_pcs <- if (need_root_pcs(viewpoints)) get_root_pcs(chords)
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

need_root_pcs <- function(viewpoints) FALSE

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

map <- list()

map$bass_pc <- function(pc_chords) {
  pc_chords %>%
    purrr::map_int(~ as.integer(hrep::get_bass_pc(.)))
}

map$pc_chord_type <- function(pc_chords) {
  message("Generating pitch-class chord type mappings...")
  x <- pc_chords %>%
    hrep::vec("pc_chord") %>%
    hrep::represent("pc_chord_type") %>%
    hrep::encode() %>%
    as.integer()
  message("Done.")
  x
}

map$pc_set_rel_root <- function(pc_chords, root_pcs) {
  message("Generating pc_set_rel_root mapping...")
  x <- pc_chords %>%
    purrr::map(hrep::pc_set) %>%
    purrr::map2(root_pcs, ~ hrep::tp(.x, interval = - .y)) %>%
    hrep::vec("pc_set") %>%
    hrep::encode() %>%
    factor()
  message("Done.")
  x
}

map$transpose_pc_chord_id <- function(pc_chords) {
  n <- hrep::alphabet_size("pc_chord")
  stopifnot(n == length(pc_chords))
  x <- matrix(nrow = 12, ncol = n)
  pb <- utils::txtProgressBar(max = n, style = 3)
  message("Generating transpose_pc_chord_id mapping...")
  for (i in seq_len(n)) {
    x[, i] <- purrr::map(0:11, ~ hrep::tp(pc_chords[[i]], interval = .)) %>%
      hrep::vec("pc_chord") %>%
      hrep::encode() %>%
      as.integer()
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)
  message("Done.")
  x
}

generate_mappings <- function() {
  pc_chord_ids <- seq_len(hrep::alphabet_size("pc_chord"))
  pc_chords <- hrep::list_chords("pc_chord")

  bass_pcs <- map$bass_pc(pc_chords)    # alphabet: 0-11
  root_pcs <- parn88::root_by_pc_chord  # alphabet: 0-11

  .map_pc_chord <- list(
    pc_chord_type_id = map$pc_chord_type(pc_chords), # alphabet: pc_chord_type
    pc_set_id = hrep::pc_chord_id_to_pc_set_id_map,  # alphabet: pc_set
    pc_set_rel_root_id = map$pc_set_rel_root(pc_chords, root_pcs), # alphabet: pc_set_rel_root
    bass_pc_id = bass_pcs + 1L,
    root_pc_id = root_pcs + 1L,
    bass_pc = bass_pcs,
    root_pc = root_pcs
  )
  save(.map_pc_chord, file = "data/map_pc_chord.rda")

  .transpose_pc_chord_id <- map$transpose_pc_chord_id(pc_chords)
  save(.transpose_pc_chord_id, file = "data/transpose_pc_chord_id.rda")
}

transpose_pc_chord_id <- function(pc_chord_id, interval) {
  checkmate::qassert(interval, "X1")
  .transpose_pc_chord_id[(interval %% 12L) + 1L, pc_chord_id]
}

map_pc_chord <- function(pc_chord_id, to) {
  checkmate::qassert(to, "S1")
  .map_pc_chord[[to]][pc_chord_id]
}



init_static_viewpoints <- function() {


  new_static_viewpoint("pc_chord",
                       alphabet_size = hrep::alphabet_size("pc_chord"),
                       mapping = pc_chord_ids)



  new_static_viewpoint("bass_pc",
                       alphabet_size = 12L,
                       mapping = bass_pcs %>% reindex_pc())



  new_static_viewpoint("root_pc",
                       alphabet_size = 12L,
                       mapping = root_pcs %>% reindex_pc())

  new_static_viewpoint("bass_pc_rel_root",
                       alphabet_size = 12L,
                       mapping = ((bass_pcs - root_pcs) %% 12L) %>% reindex_pc())



  new_static_viewpoint("pc_chord_type",
                       hrep::alphabet_size("pc_chord_type"),
                       mapping = pc_chord_type_ids)



  new_static_viewpoint("pc_chord_type_rel_root",
                       hrep::alphabet_size("pc_chord_type"),
                       )

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
