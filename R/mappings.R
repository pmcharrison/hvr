new_map <- list()

new_map$bass_pc <- function(pc_chords) {
  pc_chords %>%
    purrr::map_int(~ as.integer(hrep::get_bass_pc(.)))
}

new_map$pc_chord_type <- function(pc_chords) {
  message("Generating pitch-class chord type mappings...")
  x <- pc_chords %>%
    hrep::vec("pc_chord") %>%
    hrep::represent("pc_chord_type") %>%
    hrep::encode() %>%
    as.integer()
  message("Done.")
  x
}

new_map$pc_set_rel_root <- function(pc_chords, root_pcs) {
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

new_map$transpose_pc_chord_id <- function(pc_chords) {
  n <- hrep::alphabet_size("pc_chord")
  stopifnot(n == length(pc_chords))
  x <- matrix(nrow = 12, ncol = n)
  message("Generating transpose_pc_chord_id mapping...")
  pb <- utils::txtProgressBar(max = n, style = 3)
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

generate_ideal_voicings <- function() {
  message("Getting ideal pc_chord voicings...")

  pc_chords <- hrep::list_chords("pc_chord")

  cache_dir <- "cache/get_ideal_voicing"
  R.utils::mkdirs(cache_dir)
  get_ideal_voicing <- memoise::memoise(get_ideal_voicing,
                                        cache = memoise::cache_filesystem(cache_dir))

  .pc_chord_ideal_voicings <- plyr::llply(pc_chords,
                                          get_ideal_voicing,
                                          .progress = "text")
  save(.pc_chord_ideal_voicings, file = "data/pc_chord_ideal_voicings.rda")
}

get_ideal_voicing <- function(pc_chord) {
  x <- hrep::vec(list(pc_chord), "pc_chord")
  size <- length(pc_chord)
  opt <- voicer::voice_opt(min_notes = 1L,
                           max_notes = pmax(5, size),
                           max_octave = 0,
                           features = voicer::voice_features(ideal_num_notes = 5L),
                           verbose = FALSE)
  voicer::voice(x, opt)[[1]]
}

generate_mappings <- function() {
  pc_chord_ids <- seq_len(hrep::alphabet_size("pc_chord"))
  pc_chords <- hrep::list_chords("pc_chord")

  bass_pcs <- new_map$bass_pc(pc_chords)    # alphabet: 0-11
  root_pcs <- parn88::root_by_pc_chord  # alphabet: 0-11

  bass_pc_rel_root <- (bass_pcs - root_pcs) %% 12L

  message("Applying consonance models...")
  R.utils::mkdirs("cache/incon")
  incon <- memoise::memoise(incon::incon,
                            cache = memoise::cache_filesystem("cache/incon"))
  # voicings <- hvr::.pc_chord_ideal_voicings
  voicings <- purrr::map(pc_chords, hrep::pi_chord)
  consonance <- plyr::laply(voicings, incon, model = c("hutch_78_roughness",
                                                       "har_18_harmonicity"),
                            .progress = "text")

  .map_pc_chord <- list(
    pc_chord_type_id = new_map$pc_chord_type(pc_chords), # alphabet: pc_chord_type

    pc_set_id = hrep::pc_chord_id_to_pc_set_id_map,  # alphabet: pc_set
    pc_set_rel_root_id = new_map$pc_set_rel_root(pc_chords, root_pcs), # alphabet: pc_set_rel_root

    num_pcs = purrr::map(pc_chords, length),

    hutch_78_roughness = consonance[, "hutch_78_roughness"],
    har_18_harmonicity = consonance[, "har_18_harmonicity"],

    bass_pc_id = bass_pcs + 1L,
    root_pc_id = root_pcs + 1L,
    bass_pc_rel_root_id = bass_pc_rel_root + 1L,

    bass_pc = bass_pcs,
    root_pc = root_pcs,
    bass_pc_rel_root = bass_pc_rel_root
  )
  save(.map_pc_chord, file = "data/map_pc_chord.rda")

  .transpose_pc_chord_id <- new_map$transpose_pc_chord_id(pc_chords)
  save(.transpose_pc_chord_id, file = "data/transpose_pc_chord_id.rda")
}

transpose_pc_chord_id <- function(pc_chord_id, interval) {
  checkmate::qassert(interval, "X1")
  .transpose_pc_chord_id[(interval %% 12L) + 1L, pc_chord_id]
}

# map_pc_chord <- function(pc_chord_id, to) {
#   checkmate::qassert(to, "S1")
#   .map_pc_chord[[to]][pc_chord_id]
# }
