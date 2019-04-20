#' @export
compute_viewpoints <- function(corpus,
                               parent_dir,
                               dir = file.path(parent_dir, "0-viewpoints"),
                               seq_test = seq_along(corpus),
                               viewpoints = hvr::hvr_viewpoints,
                               verbose = TRUE) {
  # Argument sanitising
  stopifnot(hrep::is.corpus(corpus))
  checkmate::qassert(dir, "S1")
  checkmate::qassert(seq_test, "X[1,)")
  stopifnot(!anyDuplicated(seq_test))
  seq_test <- sort(as.integer(seq_test))
  stopifnot(!any(seq_test > length(corpus)))
  stopifnot(all(purrr::map_lgl(viewpoints, is_viewpoint)))

  R.utils::mkdirs(dir)
  saveRDS(corpus, file.path(dir, "corpus.rds"))

  saveRDS(
    list(
      corpus_size = length(corpus),
      seq_test = seq_test,
      seq_pretrain = setdiff(seq_along(corpus), seq_test),
      discrete_viewpoints = Filter(is_discrete, viewpoints) %>%
        purrr::map(~ .[c("name", "alphabet_size")]),
      continuous_viewpoints = Filter(Negate(is_discrete), viewpoints) %>% purrr::map_chr(name)
    ),
    file.path(dir, "about.rds")
  )

  compute_training_viewpoints(viewpoints,
                              verbose,
                              corpus,
                              dir)
  compute_test_only_viewpoints(seq_test,
                               viewpoints,
                               verbose,
                               corpus,
                               dir)
}

compute_training_viewpoints <- function(viewpoints,
                                        verbose,
                                        corpus,
                                        dir) {
  if (verbose) message("Computing training viewpoints...")
  # seq_along(corpus) %>%
  # magrittr::set_names(., .) %>%
  plyr::llply(corpus,
              get_viewpoint_matrices,
              viewpoints = viewpoints,
              continuations = FALSE,
              .progress = if (verbose) "text" else "none") %>%
    saveRDS(file.path(dir, "viewpoints-training.rds"))
}

compute_test_only_viewpoints <- function(seq_test,
                                         viewpoints,
                                         verbose,
                                         corpus,
                                         dir) {
  R.utils::mkdirs(file.path(dir, "viewpoints-test"))

  for (i in seq_along(seq_test)) {
    if (verbose) message("Analysing test sequence ", i,
                         " out of ", length(seq_test), "...")
    seq_id <- seq_test[i]
    get_viewpoint_matrices(
      chord_ids = corpus[[seq_id]],
      viewpoints = viewpoints,
      continuations = TRUE,
      verbose = verbose
    ) %>%
      saveRDS(file.path(dir, "viewpoints-test", paste0(seq_id, ".rds")))
  }
}
