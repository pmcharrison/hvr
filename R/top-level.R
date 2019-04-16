#' @export
compute_viewpoints <- function(corpus,
                               dir,
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

  # Main
  R.utils::mkdirs(dir)
  seq_train_only <- setdiff(seq_along(corpus), seq_test)
  saveRDS(corpus, file.path(dir, "corpus.rds"))
  yaml::write_yaml(list(corpus_size = length(corpus),
                        seq_test = seq_test,
                        seq_train_only = seq_train_only,
                        viewpoints = purrr::map_chr(viewpoints, name)),
                   file.path(dir, "about.yaml"))
  compute_train_only_viewpoints(seq_train_only,
                                viewpoints,
                                verbose,
                                corpus,
                                dir)
  compute_test_only_viewpoints(seq_test,
                               viewpoints,
                               verbose,
                               corpus,
                               dir)
}

compute_train_only_viewpoints <- function(seq_train_only,
                                          viewpoints,
                                          verbose,
                                          corpus,
                                          dir) {
  R.utils::mkdirs(file.path(dir, "viewpoints"))
  if (verbose) message("Computing viewpoints for train-only sequences...")
  seq_train_only %>%
    magrittr::set_names(., .) %>%
    purrr::map(~ corpus[[.]]) %>%
    plyr::llply(get_viewpoint_matrices,
                viewpoints = viewpoints,
                continuations = FALSE,
                .progress = if (verbose) "text" else "none") %>%
    saveRDS(file.path(dir, "viewpoints", "train-only.rds"))
}

compute_test_only_viewpoints <- function(seq_test,
                                         viewpoints,
                                         verbose,
                                         corpus,
                                         dir) {
  R.utils::mkdirs(file.path(dir, "viewpoints", "test"))

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
      saveRDS(file.path(dir, "viewpoints", "test", paste0(seq_id, ".rds")))
  }
}
