#' Compute viewpoints
#'
#' This function computes viewpoints (features) for every chord sequence
#' in a corpus, and saves the result to disk.
#'
#' @param corpus
#' A corpus of chord sequences as created by \code{\link[hrep]{corpus}}.
#' Must be encoded in the \code{\link[hrep]{pc_chord}} representation.
#'
#' @param parent_dir
#' (Character scalar)
#' The parent directory for the output files, shared with functions such as
#' \code{\link{compute_ppm_analyses}} and \code{\link{compute_model_matrix}}.
#' Ignored if \code{dir} is provided.
#'
#' @param output_dir
#' (Character scalar)
#' The output directory for the viewpoint analyses.
#' Will be created if it doesn't exist already.
#'
#' @param seq_test
#' (Integer vector)
#' Provides the indices of the sequences in \code{corpus} that will
#' eventually be predicted by the model.
#' Sequences not included in \code{seq_test} will only be used for training.
#' Training sequences only need viewpoints to be computed for observed chords,
#' not the full alphabet of possible chords.
#'
#' @param viewpoints
#' List of viewpoints to apply, as created by \code{\link{new_viewpoint}}.
#' Defaults to a fairly comprehensive list, \code{\link{hvr_viewpoints}}.
#'
#' @param verbose
#' (Logical scalar)
#' Whether or not to print verbose status reports.
#'
#' @param
#' The primary output is written to disk in the \code{output_dir} directory.
#'
#' @export
compute_viewpoints <- function(corpus,
                               parent_dir,
                               output_dir = file.path(parent_dir, "0-viewpoints"),
                               seq_test = seq_along(corpus),
                               viewpoints = hvr::hvr_viewpoints,
                               verbose = TRUE) {
  # Argument sanitising
  stopifnot(hrep::is.corpus(corpus))
  checkmate::qassert(output_dir, "S1")
  checkmate::qassert(seq_test, "X[1,)")
  stopifnot(!anyDuplicated(seq_test))
  seq_test <- sort(as.integer(seq_test))
  stopifnot(!any(seq_test > length(corpus)))
  stopifnot(all(purrr::map_lgl(viewpoints, is_viewpoint)))
  if (!hrep::type(corpus) == "pc_chord")
    stop("corpus must encoded using the pc_chord representation")

  R.utils::mkdirs(output_dir)
  saveRDS(corpus, file.path(output_dir, "corpus.rds"))

  saveRDS(
    list(
      corpus_size = length(corpus),
      seq_test = seq_test,
      seq_pretrain = setdiff(seq_along(corpus), seq_test),
      discrete_viewpoints = Filter(is_discrete, viewpoints) %>%
        purrr::map(~ .[c("name", "alphabet_size")]),
      continuous_viewpoints = Filter(Negate(is_discrete), viewpoints) %>% purrr::map_chr(name),
      viewpoint_labels = tibble(viewpoint = purrr::map_chr(viewpoints, "name"),
                                viewpoint_label = purrr::map_chr(viewpoints, "label"))
    ),
    file.path(output_dir, "about.rds")
  )

  compute_training_viewpoints(viewpoints,
                              verbose,
                              corpus,
                              output_dir)
  compute_test_only_viewpoints(seq_test,
                               viewpoints,
                               verbose,
                               corpus,
                               output_dir)
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
