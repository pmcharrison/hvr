#' Model dataset
#'
#' Analyses a dataset of chord sequences by constructing and optimising
#' a viewpoint regression model,
#' and using this model to generate predictions for these sequences.
#'
#' @details
#' This function wraps the following sub-routines:
#' 1. \code{\link{compute_viewpoints}}
#' 2. \code{\link{compute_ppm_analyses}}
#' 3. \code{\link{compute_model_matrix}}
#' 4. \code{\link{viewpoint_regression}}
#' 5. \code{\link{compute_predictions}}
#'
#' Users may wish to use these sub-routines explicitly if performing
#' repeated analyses with different parameter settings,
#' to save redundant computation.
#'
#' @param corpus_test
#' Corpus of chord sequences to predict,
#' as created by \code{\link[hrep]{corpus}}.
#'
#' @param corpus_pretrain
#' Corpus of chord sequences with which to pretrain the model,
#' as created by \code{\link[hrep]{corpus}}.
#' These chord sequences are used solely to pretrain
#' the discrete viewpoint models;
#' continuous viewpoint effects and discrete viewpoint weights
#' are optimised on \code{corpus_test}.
#'
#' @param output_dir
#' (Character scalar)
#' Directory in which to save the model outputs.
#'
#' @param corpus_test_folds
#' List of cross-validation folds for applying discrete viewpoint models to
#' the sequences in \code{corpus_test}.
#' Each list element should be an integer vector indexing into \code{corpus_test}.
#' These integer vectors must exhaustively partition the sequences
#' in \code{corpus_test}.
#' The algorithm iterates over each fold, predicting the sequences
#' within that fold, and training the model using the combination of
#' a) the sequences from the other folds in \code{corpus_test_folds} and
#' b) the sequences in \code{corpus_pretrain}.
#' By default, there is just one fold corresponding to the
#' entire of \code{corpus_test}, meaning that no cross-validation is applied.
#'
#' @inheritParams compute_viewpoints
#' @inheritParams compute_ppm_analyses
#' @inheritParams compute_model_matrix
#' @inheritParams viewpoint_regression
#'
#' @return
#' Various model outputs are saved to \code{output_dir}.
#' The function returns a \code{\link[tibble]{tibble}} of predicted probabilities
#' for the chords in \code{corpus_test}; see
#' \code{\link{compute_predictions}} for an explanation of this \code{tibble}.
#'
#' @md
#'
#' @export
model_dataset <- function(
  corpus_test,
  corpus_pretrain,
  output_dir,
  viewpoints = hvr::hvr_viewpoints,
  weights = NULL,
  poly_degree = 4L,
  max_iter = 500,
  corpus_test_folds = list(seq_along(corpus_test)),
  allow_repeats = FALSE,
  max_sample = Inf,
  sample_seed = 1,
  stm_opt = stm_options(),
  ltm_opt = ltm_options(),
  na_val = 0,
  perm_int = TRUE,
  perm_int_seed = 1,
  perm_int_reps = 5,
  allow_negative_weights = FALSE
) {
  check_corpus_test_folds(corpus_test_folds, corpus_test)

  corpus <- c(corpus_test, corpus_pretrain)
  seq_test <- seq_along(corpus_test)
  seq_pretrain <- seq(from = length(seq_test) + 1,
                      length.out = length(corpus_pretrain))

  message("1/5: Computing viewpoints...")
  compute_viewpoints(corpus = corpus,
                     parent_dir = output_dir,
                     seq_test = seq_along(corpus_test),
                     viewpoints = viewpoints)

  message("\n2/5: Computing PPM analyses...")
  compute_ppm_analyses(parent_dir = output_dir,
                       stm_opt = stm_opt,
                       ltm_opt = ltm_opt,
                       seq_test_folds = corpus_test_folds,
                       seq_pretrain = seq_pretrain)

  message("\n3/5: Constructing model matrix...")
  compute_model_matrix(parent_dir = output_dir,
                       max_sample = max_sample,
                       sample_seed = sample_seed,
                       poly_degree = poly_degree,
                       na_val = na_val,
                       allow_repeats = allow_repeats)

  if (is.null(weights)) {
    message("\n4/5: Fitting viewpoint regression model...")
    viewpoint_regression(parent_dir = output_dir,
                         max_iter = max_iter,
                         perm_int = perm_int,
                         perm_int_seed = perm_int_seed,
                         perm_int_reps = perm_int_reps,
                         allow_negative_weights = allow_negative_weights)
  } else {
    message("\n4/5: Skipping regression model fitting ",
            "and using prespecified weights...")
  }

  message("\n5/5: Generating final predictions...")
  compute_predictions(parent_dir = output_dir, weights = weights)
}

check_corpus_test_folds <- function(corpus_test_folds, corpus_test) {
  if (!all(purrr::map_lgl(corpus_test_folds, checkmate::qtest, "X")))
    stop("all elements of corpus_test_folds must be integer vectors")

  if (!isTRUE(all.equal(sort(unlist(corpus_test_folds)),
                        seq_along(corpus_test))))
    stop("corpus_test_folds must exhaustively partition corpus_test")
}
