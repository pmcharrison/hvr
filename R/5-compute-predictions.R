#' Compute predictions
#'
#' Computes predictions from a trained viewpoint regression model.
#' The following routines will typically be run first:
#' 1. \code{\link{compute_viewpoints}}
#' 2. \code{\link{compute_ppm_analyses}}
#' 3. \code{\link{compute_model_matrix}}
#' 4. \code{\link{viewpoint_regression}}
#'
#' @param weights
#' If \code{NULL} (default),
#' the regression weights are extracted from the viewpoint regression model
#' output in \code{regression_dir}.
#' Alternatively, the weights can be manually specified as a named numeric vector
#' where the names correspond to the \code{label} column of the
#' \code{\link[tibble]{tibble}} stored in \code{predictors.rds}
#' in \code{model_matrix_dir}.
#'
#' @param regression_dir
#' (Character scalar)
#' The directory for the already-generated
#' output files from \code{\link{viewpoint_regression}}.
#' The default should be correct if the user used the
#' default \code{dir} argument in \code{\link{viewpoint_regression}}.
#'
#' @inheritParams viewpoint_regression
#'
#' @return
#' A \code{\link[tibble]{tibble}} with one row for each event in the
#' model matrix, with the following columns:
#' - \code{seq_id} - Index of the sequence within the corpus
#' (see \code{\link{compute_viewpoints}}).
#' - \code{event_id} - Index of the chord within the sequence.
#' - \code{chord_id} - Encoded chord symbol
#' (see \code{\link[hrep]{pc_chord}}, \code{\link[hrep]{encode}}).
#' - \code{chord} - Decoded chord symbol.
#' - \code{probability} - Probability of the observed chord.
#' - \code{information_content} - Information content of the observed chord.
#'
#' @md
#'
#' @export
compute_predictions <- function(
  parent_dir,
  weights = NULL,
  model_matrix_dir = file.path(parent_dir, "2-model-matrix"),
  regression_dir = file.path(parent_dir, "3-viewpoint-regression"),
  output_dir = file.path(parent_dir, "4-predictions")) {

  checkmate::qassert(model_matrix_dir, "S1")
  checkmate::qassert(regression_dir, "S1")
  checkmate::qassert(output_dir, "S1")

  R.utils::mkdirs(output_dir)

  if (is.null(weights))
    weights <- readRDS(file.path(regression_dir, "results.rds"))$par

  check_weights(weights, model_matrix_dir)

  observation_matrix <- readRDS(file.path(model_matrix_dir, "observation-matrix.rds"))
  continuation_matrices <- readRDS(file.path(model_matrix_dir, "continuation-matrices.rds"))
  legal <- readRDS(file.path(model_matrix_dir, "legal.rds"))

  probs <- event_probs(weights = weights,
                       observation_matrix = observation_matrix,
                       continuation_matrices = continuation_matrices,
                       legal = legal)

  corpus <- readRDS(file.path(model_matrix_dir, "corpus.rds"))

  stopifnot(length(probs) == nrow(corpus))

  res <- corpus %>%
    dplyr::select(c("seq_id", "event_id", "symbol")) %>%
    dplyr::rename(chord_id = .data$symbol) %>%
    dplyr::mutate(chord = hrep::decode(chord_id, "pc_chord"),
                  probability = probs,
                  information_content = - log2(probs))

  res %>% saveRDS(file.path(output_dir, "predictions.rds"))

  res %>%
    dplyr::mutate(chord = purrr::map_chr(.data$chord, as.character)) %>%
    readr::write_csv(file.path(output_dir, "predictions.csv"))

  res
}

check_weights <- function(weights, model_matrix_dir) {
  predictors <- readRDS(file.path(model_matrix_dir, "predictors.rds"))
  if (!isTRUE(all.equal(names(weights), predictors$label)))
    stop("weights were expected as a numeric vector ",
         "with the following names: \n  ",
         paste(predictors$label, collapse = ", "), ".\n",
         "Check that you specified the viewpoints and the weights correctly.")
}
