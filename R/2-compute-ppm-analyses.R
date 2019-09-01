#' Compute PPM analyses
#'
#' This function models discrete viewpoints using the
#' Prediction by Partial Match (PPM) algorithm.
#'
#' \code{\link{compute_viewpoints}} should be run first.
#' By default, only sequences in
#' \code{seq_test} (see \code{\link{compute_viewpoints}})
#' are modelled using PPM.
#' The default PPM implementation corresponds to that described in
#' \insertCite{Pearce2005;textual}{hvr}.
#'
#' @param parent_dir
#' (Character scalar)
#' The parent directory for the output files, shared with functions such as
#' \code{\link{compute_viewpoints}} and \code{\link{compute_model_matrix}}.
#' Ignored if all other directory arguments are manually specified.
#'
#' @param viewpoint_dir
#' (Character scalar)
#' The directory for the already-generated
#' output files from \code{\link{compute_viewpoints}}.
#' The default should be correct if the user used the
#' default \code{dir} argument in \code{\link{compute_viewpoints}}.
#'
#' @param output_dir
#' (Character scalar)
#' The output directory for the PPM analyses.
#' Will be created if it doesn't exist already.
#'
#' @param stm_opt
#' Options list for the short-term PPM models, as created by the function
#' \code{\link{stm_options}}.
#'
#' @param ltm_opt
#' Options list for the long-term PPM models, as created by the function
#' \code{\link{ltm_options}}.
#'
#' @param seq_test_folds
#' List of cross-validation folds for the test sequences.
#' Each fold is represented as an integer vector,
#' with the integers indexing the sequences within the corpus
#' (see \code{\link{compute_viewpoints}}).
#' The algorithm iterates over each fold, predicting the sequences
#' within that fold, and training the model using the combination of
#' a) the sequences from the other folds in \code{seq_test_folds} and
#' b) the sequences identified in \code{seq_pretrain}.
#' By default, there is just one fold corresponding to the \code{seq_test}
#' argument of \code{\link{compute_viewpoints}}.
#'
#' @param seq_pretrain
#' (Integer vector)
#' Sequences used to pretrain the model (in addition to any cross-validation training
#' specified by \code{seq_test_folds}), specified as integer indices
#' of the corpus.
#' Defaults to the complement of \code{seq_test} as specified in
#' \code{\link{compute_viewpoints}}.
#'
#' @param viewpoints
#' List of discrete viewpoints to analyse,
#' in the format produced by the \code{$discrete_viewpoints} slot
#' of the \code{about.rds} file produced by \code{\link{compute_viewpoints}}.
#' Defaults to the full set of discrete viewpoints as specified
#' in \code{\link{compute_viewpoints}}.
#'
#' @return
#' The primary output is written to disk in the \code{dir} directory.
#' The output matrices provide raw probabilities for each event in the
#' chord alphabet.
#'
#' @md
#'
#' @export
compute_ppm_analyses <- function(
  parent_dir,
  viewpoint_dir = file.path(parent_dir, "0-viewpoints"),
  output_dir = file.path(parent_dir, "1-ppm"),
  stm_opt = stm_options(),
  ltm_opt = ltm_options(),
  seq_test_folds = list(readRDS(file.path(viewpoint_dir, "about.rds"))$seq_test),
  seq_pretrain = readRDS(file.path(viewpoint_dir, "about.rds"))$seq_pretrain,
  viewpoints = readRDS(file.path(viewpoint_dir, "about.rds"))$discrete_viewpoints
) {
  ppm_check_input(viewpoint_dir, output_dir, stm_opt, ltm_opt,
                  seq_test_folds, seq_pretrain, viewpoints)
  ppm_write_about(output_dir, stm_opt, ltm_opt, seq_test_folds, seq_pretrain, viewpoints)
  model_spec <- ppm_identify_models(stm_opt, ltm_opt, viewpoints, output_dir)
  viewpoints_observed <- readRDS(file.path(viewpoint_dir, "viewpoints-training.rds")) %>%
    purrr::map("discrete")
  purrr::map(seq_along(seq_test_folds),
             ppm_fold,
             folds = seq_test_folds,
             seq_pretrain = seq_pretrain,
             model_spec = model_spec,
             viewpoints = viewpoints,
             viewpoint_dir = viewpoint_dir,
             viewpoints_observed = viewpoints_observed,
             stm_opt = stm_opt,
             ltm_opt = ltm_opt,
             output_dir = output_dir)
  invisible()
}

ppm_fold <- function(i, folds, seq_pretrain, model_spec,
                     viewpoints,
                     viewpoint_dir,
                     viewpoints_observed,
                     stm_opt, ltm_opt, output_dir) {
  message("Computing PPM analyses for fold ", i, " out of ", length(folds), "...")

  training <- c(get_training(folds, i), seq_pretrain) %>% sort()
  seq_test <- get_test(folds, i)

  message("  Pretraining long-term models...")
  ltm_models <- if (ltm_opt$enabled) {
    viewpoints %>%
      magrittr::set_names(., purrr::map_chr(., "name")) %>%
      plyr::llply(ppm_train,
                  training = training,
                  viewpoints_observed = viewpoints_observed,
                  ltm_opt = ltm_opt,
                  .progress = "text")
  }

  purrr::map2(seq_test,
              seq_along(seq_test),
              ppm_test,
              num_seq = length(seq_test),
              model_spec = model_spec,
              viewpoints = viewpoints,
              viewpoint_dir = viewpoint_dir,
              viewpoints_observed = viewpoints_observed,
              ltm_models = ltm_models,
              stm_opt = stm_opt,
              output_dir = output_dir)
}

ppm_test <- function(seq_id, i, num_seq, model_spec,
                     viewpoints, viewpoint_dir, viewpoints_observed,
                     ltm_models, stm_opt, output_dir) {
  stopifnot(length(viewpoints) == length(ltm_models),
            length(viewpoints) == 0L ||
              all(purrr::map_chr(viewpoints, "name") == names(ltm_models)))
  file <- file.path(viewpoint_dir, "viewpoints-test", paste0(seq_id, ".rds"))
  if (!file.exists(file)) stop("failed to find viewpoint file ", file)
  viewpoints_continuations <- readRDS(file)$discrete

  num_events <- dim(viewpoints_continuations)[2]

  res <- array(dim = c(nrow(model_spec),
                       num_events,
                       hrep::alphabet_size("pc_chord")),
               data = as.numeric(NA))
  dimnames(res) <- list(if (nrow(model_spec) > 0) model_spec$label else character())
  class(res) <- c("ppm_output", "array")

  message("  Predicting test sequence ", i, " out of ", num_seq, "...")
  plyr::m_ply(model_spec, function(id, class, viewpoint, alphabet_size, label) {
    res[id, , ] <<- ppm_test_viewpoint(
      viewpoint_observed = viewpoints_observed[[seq_id]][viewpoint, ],
      viewpoint_continuations = viewpoints_continuations[viewpoint, , ],
      mod = if (class == "ltm") ltm_models[[viewpoint]] else
        init_ppm_mod(alphabet_size, stm_opt),
      mod_class = class
    )
  }, .progress = "text")

  R.utils::mkdirs(file.path(output_dir, "output"))
  saveRDS(res, file.path(output_dir, "output", paste0(seq_id, ".rds")))
}

ppm_test_viewpoint <- function(viewpoint_observed,
                               viewpoint_continuations,
                               mod,
                               mod_class) {
  stopifnot(mod_class %in% c("stm", "ltm"))
  train <- mod_class == "stm"
  not_na <- which(!is.na(viewpoint_observed))
  viewpoint_predictions <- ppm::model_seq(model = mod,
                                          seq = viewpoint_observed[not_na],
                                          train = train)$distribution
  projected_predictions <- matrix(data = as.numeric(NA),
                                  nrow = nrow(viewpoint_continuations),
                                  ncol = ncol(viewpoint_continuations))
  purrr::map2(seq_along(not_na), not_na, function(prediction_id, event_id) {
    projected_predictions[event_id, ] <<- project_to_basic(
      viewpoint_predictions[[prediction_id]],
      viewpoint_continuations[event_id, ]
    )})
  projected_predictions
}

project_to_basic <- function(vp_dist, vp_vals) {
  checkmate::qassert(vp_dist, "N")
  checkmate::qassert(vp_vals, "X[1,)")
  if (any(vp_vals > length(vp_dist))) stop("invalid viewpoint value")
  vp_counts <- tabulate(vp_vals, nbins = length(vp_dist))
  weights <- (vp_dist / vp_counts)[vp_vals]
  s <- sum(weights)
  if (s == 0) stop("no available viewpoint values had non-zero probabilities")
  weights / s
}

#' @export
print.ppm_output <- function(x, ...) {
  cat("PPM output array:\n",
      "  ",
      dim(x)[1], " (viewpoint models) x ",
      dim(x)[2], " (observed events) x ",
      dim(x)[3], " (possible continuations).\n",
      sep = "")
}

ppm_train <- function(viewpoint, training, viewpoints_observed, ltm_opt) {
  checkmate::qassert(training, "X")
  dat <- viewpoints_observed[training] %>%
    purrr::map(~ .[viewpoint$name, ]) %>%
    purrr::map(na.omit)
  mod <- init_ppm_mod(viewpoint$alphabet_size, ltm_opt)
  purrr::map(dat, ~ ppm::model_seq(model = mod,
                                   seq = .,
                                   train = TRUE,
                                   predict = FALSE))
  mod
}

init_ppm_mod <- function(alphabet_size, opt) {
  ppm::new_ppm_simple(alphabet_size = alphabet_size,
                      order_bound = opt$order_bound,
                      shortest_deterministic = opt$shortest_deterministic,
                      exclusion = opt$exclusion,
                      update_exclusion = opt$update_exclusion,
                      escape = opt$escape)
}

get_training <- function(folds, i) {
  sort(unlist(folds[- i]))
}

get_test <- function(folds, i) {
  sort(folds[[i]])
}

ppm_check_input <- function(viewpoint_dir, output_dir, stm_opt, ltm_opt,
                            seq_test_folds, seq_pretrain, viewpoints) {
  checkmate::qassert(viewpoint_dir, "S1")
  if (!file.exists(viewpoint_dir)) stop("couldn't find ", viewpoint_dir)
  checkmate::qassert(output_dir, "S1")
  checkmate::qassert(viewpoints, "L")
  checkmate::qassert(stm_opt, "L")
  checkmate::qassert(ltm_opt, "L")
  stopifnot(identical(names(stm_opt), names(stm_options())))
  stopifnot(identical(names(ltm_opt), names(ltm_options())))
  if (!is.list(seq_test_folds) && purrr::map_lgl(seq_test_folds, checkmate::qtest, "X"))
    stop("'seq_test_folds' must be a list of integer vectors, ",
         "each corresponding to a different cross-validation fold")
  checkmate::qassert(seq_pretrain, "X")
}

ppm_write_about <- function(output_dir, stm_opt, ltm_opt,
                           seq_test_folds, seq_pretrain,
                           viewpoints) {
  R.utils::mkdirs(output_dir)
  list(stm_options = stm_opt,
       ltm_options = ltm_opt,
       seq_test_folds = seq_test_folds,
       seq_pretrain = seq_pretrain,
       viewpoints = viewpoints) %>%
    saveRDS(file.path(output_dir, "about.rds"))
}

ppm_identify_models <- function(stm_opt, ltm_opt, viewpoints, output_dir) {
  model_classes <- c(if (stm_opt$enabled) "stm",
                     if (ltm_opt$enabled) "ltm")
  models <- tibble(class = character(),
                   viewpoint = character(),
                   alphabet_size = integer(),
                   label = character()) %>%
    dplyr::bind_rows(
      purrr::map_dfr(viewpoints,
                     ~ tibble(class = model_classes,
                              viewpoint = .$name,
                              alphabet_size = .$alphabet_size,
                              label = paste(class, viewpoint, sep = "_")))
    ) %>%
    tibble::add_column(., id = seq_len(nrow(.)), .before = 1L)
  write.csv(models, file.path(output_dir, "models.csv"), row.names = FALSE)
  models
}

#' @export
ltm_options <- function(enabled = TRUE,
                        order_bound = 10L,
                        shortest_deterministic = TRUE,
                        exclusion = TRUE,
                        update_exclusion = FALSE,
                        escape = "c") {
  as.list(environment())
}

#' @export
stm_options <- function(enabled = TRUE,
                        order_bound = 10L,
                        shortest_deterministic = TRUE,
                        exclusion = TRUE,
                        update_exclusion = TRUE,
                        escape = "ax") {
  as.list(environment())
}

# @export
# cross_validate <- function(x, folds, seed = 1) {
#   checkmate::qassert(x, "X")
#   checkmate::qassert(folds, "X1")
#   stopifnot(!anyDuplicated(x))
#
#   withr::with_seed(seed, {
#     if (folds == 1) {
#       list(sort(x))
#     } else {
#       stop("sorry, cross-validation has not yet been implemented")
#     }
#   })
# }
