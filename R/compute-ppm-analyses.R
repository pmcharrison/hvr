#' Compute PPM analyses
#'
#' This function computes PPM analyses.
#' @export
compute_ppm_analyses <- function(
  parent_dir,
  viewpoint_dir = file.path(parent_dir, "0-viewpoints"),
  output_dir = file.path(parent_dir, "1-ppm"),
  stm_opt = stm_options(),
  ltm_opt = ltm_options(),
  seq_test_folds = list(yaml::read_yaml(file.path(viewpoint_dir, "about.yaml"))$seq_test),
  seq_pretrain = yaml::read_yaml(file.path(viewpoint_dir, "about.yaml"))$seq_pretrain,
  viewpoints = yaml::read_yaml(file.path(viewpoint_dir, "about.yaml"))$discrete_viewpoints
) {
  ppm_check_input(viewpoint_dir, output_dir, stm_opt, ltm_opt,
                  seq_test_folds, seq_pretrain, viewpoints)
  ppm_write_yaml(output_dir, stm_opt, ltm_opt, seq_test_folds, seq_pretrain, viewpoints)
  model_spec <- ppm_identify_models(stm_opt, ltm_opt, viewpoints, output_dir)
  purrr::map(seq_along(seq_test_folds),
             ppm_fold,
             folds = seq_test_folds,
             seq_pretrain = seq_pretrain,
             model_spec = model_spec,
             viewpoints = viewpoints,
             viewpoint_dir = viewpoint_dir,
             stm_opt = stm_opt,
             ltm_opt = ltm_opt,
             output_dir = output_dir)
}

ppm_fold <- function(i, folds, seq_pretrain, model_spec, viewpoints, viewpoint_dir,
                     stm_opt, ltm_opt, output_dir) {
  message("Computing PPM analyses for fold ", i, " out of ", length(folds), "...")

  training <- c(get_training(folds, i), seq_pretrain) %>% sort()
  test <- get_test(folds, i)

  message("  Pretraining long-term models...")
  ltm_models <- if (ltm_opt$enabled) {
    viewpoints %>%
      magrittr::set_names(., purrr::map_chr(., "name")) %>%
      plyr::llply(ppm_train,
                  training = training,
                  viewpoint_dir = viewpoint_dir,
                  ltm_opt = ltm_opt,
                  .progress = "text")
  }

  message("  Predicting test sequences...")
  test %>%
    plyr::l_ply(ppm_test,
                model_spec = model_spec,
                viewpoints = viewpoints,
                viewpoint_dir = viewpoint_dir,
                ltm_models = ltm_models,
                stm_opt = stm_opt,
                output_dir = output_dir,
                .progress = "text")
}

ppm_test <- function(seq_id, model_spec,
                     viewpoints, viewpoint_dir,
                     ltm_models, stm_opt, output_dir) {
  stopifnot(length(viewpoints) == length(ltm_models),
            identical(purrr::map_chr(viewpoints, "name"), names(ltm_models)))

  file <- file.path(viewpoint_dir, "viewpoints-test", paste0(seq_id, ".rds"))
  if (!file.exists(file)) stop("failed to find viewpoint file ", file)
  dat <- readRDS(file)$discrete

  num_events <- dim(dat)[2]

  res <- array(dim = c(nrow(model_spec),
                       num_events,
                       hrep::alphabet_size("pc_chord")),
               data = as.numeric(NA))
  dimnames(res) <- list(model_spec$label)
  class(res) <- c("ppm_output", "array")

  purrr::pmap(model_spec, function(id, class, viewpoint, alphabet_size, label) {
    res[i, , ] <<- ppm_test_viewpoint(
      viewpoint_values = dat[viewpoint, , ],
      viewpoint_alphabet_size = alphabet_size,
      mod_class = class,
      ltm_mod = ltm_models[[viewpoint]]
    )
  })
}

ppm_test_viewpoint <- function(viewpoint_values,
                               viewpoint_alphabet_size,
                               mod_class,
                               ltm_mod) {
  stopifnot(mod_class %in% c("stm", "ltm"))
  browser()

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

ppm_train <- function(viewpoint, training, viewpoint_dir, ltm_opt) {
  checkmate::qassert(training, "X")
  dat <- readRDS(file.path(viewpoint_dir, "viewpoints-training.rds")) %>%
    purrr::map("discrete") %>%
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

ppm_write_yaml <- function(output_dir, stm_opt, ltm_opt,
                           seq_test_folds, seq_pretrain,
                           viewpoints) {
  R.utils::mkdirs(output_dir)
  list(stm_options = stm_opt,
       ltm_options = ltm_opt,
       seq_test_folds = seq_test_folds,
       seq_pretrain = seq_pretrain,
       viewpoints = viewpoints) %>%
    yaml::write_yaml(file.path(output_dir, "about.yaml"))
}

ppm_identify_models <- function(stm_opt, ltm_opt, viewpoints, output_dir) {
  model_classes <- c(if (stm_opt$enabled) "stm",
                     if (ltm_opt$enabled) "ltm")
  models <- purrr::map_dfr(viewpoints,
                           ~ tibble(class = model_classes,
                                    viewpoint = .$name,
                                    alphabet_size = .$alphabet_size,
                                    label = paste(class, viewpoint, sep = "_"))) %>%
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
                        escape = "x") {
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
