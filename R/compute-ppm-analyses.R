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
  seq_test = yaml::read_yaml(file.path(viewpoint_dir, "about.yaml"))$seq_test,
  seq_train_only = yaml::read_yaml(file.path(viewpoint_dir, "about.yaml"))$seq_train_only,
  viewpoints = yaml::read_yaml(file.path(viewpoint_dir, "about.yaml"))$discrete_viewpoints,
  folds = cross_validate(seq_test, folds = 1)
) {
  ppm_check_input(viewpoint_dir, output_dir, viewpoints, stm_opt, ltm_opt)
  ppm_write_yaml(stm_opt, ltm_opt, viewpoints, folds, output_dir)
  models <- ppm_identify_models(stm_opt, ltm_opt, viewpoints, output_dir)


}

ppm_check_input <- function(viewpoint_dir, output_dir, viewpoints,
                            stm_opt, ltm_opt) {
  checkmate::qassert(viewpoint_dir, "S1")
  if (!file.exists(viewpoint_dir)) stop("couldn't find ", viewpoint_dir)
  checkmate::qassert(output_dir, "S1")
  checkmate::qassert(viewpoints, "S")
  checkmate::qassert(stm_opt, "L")
  checkmate::qassert(ltm_opt, "L")
  stopifnot(identical(names(stm_opt), names(stm_options())))
  stopifnot(identical(names(ltm_opt), names(ltm_options())))
}

ppm_write_yaml <- function(stm_opt, ltm_opt, viewpoints, folds, output_dir) {
  R.utils::mkdirs(output_dir)
  list(stm_options = stm_opt,
       ltm_options = ltm_opt,
       viewpoints = viewpoints,
       folds = folds) %>%
    yaml::write_yaml(file.path(output_dir, "about.yaml"))
}

ppm_identify_models <- function(stm_opt, ltm_opt, viewpoints, output_dir) {
  model_classes <- c(if (stm_opt$enabled) "stm",
                     if (ltm_opt$enabled) "ltm")
  models <- purrr::map_dfr(viewpoints, ~ tibble(class = model_classes,
                                                viewpoint = .)) %>%
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

#' @export
cross_validate <- function(x, folds, seed = 1) {
  checkmate::qassert(x, "X")
  checkmate::qassert(folds, "X1")
  stopifnot(!anyDuplicated(x))

  withr::with_seed(seed, {
    if (folds == 1) {
      list(sort(x))
    } else {
      stop("sorry, cross-validation has not yet been implemented")
    }
  })
}
