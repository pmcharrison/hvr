viewpoint_regression <- function(
  parent_dir,
  max_sample = 1e4,
  sample_seed = 1,
  poly_degree = 3L,
  model_matrix_dir = file.path(parent_dir, "2-model-matrix"),
  output_dir = file.path(parent_dir, "3-viewpoint-regression"),
  viewpoints = yaml::read_yaml(file.path(model_matrix_dir, "about.yaml"))$viewpoints
) {
  checkmate::qassert(poly_degree, "X1[1,)")
  poly_degree <- as.integer(poly_degree)

  checkmate::qassert(max_sample, "X1[50,)")
  max_sample <- as.integer(max_sample)

  predictors <- get_regression_predictors(model_matrix_dir, viewpoints, poly_degree)
  corpus <- get_regression_corpus(model_matrix_dir, max_sample, sample_seed)
  model_matrix <- get_regression_model_matrix(corpus, model_matrix_dir, predictors)
  res <- conduct_regression(model_matrix, predictors)
  res
}

conduct_regression <- function(model_matrix, predictors) {

  observation_matrix <- model_matrix %>%
    dplyr::filter(observed) %>%
    dplyr::select(predictors$label) %>%
    as.matrix()

  continuation_matrices <- model_matrix %>%
    dplyr::group_by(seq_event_id) %>%
    dplyr::group_split() %>%
    purrr::map(dplyr::select, predictors$label) %>%
    purrr::map(as.matrix)


  message("Fitting conditional logit model...")

  browser()

  m <- predictors$label %>%
    # setdiff(., "ltm_pc_set") %>%
    # setdiff(., "stm_bass_pc") %>%
    # setdiff(., "ltm_bass_pc") %>%
    # setdiff(., "stm_pc_set") %>%
    # sprintf("scale(%s)", .) %>%
    paste(collapse = " + ") %>%
    sprintf("cbind(observed, seq_event_id) ~ %s", .) %>%
    as.formula() %>%
    mclogit::mclogit(data = model_matrix,
                     start = c(1, 1),  #rep(1, times = nrow(predictors)),
                     control = mclogit::mclogit.control(trace = TRUE))

  m
}

conduct_regression_old <- function(model_matrix, predictors) {

  message("Fitting conditional logit model...")

  browser()

  m <- predictors$label %>%
    # setdiff(., "ltm_pc_set") %>%
    # setdiff(., "stm_bass_pc") %>%
    # setdiff(., "ltm_bass_pc") %>%
    # setdiff(., "stm_pc_set") %>%
    # sprintf("scale(%s)", .) %>%
    paste(collapse = " + ") %>%
    sprintf("cbind(observed, seq_event_id) ~ %s", .) %>%
    as.formula() %>%
    mclogit::mclogit(data = model_matrix,
                     start = c(1, 1),  #rep(1, times = nrow(predictors)),
                     control = mclogit::mclogit.control(trace = TRUE))

  m
}

get_regression_predictors <- function(model_matrix_dir, viewpoints, poly_degree) {
  available <- readRDS(file.path(model_matrix_dir, "predictors.rds"))

  checkmate::qassert(viewpoints, "S[1,)")
  valid <- viewpoints %in% available$viewpoint
  if (any(!valid)) stop("unrecognised viewpoints: ",
                        paste(viewpoints[!valid], collapse = ", "),
                        "\n  available viewpoints: ",
                        paste(unique(available$viewpoint), collapse = ", "))
  available %>%
    dplyr::filter(.data$discrete | .data$poly_degree <= !!poly_degree) %>%
    dplyr::filter(viewpoint %in% viewpoints)
}

get_regression_model_matrix <- function(corpus, model_matrix_dir, predictors) {
  message("Loading model matrix...")
  raw <- readRDS(file.path(model_matrix_dir, "model-matrix.rds"))
  message("  Done.")
  dplyr::inner_join(raw,
                    dplyr::select(corpus, - .data$symbol),
                    by = c("seq_id", "event_id")) %>%
    dplyr::select(c("seq_event_id", "seq_id", "event_id", "symbol",
                    "observed", predictors$label))
}


get_regression_corpus <- function(model_matrix_dir, max_sample, sample_seed) {
  corpus <- readRDS(file.path(model_matrix_dir, "corpus.rds"))
  corpus$selected <- FALSE

  withr::with_seed(sample_seed, {
    ind <- sample(nrow(corpus),
                  pmin(nrow(corpus), max_sample),
                  replace = FALSE) %>% sort()
    corpus$selected[ind] <- TRUE
  })

  corpus %>%
    dplyr::filter(.data$selected) %>%
    dplyr::select(- .data$selected) %>%
    tibble::add_column(., seq_event_id = seq_len(nrow(.)), .before = 1)
}
