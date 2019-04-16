viewpoint_regression <- function(
  parent_dir,
  max_sample = 1e4,
  sample_seed = 1,
  poly_degree = 3L,
  viewpoint_dir = file.path(parent_dir, "0-viewpoints"),
  ppm_dir = file.path(parent_dir, "1-ppm"),
  output_dir = file.path(parent_dir, "2-regression"),
  viewpoints = list_viewpoints(viewpoint_dir),
  test_seq = list_test_seq(ppm_dir)
) {
  checkmate::qassert(poly_degree, "X1[1,)")
  corpus <- get_regression_corpus(viewpoint_dir, test_seq, max_sample, sample_seed)
  predictors <- get_regression_predictors(viewpoints, viewpoint_dir, ppm_dir, poly_degree)

  model_matrix <- get_model_matrix(
    get_continuous_model_matrix(corpus, predictors, viewpoint_dir),
    get_discrete_model_matrix(corpus, predictors, ppm_dir)
  )

  browser()
}


list_test_seq <- function(ppm_dir) {
  yaml::read_yaml(file.path(ppm_dir, "about.yaml"))$seq_test_folds %>%
    unlist() %>% sort()
}

get_continuous_model_matrix <- function(corpus, predictors, viewpoint_dir) {
  purrr::map_dfr(sort(unique(corpus$seq_id)), function(i) {
    corpus %>%
      dplyr::filter(.data$seq_id == i) %>%
      seq_continuous_model_matrix(predictors, viewpoint_dir)
  }) %>%
    add_polynomials(predictors)
}

get_discrete_model_matrix <- function(corpus, predictors, ppm_dir) {
  purrr::map_dfr(sort(unique(corpus$seq_id)), function(i) {
    corpus %>%
      dplyr::filter(.data$seq_id == i) %>%
      seq_discrete_model_matrix(predictors, ppm_dir)
  })
  browser()
}

seq_discrete_model_matrix <- function(events, predictors, ppm_dir) {
  browser()
  seq_id <- events$seq_id %>% unique()
  checkmate::qassert(seq_id, "X1")
  input <- readRDS(file.path(ppm_dir, "output", paste0(seq_id, ".rds")))
  refined <- input[]
  browser()
}

# seq_model_matrix <- function(events, predictors, viewpoint_dir, ppm_dir) {
#   dplyr::bind_cols(
#     events,
#     seq_discrete_model_matrix(events,
#                               predictors %>% dplyr::filter(.data$discrete),
#                               ppm_dir),
#     seq_continuous_model_matrix(events,
#                                 predictors %>% dplyr::filter(!.data$discrete),
#                                 viewpoint_dir)
#   )
#   browser()
# }

get_regression_predictors <- function(viewpoints, viewpoint_dir, ppm_dir, poly_degree) {
  discrete <- readr::read_csv(file.path(ppm_dir, "models.csv"), col_types = readr::cols()) %>%
    tibble::add_column(poly_degree = as.integer(NA), .after = 3L) %>%
    tibble::add_column(discrete = TRUE, .before = 1L) %>%
    dplyr::select(- .data$id)

  continuous <- yaml::read_yaml(file.path(viewpoint_dir, "about.yaml"))$continuous_viewpoints %>%
    purrr::map_dfr(~ tibble(discrete = FALSE,
                            class = as.character(NA),
                            viewpoint = .,
                            poly_degree = seq_len(poly_degree),
                            alphabet_size = as.integer(NA),
                            label = paste(viewpoint, "degree", poly_degree, sep = "_")))

  rbind(discrete, continuous) %>%
    dplyr::filter(.data$viewpoint %in% viewpoints)
}

list_viewpoints <- function(viewpoint_dir) {
  yaml <- yaml::read_yaml(file.path(viewpoint_dir, "about.yaml"))
  c(yaml$discrete %>% purrr::map_chr("name"),
    yaml$continuous) %>% sort()
}

get_regression_corpus <- function(viewpoint_dir, test_seq, max_sample, sample_seed) {
  corpus <- readRDS(file.path(viewpoint_dir, "corpus.rds")) %>%
    as.list() %>%
    purrr::map2_dfr(seq_along(.), ., ~ tibble(seq_id = .x,
                                              event_id = seq_along(.y),
                                              symbol = as.integer(.y),
                                              selected = FALSE)) %>%
    dplyr::filter(.data$seq_id %in% test_seq)

  withr::with_seed(sample_seed, {
    ind <- sample(nrow(corpus),
                  pmin(nrow(corpus), max_sample),
                  replace = FALSE) %>% sort()
    corpus$selected[ind] <- TRUE
  })

  corpus %>%
    dplyr::filter(.data$selected) %>%
    dplyr::select(- .data$selected)
}
