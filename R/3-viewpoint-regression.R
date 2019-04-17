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

  c(continuous_model_matrix, poly_coefs) %<-%
    get_continuous_model_matrix(corpus, predictors, viewpoint_dir, poly_degree)

  model_matrix <- get_model_matrix(
    continuous_model_matrix,
    get_discrete_model_matrix(corpus, predictors, ppm_dir)
  )

  browser()
}


list_test_seq <- function(ppm_dir) {
  yaml::read_yaml(file.path(ppm_dir, "about.yaml"))$seq_test_folds %>%
    unlist() %>% sort()
}

get_model_matrix <- function(continuous_model_matrix,
                             discrete_model_matrix) {
  id_vars <- c("seq_id", "event_id", "symbol")
  stopifnot(identical(
    as.data.frame(continuous_model_matrix[, id_vars]),
    as.data.frame(discrete_model_matrix[, id_vars])
  ))
  dplyr::bind_cols(
    continuous_model_matrix,
    dplyr::select(discrete_model_matrix, - id_vars)
  )
}

get_continuous_model_matrix <- function(corpus, predictors, viewpoint_dir, poly_degree) {
  purrr::map_dfr(sort(unique(corpus$seq_id)), function(i) {
    corpus %>%
      dplyr::filter(.data$seq_id == i) %>%
      seq_continuous_model_matrix(predictors, viewpoint_dir)
  }) %>%
    add_polynomials(predictors, poly_degree)
}

seq_continuous_model_matrix <- function(events, predictors, viewpoint_dir) {
  viewpoints <- predictors %>%
    dplyr::filter(!.data$discrete) %>%
    dplyr::pull(.data$viewpoint) %>%
    unique()
  seq_id <- unique(events$seq_id)
  stopifnot(!anyDuplicated(viewpoints),
            length(seq_id) == 1)
  raw <- readRDS(file.path(viewpoint_dir,
                           "viewpoints-test",
                           paste0(seq_id, ".rds")))$continuous
  purrr::pmap_dfr(events, function(seq_id, event_id, symbol) {
    dplyr::bind_cols(
      tibble(
        seq_id,
        event_id,
        symbol = seq_len(hrep::alphabet_size("pc_chord")),
        observed = symbol == !!symbol
      ),
      t(raw[viewpoints, event_id, ]) %>%
        tibble::as_tibble() %>%
        dplyr::mutate_all(~ dplyr::if_else(is.na(.), 0, .))
    )
  })
}

add_polynomials <- function(continuous_model_matrix, predictors, poly_degree) {
  viewpoints <- predictors %>%
    dplyr::filter(!.data$discrete) %>%
    dplyr::pull(.data$viewpoint) %>%
    unique() %>%
    magrittr::set_names(., .)
  res <- purrr::map(viewpoints, function(v) {
    raw <- stats::poly(continuous_model_matrix[[v]], degree = poly_degree)
    vals <- tibble::as_tibble(raw)
    names(vals) <- purrr::map_chr(seq_len(poly_degree), function(i) {
      predictors %>%
        dplyr::filter(.data$viewpoint == v & .data$poly_degree == i) %>%
        dplyr::pull(.data$label)
    })
    coefs <- attr(raw, "coefs")
    list(vals = vals, coefs = coefs)
  })
  vals <- purrr::map(res, "vals")
  list(
    continuous_model_matrix = dplyr::bind_cols(continuous_model_matrix, vals),
    coefs = purrr::map(res, "coefs")
  )
}

get_discrete_model_matrix <- function(corpus, predictors, ppm_dir) {
  purrr::map_dfr(sort(unique(corpus$seq_id)), function(i) {
    corpus %>%
      dplyr::filter(.data$seq_id == i) %>%
      seq_discrete_model_matrix(predictors, ppm_dir)
  })
}

seq_discrete_model_matrix <- function(events, predictors, ppm_dir) {
  models <- predictors %>%
    dplyr::filter(.data$discrete) %>%
    dplyr::pull(.data$label) %>%
    unique()
  seq_id <- unique(events$seq_id)
  stopifnot(!anyDuplicated(models),
            length(seq_id) == 1)
  raw <- readRDS(file.path(ppm_dir, "output", paste0(seq_id, ".rds")))

  purrr::pmap_dfr(events, function(seq_id, event_id, symbol) {
    cbind(
      seq_id, event_id,
      symbol = seq_len(hrep::alphabet_size("pc_chord")),
      t(raw[models, event_id, ]) %>%
        tibble::as_tibble() %>%
        dplyr::mutate_all(~ dplyr::if_else(is.na(.), 1, .)) %>%
        dplyr::mutate_all(~ - log2(.))
    ) %>% tibble::as_tibble()
  })
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
