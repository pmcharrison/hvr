#' @export
compute_model_matrix <- function(
  parent_dir,
  max_sample = 1e4,
  sample_seed = 1,
  poly_degree = 4L,
  na_val = 0,
  viewpoint_dir = file.path(parent_dir, "0-viewpoints"),
  ppm_dir = file.path(parent_dir, "1-ppm"),
  output_dir = file.path(parent_dir, "2-model-matrix"),
  viewpoints = list_viewpoints(viewpoint_dir),
  test_seq = list_test_seq(ppm_dir),
  allow_repeats = FALSE
) {
  checkmate::qassert(max_sample, "X1[50,)")
  checkmate::qassert(na_val, "N1(,)")
  max_sample <- as.integer(max_sample)
  checkmate::qassert(poly_degree, "X1[1,)")
  checkmate::qassert(allow_repeats, "B1")
  corpus <- get_model_matrix_corpus(viewpoint_dir, test_seq, max_sample, sample_seed)
  predictors <- get_model_matrix_predictors(viewpoints, viewpoint_dir, ppm_dir, poly_degree)

  R.utils::mkdirs(output_dir)
  write_model_matrix_about(max_sample, sample_seed, poly_degree, viewpoints,
                           output_dir)

  tmp <- get_continuous_model_matrix(corpus, predictors, viewpoint_dir,
                                     poly_degree, na_val, allow_repeats)

  model_matrix <- get_model_matrix(
    tmp$continuous_model_matrix,
    get_discrete_model_matrix(corpus, predictors, ppm_dir, na_val),
    predictors
  )

  check_model_matrix(model_matrix)

  message("Saving outputs...")
  saveRDS(tmp$moments, file.path(output_dir, "moments.rds"))
  saveRDS(tmp$poly_coefs, file.path(output_dir, "poly-coefs.rds"))
  saveRDS(corpus, file.path(output_dir, "corpus.rds"))
  saveRDS(predictors, file.path(output_dir, "predictors.rds"))
  saveRDS(model_matrix, file.path(output_dir, "model-matrix.rds"))
}

check_model_matrix <- function(model_matrix) {
  if (any(model_matrix$observed & !model_matrix$legal)) {
    stop("Observed illegal events. Did you forget to ",
         "set 'allow_repeats' to TRUE?")
  }
}

get_moments <- function(model_matrix, predictors) {
  message("Computing moments...")
  obs <- model_matrix %>% dplyr::filter(.data$observed)
  viewpoints <- predictors %>%
    dplyr::filter(!.data$discrete) %>%
    dplyr::pull(.data$viewpoint) %>%
    unique() %>% sort()
  purrr::map_dfr(viewpoints, function(v) {
    tibble(viewpoint = v,
           mean = mean(obs[[v]], na.rm = TRUE),
           sd = sd(obs[[v]], na.rm = TRUE))
  })
}

write_model_matrix_about <- function(max_sample, sample_seed, poly_degree, viewpoints,
                                     output_dir) {
  list(
    max_sample = max_sample,
    sample_seed = sample_seed,
    poly_degree = poly_degree,
    viewpoints = viewpoints
  ) %>%
    saveRDS(file.path(output_dir, "about.rds"))
}


list_test_seq <- function(ppm_dir) {
  readRDS(file.path(ppm_dir, "about.rds"))$seq_test_folds %>%
    unlist() %>% sort()
}

get_model_matrix <- function(continuous_model_matrix,
                             discrete_model_matrix,
                             predictors) {
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

get_continuous_model_matrix <- function(corpus, predictors, viewpoint_dir, poly_degree, na_val, allow_repeats) {
  message("Getting model matrix for continuous viewpoints...")
  raw <- plyr::llply(sort(unique(corpus$seq_id)), function(i) {
    corpus %>%
      dplyr::filter(.data$seq_id == i) %>%
      seq_continuous_model_matrix(predictors, viewpoint_dir, allow_repeats)
  }, .progress = "text") %>%
    dplyr::bind_rows()

  tmp <- add_polynomials(raw, predictors, poly_degree, na_val)
  list(
    continuous_model_matrix = tmp$continuous_model_matrix,
    poly_coefs = tmp$poly_coefs,
    moments = get_moments(raw, predictors)
  )
}

seq_continuous_model_matrix <- function(events, predictors, viewpoint_dir, allow_repeats) {
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
  purrr::map_dfr(seq_len(nrow(events)), function(i) {
    dplyr::bind_cols(
      tibble(
        seq_id = events$seq_id[i],
        event_id = events$event_id[i],
        symbol = seq_len(hrep::alphabet_size("pc_chord")),
        observed = .data$symbol == events$symbol[i],
        legal = allow_repeats |
          events$event_id[i] == 1L |
          .data$symbol != events$prev_symbol[i]
      ),
      t(raw[viewpoints, events$event_id[i], ]) %>%
        tibble::as_tibble()
    )
  })
}

add_polynomials <- function(continuous_model_matrix, predictors, poly_degree, na_val) {
  viewpoints <- predictors %>%
    dplyr::filter(!.data$discrete) %>%
    dplyr::pull(.data$viewpoint) %>%
    unique() %>%
    magrittr::set_names(., .)
  res <- purrr::map(viewpoints, function(v) {
    raw <- continuous_model_matrix[[v]] %>%
      dplyr::if_else(is.na(.), na_val, .) %>%
      stats::poly(degree = poly_degree)
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
    continuous_model_matrix = dplyr::bind_cols(
      continuous_model_matrix %>%
        dplyr::select(c("seq_id", "event_id", "symbol", "observed", "legal")),
      vals
    ),
    poly_coefs = purrr::map(res, "coefs")
  )
}

get_discrete_model_matrix <- function(corpus, predictors, ppm_dir, na_val) {
  message("Getting model matrix for discrete viewpoints...")
  plyr::llply(sort(unique(corpus$seq_id)), function(i) {
    corpus %>%
      dplyr::filter(.data$seq_id == i) %>%
      seq_discrete_model_matrix(predictors, ppm_dir, na_val)
  }, .progress = "text") %>%
    dplyr::bind_rows()
}

seq_discrete_model_matrix <- function(events, predictors, ppm_dir, na_val) {
  models <- predictors %>%
    dplyr::filter(.data$discrete) %>%
    dplyr::pull(.data$label) %>%
    unique()
  seq_id <- unique(events$seq_id)
  stopifnot(!anyDuplicated(models),
            length(seq_id) == 1)
  raw <- readRDS(file.path(ppm_dir, "output", paste0(seq_id, ".rds")))

  purrr::pmap_dfr(events, function(seq_id, event_id, symbol, prev_symbol) {
    cbind(
      seq_id, event_id,
      symbol = seq_len(hrep::alphabet_size("pc_chord")),
      t(raw[models, event_id, ]) %>%
        tibble::as_tibble() %>%
        dplyr::mutate_all(~ log2(.)) %>%
        dplyr::mutate_all(~ dplyr::if_else(is.na(.), na_val, .))
    ) %>% tibble::as_tibble()
  })
}

get_model_matrix_predictors <- function(viewpoints, viewpoint_dir, ppm_dir, poly_degree) {
  discrete <- readr::read_csv(file.path(ppm_dir, "models.csv"), col_types = readr::cols()) %>%
    tibble::add_column(poly_degree = as.integer(NA), .after = 3L) %>%
    tibble::add_column(discrete = TRUE, .before = 1L) %>%
    dplyr::select(- .data$id)

  continuous <- readRDS(file.path(viewpoint_dir, "about.rds"))$continuous_viewpoints %>%
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
  about <- readRDS(file.path(viewpoint_dir, "about.rds"))
  c(about$discrete %>% purrr::map_chr("name"),
    about$continuous) %>% sort()
}

get_model_matrix_corpus <- function(viewpoint_dir, test_seq, max_sample, sample_seed) {
  corpus <- readRDS(file.path(viewpoint_dir, "corpus.rds")) %>%
    as.list() %>%
    purrr::map2_dfr(seq_along(.), ., ~ tibble(seq_id = .x,
                                              event_id = seq_along(.y),
                                              symbol = as.integer(.y),
                                              prev_symbol = c(as.integer(NA),
                                                              .data$symbol[- length(.data$symbol)]),
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
