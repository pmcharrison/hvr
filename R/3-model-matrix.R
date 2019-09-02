#' Compute model matrix
#'
#' Computes the model matrix, which compiles together expectedness values
#' from the PPM analyses as well as polynomial expansions of the
#' continuous features.
#'
#' The following routines should have been run already:
#' 1. \code{\link{compute_viewpoints}}
#' 2. \code{\link{compute_ppm_analyses}}
#'
#' @param parent_dir
#' (Character scalar)
#' The parent directory for the output files, shared with functions such as
#' \code{\link{compute_viewpoints}} and \code{\link{compute_ppm_analyses}}.
#' Ignored if all other directory arguments are manually specified.
#'
#' @param max_sample
#' (Numeric scalar)
#' Maximum number of events to sample for the model matrix,
#' defaults to \code{Inf} (no downsampling).
#' Lower values of \code{max_sample} prompt random downsampling.
#'
#' @param sample_seed
#' (Integer scalar)
#' Random seed to make the downsampling reproducible.
#'
#' @param poly_degree
#' (Integer scalar)
#' Degree of the polynomials to compute for the continuous features.
#'
#' @param na_val
#' (Numeric scalar)
#' Value to use to code for NA in the model matrix.
#' The statistical analyses are mostly unaffected by this value.
#'
#' @param filter_corpus
#' (NULL or a function)
#' An optional function to apply to the corpus to determine which
#' events should be retained in the model matrix.
#' The function is applied to the corpus object saved as \code{corpus.rds}
#' in \code{viewpoint_dir}. This corpus object takes the form of a
#' \code{\link[tibble]{tibble}}; the function should return a
#' row-subset of this \code{tibble}.
#'
#' @param ltm
#' (Logical scalar, default = \code{TRUE})
#' If \code{FALSE}, long-term (i.e. pretrained) PPM model outputs are
#' excluded from the model matrix.
#'
#' @param viewpoint_dir
#' (Character scalar)
#' The directory for the already-generated
#' output files from \code{\link{compute_viewpoints}}.
#' The default should be correct if the user used the
#' default \code{dir} argument in \code{\link{compute_viewpoints}}.
#'
#' @param compute_ppm_analyses
#' (Character scalar)
#' The directory for the already-generated
#' output files from \code{\link{compute_ppm_analyses}}.
#' The default should be correct if the user used the
#' default \code{dir} argument in \code{\link{compute_ppm_analyses}}.
#'
#' @param output_dir
#' (Character scalar)
#' The output directory for the model matrix.
#' Will be created if it doesn't exist already.
#'
#' @param viewpoints
#' Character vector listing the viewpoints to be included in the model matrix.
#' By default this list is read from \code{viewpoint_dir}.
#'
#' @param seq_test
#' Integer vector identifying which sequences should be sampled from for
#' constructing the model matrix, indexing into the \code{corpus}
#' argument of \code{\link{compute_viewpoints}}.
#' Defaults to the \code{seq_test} argument that was provided to
#' \code{\link{compute_viewpoints}}.
#'
#' @param allow_repeats
#' (Logical scalar)
#' Whether repeated chords are theoretically permitted in the
#' chord sequences. It is recommended to remove such repetitions
#' before modelling.
#'
#' @return
#' The primary output is written to disk in the \code{dir} directory.
#' The model matrix provides metafeature values
#' (i.e. expectedness values for discrete features
#' and polynomial values for continuous features)
#' over the entire chord alphabet at every location in \code{seq_test}.
#'
#' @md
#'
#' @export
compute_model_matrix <- function(
  parent_dir,
  max_sample = Inf,
  sample_seed = 1,
  poly_degree = 4L,
  na_val = 0,
  filter_corpus = NULL,
  ltm = TRUE,
  viewpoint_dir = file.path(parent_dir, "0-viewpoints"),
  ppm_dir = file.path(parent_dir, "1-ppm"),
  output_dir = file.path(parent_dir, "2-model-matrix"),
  viewpoints = read_viewpoints(viewpoint_dir),
  seq_test = list_seq_test(ppm_dir),
  allow_repeats = FALSE
) {
  checkmate::qassert(max_sample, "N1[50,]")
  checkmate::qassert(na_val, "N1(,)")
  checkmate::qassert(poly_degree, "X1[1,)")
  checkmate::qassert(allow_repeats, "B1")
  checkmate::qassert(ltm, "B1")
  stopifnot(is.null(filter_corpus) || is.function(filter_corpus))
  corpus <- get_model_matrix_corpus(viewpoint_dir, seq_test, max_sample, sample_seed, filter_corpus)
  predictors <- get_model_matrix_predictors(viewpoints, viewpoint_dir, ppm_dir, poly_degree, ltm)

  R.utils::mkdirs(output_dir)
  write_model_matrix_about(max_sample, sample_seed, poly_degree, viewpoints,
                           output_dir)

  tmp_1 <- get_continuous_model_matrix(corpus, predictors, viewpoint_dir,
                                       poly_degree, na_val, allow_repeats)

  model_matrix <- get_model_matrix(
    tmp_1$continuous_model_matrix,
    get_discrete_model_matrix(corpus, predictors, ppm_dir, na_val),
    predictors
  )

  check_model_matrix(model_matrix)

  observation_matrix <- model_matrix %>%
    dplyr::filter(.data$observed) %>%
    dplyr::select(predictors$label) %>%
    as.matrix()

  tmp_2 <- model_matrix %>%
    split(., .$seq_event_id) %>%
    magrittr::set_names(NULL)

  continuation_matrices <- tmp_2 %>%
    purrr::map(dplyr::select, predictors$label) %>%
    purrr::map(as.matrix)

  legal <- tmp_2 %>% purrr::map("legal")

  message("Saving outputs...")
  saveRDS(tmp_1$moments, file.path(output_dir, "moments.rds"))
  saveRDS(tmp_1$poly_coefs, file.path(output_dir, "poly-coefs.rds"))
  saveRDS(corpus, file.path(output_dir, "corpus.rds"))
  saveRDS(predictors, file.path(output_dir, "predictors.rds"))
  # saveRDS(model_matrix, file.path(output_dir, "model-matrix.rds"))
  saveRDS(observation_matrix, file.path(output_dir, "observation-matrix.rds"))
  saveRDS(continuation_matrices, file.path(output_dir, "continuation-matrices.rds"))
  saveRDS(legal, file.path(output_dir, "legal.rds"))
}

check_model_matrix <- function(model_matrix) {
  if (any(model_matrix$observed & !model_matrix$legal)) {
    stop("Observed illegal events. Did you forget to ",
         "set 'allow_repeats' to TRUE?")
  }
}

get_moments <- function(model_matrix, predictors) {
  message("Computing moments...")
  viewpoints <- predictors %>%
    dplyr::filter(!.data$discrete) %>%
    dplyr::pull(.data$viewpoint) %>%
    unique() %>% sort()
  list(observed = model_matrix %>% dplyr::filter(.data$observed),
       all_legal = model_matrix %>% dplyr::filter(.data$legal)) %>%
    purrr::map(
      function(df) {
        purrr::map(viewpoints %>% purrr::set_names(viewpoints), function(v) {
          list(
            mean = mean(df[[v]], na.rm = TRUE),
            sd = sd(df[[v]], na.rm = TRUE),
            quantiles = stats::quantile(df[[v]],
                                        c(0.05, 0.25, 0.5, 0.75, 0.95),
                                        na.rm = TRUE),
            range = range(df[[v]], na.rm = TRUE)
          )
        })
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


list_seq_test <- function(ppm_dir) {
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
    data.table::rbindlist()
    # dplyr::bind_rows()

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
      array(raw[viewpoints, events$event_id[i], ],
            dim = c(length(viewpoints), hrep::alphabet_size("pc_chord")),
            dimnames = list(viewpoints)) %>%
        t() %>% tibble::as_tibble()
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
    data.table::rbindlist()
    # dplyr::bind_rows()
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

  purrr::pmap_dfr(events, function(seq_event_id, seq_id, event_id,
                                   symbol, prev_symbol) {
    cbind(
      seq_event_id, seq_id, event_id,
      symbol = seq_len(hrep::alphabet_size("pc_chord")),
      array(raw[models, event_id, ],
            dim = c(length(models), hrep::alphabet_size("pc_chord")),
            dimnames = list(models)) %>%
        t() %>%
        tibble::as_tibble() %>%
        dplyr::mutate_all(~ log(.)) %>%
        dplyr::mutate_all(~ dplyr::if_else(is.na(.), na_val, .))
    ) %>% tibble::as_tibble()
  })
}

get_model_matrix_predictors <- function(viewpoints, viewpoint_dir, ppm_dir, poly_degree, ltm) {
  discrete <- readr::read_csv(file.path(ppm_dir, "models.csv"), col_types = readr::cols()) %>%
    tibble::add_column(poly_degree = as.integer(NA), .after = 3L) %>%
    tibble::add_column(discrete = TRUE, .before = 1L) %>%
    dplyr::select(- .data$id) %>%
    dplyr::filter(!!ltm | .data$class == "stm")

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

read_viewpoints <- function(viewpoint_dir) {
  about <- readRDS(file.path(viewpoint_dir, "about.rds"))
  c(about$discrete %>% purrr::map_chr("name"),
    about$continuous) %>% sort()
}

get_model_matrix_corpus <- function(viewpoint_dir, seq_test, max_sample, sample_seed, filter_corpus) {
  corpus <- readRDS(file.path(viewpoint_dir, "corpus.rds")) %>%
    as.list() %>%
    purrr::map2_dfr(seq_along(.), ., ~ tibble(seq_id = .x,
                                              event_id = seq_along(.y),
                                              symbol = as.integer(.y),
                                              prev_symbol = c(as.integer(NA),
                                                              .data$symbol[- length(.data$symbol)]))) %>%
    dplyr::filter(.data$seq_id %in% seq_test)

  if (!is.null(filter_corpus)) corpus <- filter_corpus(corpus)
  if (!(tibble::is_tibble(corpus) && identical(names(corpus), c(
    "seq_id", "event_id", "symbol", "prev_symbol"
  )))) stop("filter_corpus must return a tibble preserving the columns ",
            "of its input")

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
