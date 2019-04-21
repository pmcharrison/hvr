#' @export
viewpoint_regression <- function(
  parent_dir,
  max_sample = 1e4,
  sample_seed = 1,
  poly_degree = 3L,
  max_iter = 500,
  perm_int = TRUE,
  perm_int_seed = 1,
  perm_int_reps = 25,
  model_matrix_dir = file.path(parent_dir, "2-model-matrix"),
  output_dir = file.path(parent_dir, "3-viewpoint-regression"),
  viewpoints = readRDS(file.path(model_matrix_dir, "about.rds"))$viewpoints
) {
  checkmate::qassert(poly_degree, "X1[1,)")
  poly_degree <- as.integer(poly_degree)

  checkmate::qassert(max_sample, "X1[50,)")
  max_sample <- as.integer(max_sample)

  message("Loading data...")
  predictors <- get_regression_predictors(model_matrix_dir, viewpoints, poly_degree)
  corpus <- get_regression_corpus(model_matrix_dir, max_sample, sample_seed)
  model_matrix <- get_regression_model_matrix(corpus, model_matrix_dir, predictors)
  poly_coefs <- readRDS(file.path(model_matrix_dir, "poly-coefs.rds"))
  moments <- readRDS(file.path(model_matrix_dir, "moments.rds"))
  message("  done.")

  res <- conduct_regression(model_matrix, corpus, predictors, poly_degree, poly_coefs, moments,
                            max_iter, perm_int, perm_int_seed, perm_int_reps)

  R.utils::mkdirs(output_dir)
  write_regression_about(output_dir, max_sample, sample_seed, poly_degree,
                         perm_int, perm_int_seed, perm_int_reps, viewpoints)
  saveRDS(corpus, file.path(output_dir, "corpus.rds"))
  saveRDS(res, file.path(output_dir, "results.rds"))

  invisible(res)
}

write_regression_about <- function(output_dir, max_sample, sample_seed, poly_degree,
                                   perm_int, perm_int_seed, perm_int_reps,
                                   viewpoints) {
  list(
    max_sample = max_sample,
    sample_seed = sample_seed,
    poly_degree = poly_degree,
    perm_int = perm_int,
    perm_int_seed = perm_int_seed,
    perm_int_reps = perm_int_reps,
    viewpoints = viewpoints
  ) %>%
    saveRDS(file.path(output_dir, "about.rds"))
}

#' @export
get_marginal <- function(x, viewpoint) {
  UseMethod("get_marginal")
}

#' @export
get_marginal.viewpoint_regression <- function(x, viewpoint) {
  checkmate::qassert(viewpoint, "S1")
  continuous_viewpoints <- x$predictors %>%
    dplyr::filter(!.data$discrete) %>%
    dplyr::pull(.data$viewpoint) %>%
    unique()
  if (!viewpoint %in% continuous_viewpoints)
    stop("can only plot marginals for continuous viewpoints")

  moments <- x$moments %>% dplyr::filter(.data$viewpoint == !!viewpoint)
  z_seq <- seq(from = -4, to = 4, length.out = 1000)
  x_seq <- seq(from = moments$mean - 4 * moments$sd,
               to = moments$mean + 4 * moments$sd,
               length.out = 1000)
  poly_coef <- x$poly_coefs[[viewpoint]]
  degree <- poly_coef$alpha %>% length()
  poly_x <- poly(x_seq, degree = degree, coefs = poly_coef)[, seq_len(x$poly_degree)]
  par <- x$predictors %>%
    dplyr::filter(.data$viewpoint == !!viewpoint) %>%
    dplyr::arrange(.data$poly_degree) %>%
    dplyr::pull(.data$label) %>%
    {x$par[.]}

  y <- poly_x %*% matrix(par, ncol = 1)

  tibble(feature_raw = x_seq,
         feature_z = z_seq,
         effect = y)
}

#' @export
plot_perm_int <- function(
  x,
  gg = FALSE,
  labels = character(),
  # axis_1 = "Viewpoint",
  axis_label = "Model reliance (bits)",
  mai = c(1, 2.5, 0, 0.5),
  ...
) {
  UseMethod("plot_perm_int")
}

#' @export
plot_perm_int.viewpoint_regression <- function(
  x,
  gg = FALSE,
  labels = character(),
  # axis_1 = "Viewpoint",
  axis_label = "Model reliance (bits)",
  mai = c(1, 2.5, 0, 0.5),
  ...
) {
  dat <- x$perm_int
  names(dat) <- plyr::revalue(names(dat), labels, warn_missing = FALSE)

  if (gg) {
    tibble(viewpoint = factor(names(dat),
                              levels = sort(names(dat), decreasing = TRUE)),
           perm_int = as.numeric(dat)) %>%
      ggplot2::ggplot(ggplot2::aes_string("viewpoint", "perm_int")) +
      ggplot2::geom_bar(colour = "black", fill = "#6ba3ff", stat = "identity") +
      ggplot2::scale_x_discrete(NULL) +
      ggplot2::scale_y_continuous(axis_label) +
      ggplot2::coord_flip()
  } else {
    withr::with_par(list(mai = mai), {
      dat %>%
        {.[order(names(dat), decreasing = TRUE)]} %>%
        barplot(horiz = TRUE, las = 1, xlab = axis_label, ...)
    })
  }
}

#' @export
plot_marginal <- function(x,
                          viewpoint,
                          gg = FALSE,
                          title = viewpoint,
                          x_lab = "Feature value (z-score)",
                          y_lab = "Odds ratio") {
  UseMethod("plot_marginal")
}

#' @export
plot_marginal.viewpoint_regression <- function(
  x,
  viewpoint,
  gg = FALSE,
  title = viewpoint,
  x_lab = "Feature value (z-score)",
  y_lab = "Log odds"
) {
  dat <- get_marginal(x, viewpoint = viewpoint)

  if (gg) {
    if (!requireNamespace("ggplot2", quietly = TRUE))
      stop("please install the ggplot2 package before using this function")
    p <- ggplot2::ggplot(dat, ggplot2::aes_string("feature_z", "effect")) +
      ggplot2::geom_line() +
      ggplot2::scale_x_continuous(x_lab) +
      ggplot2::scale_y_continuous(y_lab)
    if (!is.null(title)) p <- p + ggplot2::ggtitle(title)
    p

  } else {
    plot(dat$feature_z,
         dat$effect,
         type = "l",
         xlab = x_lab,
         ylab = y_lab,
         main = viewpoint)
  }
}

conduct_regression <- function(model_matrix, corpus,
                               predictors, poly_degree, poly_coefs, moments,
                               max_iter, perm_int, perm_int_seed, perm_int_reps) {
  message("Reshaping data...")

  observation_matrix <- model_matrix %>%
    dplyr::filter(.data$observed) %>%
    dplyr::select(predictors$label) %>%
    as.matrix()

  tmp <- continuation_matrices <- model_matrix %>%
    dplyr::group_by(.data$seq_event_id) %>%
    dplyr::group_split() %>%
    purrr::map(~ dplyr::arrange(., .data$symbol))

  continuation_matrices <- tmp %>%
    purrr::map(dplyr::select, predictors$label) %>%
    purrr::map(as.matrix)

  legal <- tmp %>%
    purrr::map("legal")

  message("Fitting conditional logit model...")

  x <- optim(par = rep(0, times = nrow(predictors)),
             fn = cost,
             gr = gradient,
             method = "BFGS",
             observation_matrix = observation_matrix,
             continuation_matrices = continuation_matrices,
             legal = legal,
             control = list(trace = 6,
                            maxit = max_iter))

  permutation_importance <- if (perm_int)
    get_perm_int(weights = x$par,
                 benchmark_cost = x$value,
                 corpus = corpus,
                 observation_matrix = observation_matrix,
                 continuation_matrices = continuation_matrices,
                 legal = legal,
                 predictors = predictors,
                 seed = perm_int_seed,
                 reps = perm_int_reps)

  res <- list(predictors = predictors,
              par = x$par %>% magrittr::set_names(predictors$label),
              poly_degree = poly_degree,
              poly_coefs = poly_coefs,
              moments = moments,
              cost = x$value,
              perm_int = permutation_importance
  )
  class(res) <- c("viewpoint_regression", "list")
  res
}

get_perm_int <- function(weights,
                         benchmark_cost,
                         corpus,
                         observation_matrix,
                         continuation_matrices,
                         legal,
                         predictors,
                         seed,
                         reps) {
  message("Computing permutation feature importances...")
  viewpoints <- predictors$viewpoint %>% unique() %>% sort()
  plyr::laply(viewpoints, function(v) {
    withr::with_seed(seed, {
      plyr::laply(seq_len(reps), function(...) {
        tmp <-  permute_matrices(observation_matrix,
                                 continuation_matrices,
                                 corpus,
                                 predictors,
                                 v)
        cost(weights, tmp$new_obs_matrix, tmp$new_con_matrices, legal) - benchmark_cost
      })
    }) %>% mean()
  }, .progress = "text") %>%
    magrittr::set_names(viewpoints)
}

permute_matrices <- function(observation_matrix,
                             continuation_matrices,
                             corpus,
                             predictors,
                             v) {
  cols <- predictors %>% dplyr::filter(.data$viewpoint == v) %>% `$`("label")

  new_continuation_matrices <- purrr::map(continuation_matrices, permute_cols, cols)

  new_observation_matrix <-
    purrr::pmap(corpus, function(seq_event_id, seq_id, event_id, symbol, ...) {
      new_continuation_matrices[[seq_event_id]][symbol, ]
    }) %>% do.call(rbind, .)

  list(new_obs_matrix = new_observation_matrix,
       new_con_matrices = new_continuation_matrices)
}

permute_cols <- function(df, cols) {
  n <- nrow(df)
  ind <- sample(n, size = n, replace = FALSE)
  df[, cols] <- df[ind, cols]
  # print(cols)
  df
}

# conduct_regression_old <- function(model_matrix, predictors) {
#
#   message("Fitting conditional logit model...")
#
#   m <- predictors$label %>%
#     paste(collapse = " + ") %>%
#     sprintf("cbind(observed, seq_event_id) ~ %s", .) %>%
#     as.formula() %>%
#     mclogit::mclogit(data = model_matrix,
#                      start = c(1, 1),  #rep(1, times = nrow(predictors)),
#                      control = mclogit::mclogit.control(trace = TRUE))
#
#   m
# }

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
    dplyr::filter(.data$viewpoint %in% viewpoints)
}

get_regression_model_matrix <- function(corpus, model_matrix_dir, predictors) {
  raw <- readRDS(file.path(model_matrix_dir, "model-matrix.rds"))
  dplyr::inner_join(raw,
                    dplyr::select(corpus, - .data$symbol),
                    by = c("seq_id", "event_id")) %>%
    dplyr::select(c("seq_event_id", "seq_id", "event_id", "symbol",
                    "observed", "legal", predictors$label))
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
