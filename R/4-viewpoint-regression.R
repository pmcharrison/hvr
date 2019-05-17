#' @export
viewpoint_regression <- function(
  parent_dir,
  max_iter = 500,
  perm_int = TRUE,
  perm_int_seed = 1,
  perm_int_reps = 5,
  allow_negative_weights = FALSE,
  viewpoint_dir = file.path(parent_dir, "0-viewpoints"),
  model_matrix_dir = file.path(parent_dir, "2-model-matrix"),
  output_dir = file.path(parent_dir, "3-viewpoint-regression")
) {
  checkmate::qassert(parent_dir, "S1")
  checkmate::qassert(max_iter, "X1[10,)")
  checkmate::qassert(perm_int, "B1")
  checkmate::qassert(perm_int_reps, "X1[1,)")
  checkmate::qassert(allow_negative_weights, "B1")
  checkmate::qassert(viewpoint_dir, "S1")
  checkmate::qassert(model_matrix_dir, "S1")
  checkmate::qassert(output_dir, "S1")

  viewpoint_labels <- readRDS(file.path(viewpoint_dir, "about.rds"))$viewpoint_labels
  viewpoints <- readRDS(file.path(model_matrix_dir, "about.rds"))$viewpoints
  poly_degree <- readRDS(file.path(model_matrix_dir, "about.rds"))$poly_degree

  message("Loading data...")
  predictors <- get_regression_predictors(model_matrix_dir)
  corpus <- get_regression_corpus(model_matrix_dir)
  observation_matrix <- readRDS(file.path(model_matrix_dir, "observation-matrix.rds"))
  continuation_matrices <- readRDS(file.path(model_matrix_dir, "continuation-matrices.rds"))
  legal <- readRDS(file.path(model_matrix_dir, "legal.rds"))
  poly_coefs <- readRDS(file.path(model_matrix_dir, "poly-coefs.rds"))
  moments <- readRDS(file.path(model_matrix_dir, "moments.rds"))
  message("  done.")

  res <- conduct_regression(observation_matrix, continuation_matrices, legal,
                            corpus, predictors, poly_degree, poly_coefs, moments,
                            max_iter, perm_int, perm_int_seed, perm_int_reps,
                            viewpoint_labels, allow_negative_weights)

  R.utils::mkdirs(output_dir)
  write_regression_about(output_dir, poly_degree,
                         perm_int, perm_int_seed, perm_int_reps, viewpoints)
  saveRDS(corpus, file.path(output_dir, "corpus.rds"))
  saveRDS(res, file.path(output_dir, "results.rds"))

  invisible(res)
}

#' @export
is_viewpoint_regression <- function(x) {
  is(x, "viewpoint_regression")
}

write_regression_about <- function(output_dir, poly_degree,
                                   perm_int, perm_int_seed, perm_int_reps,
                                   viewpoints) {
  list(
    poly_degree = poly_degree,
    perm_int = perm_int,
    perm_int_seed = perm_int_seed,
    perm_int_reps = perm_int_reps,
    viewpoints = viewpoints
  ) %>%
    saveRDS(file.path(output_dir, "about.rds"))
}

#' @export
get_discrete_weights <- function(x) {
  UseMethod("get_discrete_weights")
}

get_discrete_weights.viewpoint_regression <- function(x) {
  viewpoints <- list_viewpoints(x, continuous = FALSE)
  tibble(label = names(x$par),
         par = as.numeric(x$par)) %>%
    dplyr::left_join(x$predictors, by = "label") %>%
    dplyr::filter(.data$discrete) %>%
    dplyr::left_join(x$viewpoint_labels, by = "viewpoint")
}

#' @export
plot_discrete_weights <- function(x,
                                  x_lab = "Viewpoint",
                                  y_lab = "Weight",
                                  colours = c("#11A3FF", "#B50000")) {
  UseMethod("plot_discrete_weights")
}

#' @export
plot_discrete_weights.viewpoint_regression <- function(
  x,
  x_lab = "Viewpoint",
  y_lab = "Weight",
  colours = c("#11A3FF", "#B50000")
) {
  if (!requireNamespace("ggplot2")) stop("ggplot2 must be installed first")
  get_discrete_weights(x) %>%
    dplyr::mutate(class = dplyr::recode(class,
                                        ltm = "Long-term",
                                        stm = "Short-term")) %>%
    ggplot2::ggplot(ggplot2::aes_string(x = "viewpoint_label",
                                        y = "par",
                                        fill = "class")) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.9),
                      colour = "black", width = 0.9) +
    ggplot2::scale_fill_manual("Class", values = colours) +
    ggplot2::scale_x_discrete(x_lab) +
    ggplot2::scale_y_continuous(y_lab) +
    ggplot2::coord_flip()
}

#' @export
plot_discrete_weights_compared <- function(x,
                                           y,
                                           x_lab,
                                           y_lab,
                                           point_size = 3,
                                           colours = c("#11A3FF", "#B50000")) {
  UseMethod("plot_discrete_weights_compared")
}

#' @export
plot_discrete_weights_compared.viewpoint_regression <- function(x,
                                                                y,
                                                                x_lab,
                                                                y_lab,
                                                                point_size = 3,
                                                                colours = c("#11A3FF", "#B50000")) {
  stopifnot(is_viewpoint_regression(y))
  if (!requireNamespace("ggplot2")) stop("ggplot2 must be installed first")

  df_y <- get_discrete_weights(y) %>%
    dplyr::mutate(class = dplyr::recode(class,
                                        ltm = "Long-term",
                                        stm = "Short-term"))

  df_scatter <- dplyr::inner_join(
    get_discrete_weights(x) %>% dplyr::select(c("label", "par")),
    get_discrete_weights(y) %>% dplyr::select(c("label", "par", "class")),
    by = "label", suffix = c("_x", "_y")
  ) %>%
    dplyr::mutate(class = dplyr::recode(class,
                                        ltm = "Long-term",
                                        stm = "Short-term"))

  list(
    bar = plot_discrete_weights(x, colours = colours) +
      ggplot2::scale_colour_manual(values = colours) +
      ggplot2::geom_point(data = df_y,
                          mapping = ggplot2::aes_string(x = "viewpoint_label",
                                                        y = "par",
                                                        colour = "class"),
                          position = ggplot2::position_dodge(width = 0.9),
                          fill = "white", shape = 21, size = point_size,
                          show.legend = FALSE),
    scatter = df_scatter %>%
      ggplot2::ggplot(ggplot2::aes_string("par_x", "par_y", colour = "class")) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
      ggplot2::geom_point() +
      ggplot2::scale_colour_manual("Class", values = colours) +
      ggplot2::scale_x_continuous(x_lab) +
      ggplot2::scale_y_continuous(y_lab),
    data = list(scatter = df_scatter)
  )

}

#' @export
get_marginals <- function(x) {
  UseMethod("get_marginals")
}

#' @export
get_marginals.viewpoint_regression <- function(x) {
  viewpoints <- list_viewpoints(x, discrete = FALSE)
  tmp <- purrr::map(viewpoints, function(v) {
    get_marginal(x, v) %>%
      tibble::add_column(viewpoint = v, .before = 1)
  })
  marginals <- dplyr::bind_rows(tmp)
  moments <- c("observed", "all_legal") %>% {purrr::set_names(., .)} %>%
    purrr::map(
      ~ purrr::map2_dfr(viewpoints, tmp, function(v, w) {
        m <- attr(w, "moments")[[.]]
        tibble(viewpoint = v,
               mean = m$mean,
               sd = m$sd,
               quantile_05 = m$quantiles["5%"],
               quantile_95 = m$quantiles["95%"],
               min = m$range[1],
               max = m$range[2])
      }))
  list(marginals = marginals,
       moments = moments)
}

#' @export
get_marginal <- function(x, viewpoint) {
  UseMethod("get_marginal")
}

#' @export
get_marginal.viewpoint_regression <- function(x, viewpoint) {
  checkmate::qassert(viewpoint, "S1")
  if (!viewpoint %in% list_viewpoints(x, discrete = FALSE))
    stop("can only plot marginals for continuous viewpoints")

  all_legal_range <- x$moments$all_legal[[viewpoint]]$range

  seq_range <- seq(from = 0, to = 1, length.out = 1000)
  seq_viewpoint <- seq(from = all_legal_range[1],
                       to = all_legal_range[2],
                       length.out = 1000)
  poly_coef <- x$poly_coefs[[viewpoint]]
  poly_degree <- poly_coef$alpha %>% length()
  seq_polynomial <- poly(seq_viewpoint,
                         degree = poly_degree,
                         coefs = poly_coef)[, seq_len(x$poly_degree), drop = FALSE]
  par <- x$predictors %>%
    dplyr::filter(.data$viewpoint == !!viewpoint) %>%
    dplyr::arrange(.data$poly_degree) %>%
    dplyr::pull(.data$label) %>%
    {x$par[.]}

  seq_effect <- as.numeric(seq_polynomial %*% matrix(par, ncol = 1))

  res <- tibble(feature_range = seq_range,
                feature_raw = seq_viewpoint,
                effect = seq_effect)
  attr(res, "moments") <- list(observed = x$moments$observed[[viewpoint]],
                               all_legal = x$moments$all_legal[[viewpoint]])
  res
}

#' @export
get_costs <- function(x) {
  UseMethod("get_costs")
}

#' @export
get_costs.viewpoint_regression <- function(x) {
  tibble(
    model = names(x$cost_benchmarks),
    cost = as.numeric(x$cost_benchmarks)
  ) %>%
    dplyr::left_join(x$predictors %>% dplyr::select(c("class", "viewpoint", "label")),
                     by = c(model = "label")) %>%
    dplyr::left_join(x$viewpoint_labels, by = "viewpoint") %>%
    dplyr::mutate(class = dplyr::recode(.data$class,
                                        stm = "Short-term",
                                        ltm = "Long-term")) %>%
    dplyr::rename(label = .data$viewpoint_label) %>%
    dplyr::bind_rows(
      tibble(
        model = "combined",
        cost = x$cost,
        class = "Combined",
        viewpoint = as.character(NA),
        label = "Combined"
      )
    )
}

#' @export
plot_costs <- function(x,
                       x_lab = "Cost (bits/chord)",
                       y_lab = "Model",
                       factor_lab = "Model type",
                       factor_col = c("#E8E410", "#11A3FF", "#B50000")) {
  UseMethod("plot_costs")
}

#' @export
plot_costs.viewpoint_regression <- function(x,
                                            x_lab = "Cost (bits/chord)",
                                            y_lab = "Model",
                                            factor_lab = "Model type",
                                            factor_col = c("#E8E410", "#11A3FF", "#B50000")) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("please install the ggplot2 package before using this function")
  get_costs(x) %>%
    dplyr::mutate(label = factor(.data$label,
                                 levels = c("Combined",
                                            setdiff(unique(.data$label),
                                                    "Combined") %>% sort(TRUE)))) %>%
    ggplot2::ggplot(ggplot2::aes_string(x = "label", y = "cost", fill = "class")) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", colour = "black") +
    ggplot2::scale_x_discrete(y_lab) +
    ggplot2::scale_y_continuous(x_lab, limits = c(0, 15)) +
    ggplot2::scale_fill_manual(factor_lab, values = factor_col) +
    ggplot2::coord_flip() +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))
}

#' @export
plot_perm_int <- function(
  x,
  gg = FALSE,
  labels = character(),
  # axis_1 = "Viewpoint",
  axis_label = "Model reliance (bits)",
  mai = c(1, 2.5, 0, 0.5),
  order_by_label = FALSE,
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
  axis_label = "Model reliance (bits/chord)",
  mai = c(1, 2.5, 0, 0.5),
  order_by_label = FALSE,
  ...
) {
  dat <- x$perm_int
  names(dat) <- plyr::revalue(names(dat), labels, warn_missing = FALSE)
  if (order_by_label)
    dat <- dat[order(names(dat), decreasing = TRUE)] else
      dat <- dat[order(dat, decreasing = TRUE)]

  if (gg) {
    tibble(viewpoint = factor(names(dat), levels = names(dat)),
           perm_int = as.numeric(dat)) %>%
      ggplot2::ggplot(ggplot2::aes_string("viewpoint", "perm_int")) +
      ggplot2::geom_bar(colour = "black", fill = "#6ba3ff", stat = "identity") +
      ggplot2::scale_x_discrete(NULL) +
      ggplot2::scale_y_continuous(axis_label) +
      ggplot2::coord_flip()
  } else {
    withr::with_par(list(mai = mai), {
      dat %>%
        # rev() %>%
        graphics::barplot(horiz = TRUE, las = 1, xlab = axis_label, ...)
    })
  }
}

#' @export
plot_marginals <- function(model_1,
                           model_2 = NULL,
                           only_observed = FALSE,
                           x_lab = "Relative feature value",
                           y_lab = "Effect",
                           viewpoint_labels = x$viewpoint_labels,
                           fill = "blue",
                           alpha = 0.25,
                           ...) {
  UseMethod("plot_marginals")
}

#' @export
plot_marginals.viewpoint_regression <- function(model_1,
                                                model_2 = NULL,
                                                model_labels = NULL,
                                                only_observed = FALSE,
                                                x_lab = "Relative feature value",
                                                y_lab = "Effect",
                                                viewpoint_labels = model_1$viewpoint_labels,
                                                fill = "blue",
                                                alpha = 0.25,
                                                ...) {
  stopifnot(is.data.frame(viewpoint_labels),
            all(names(viewpoint_labels) == c("viewpoint", "viewpoint_label")),
            !anyDuplicated(viewpoint_labels$viewpoint),
            !anyDuplicated(viewpoint_labels$viewpoint_label))
  if (!is.null(model_2)) {
    stopifnot(is_viewpoint_regression(model_2))
    checkmate::qassert(model_labels, "S2")
  }

  tmp <- get_marginals(model_1)
  moments <- tmp$moments

  marginals <- if (is.null(model_2)) {
    tmp$marginals
  } else {
    dplyr::bind_rows(
      tmp$marginals %>% dplyr::mutate(model = model_labels[1]),
      get_marginals(model_2)$marginals %>% dplyr::mutate(model = model_labels[2])
    ) %>%
      dplyr::mutate(model = factor(model, levels = model_labels))
  }

  if (!all(unique(marginals$viewpoint) %in% viewpoint_labels$viewpoint))
    stop("labels need to be provided for the following viewpoint(s): ",
         paste(setdiff(unique(marginals$viewpoint), viewpoint_labels$viewpoint)))

  stopifnot(all(marginals$viewpoint %in% viewpoint_labels$viewpoint))


  df_quantiles <- dplyr::inner_join(moments$observed, moments$all_legal,
                                    by = "viewpoint",
                                    suffix = c("_obs", "_legal")) %>%
    dplyr::select(c("viewpoint", "quantile_05_obs", "quantile_95_obs",
                    "min_legal", "max_legal")) %>%
    dplyr::mutate(range_legal = .data$max_legal - .data$min_legal,
                  x_min = (.data$quantile_05_obs - .data$min_legal) / .data$range_legal,
                  x_max = (.data$quantile_95_obs - .data$min_legal) / .data$range_legal) %>%
    dplyr::left_join(viewpoint_labels, by = "viewpoint")



  if (only_observed) {
    marginals <- marginals %>%
      dplyr::left_join(df_quantiles %>%
                         dplyr::select(c("viewpoint",
                                         "quantile_05_obs",
                                         "quantile_95_obs")),
                       by = "viewpoint") %>%
      dplyr::mutate(feature_obs_range = (.data$feature_raw - .data$quantile_05_obs) /
                      (.data$quantile_95_obs - .data$quantile_05_obs)) %>%
      dplyr::filter(.data$feature_obs_range >= 0 &
                      .data$feature_obs_range <= 1)
  }

  x_feature <- if (only_observed) "feature_obs_range" else "feature_range"

  aes <- if (is.null(model_2))
    ggplot2::aes_string(x = x_feature, y = "effect") else
      ggplot2::aes_string(x = x_feature, y = "effect", linetype = "model")

  p <- marginals %>%
    dplyr::left_join(viewpoint_labels, by = "viewpoint") %>%
    ggplot2::ggplot(aes)

  if (!only_observed)
    p <- p +
    ggplot2::geom_rect(data = df_quantiles,
                       mapping = ggplot2::aes_string(xmin = "x_min",
                                                     xmax = "x_max",
                                                     ymin = -Inf,
                                                     ymax = Inf),
                       inherit.aes = FALSE,
                       fill = fill,
                       alpha = alpha)

  p <- p +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(x_lab) +
    ggplot2::scale_y_continuous(y_lab) +
    ggplot2::facet_wrap(~ viewpoint_label, ...)

  if (!is.null(model_2))
    p <- p + ggplot2::scale_linetype_manual("Model",
                                            values = c("solid", "dotted"))

  p
}

#' @export
list_viewpoints <- function(x, discrete = TRUE, continuous = TRUE) {
  UseMethod("list_viewpoints")
}

#' @export
list_viewpoints.viewpoint_regression <- function(x,
                                                 discrete = TRUE,
                                                 continuous = TRUE) {
  pred <- x$predictors
  if (!discrete) pred <- dplyr::filter(pred, !.data$discrete)
  if (!continuous) pred <- dplyr::filter(pred, .data$discrete)
  pred$viewpoint %>% unique()
}


# plot_marginal <- function(x,
#                           viewpoint,
#                           gg = FALSE,
#                           title = viewpoint,
#                           x_lab = "Feature value (z-score)",
#                           y_lab = "Effect",
#                           reverse_x = FALSE) {
#   UseMethod("plot_marginal")
# }
#
# plot_marginal.viewpoint_regression <- function(
#   x,
#   viewpoint,
#   gg = FALSE,
#   title = viewpoint,
#   x_lab = "Feature value (z-score)",
#   y_lab = "Log odds",
#   reverse_x = FALSE
# ) {
#   dat <- get_marginal(x, viewpoint = viewpoint)
#   if (reverse_x) dat$effect <- dat$effect * - 1
#
#   if (gg) {
#     if (!requireNamespace("ggplot2", quietly = TRUE))
#       stop("please install the ggplot2 package before using this function")
#     p <- ggplot2::ggplot(dat, ggplot2::aes_string("feature_z", "effect")) +
#       ggplot2::geom_line() +
#       ggplot2::scale_x_continuous(x_lab) +
#       ggplot2::scale_y_continuous(y_lab)
#     if (!is.null(title)) p <- p + ggplot2::ggtitle(title)
#     p
#
#   } else {
#     plot(dat$feature_z,
#          dat$effect,
#          type = "l",
#          xlab = x_lab,
#          ylab = y_lab,
#          main = viewpoint)
#   }
# }

# predict_symbols <- function(model_matrix, par, observed_only = TRUE) {
#   model_matrix %>%
#     dplyr::group_by(seq_id, event_id) %>%
#     dplyr::group_split() %>%
#     plyr::llply(predict_symbol, par, observed_only, .progress = "text") %>%
#     dplyr::bind_rows()
# }
#
# predict_symbol <- function(x, par, observed_only) {
#   checkmate::qassert(observed_only, "B1")
#   if (!observed_only) stop("observed_only = FALSE is not yet implemented")
#   stopifnot(all(x$symbol == seq_len(nrow(x))))
#   if (is.null(names(par)) || any(!names(par) %in% names(x)))
#     stop("'par' must be named, and all names must be present in model matrix")
#
#   mat <- dplyr::select(x, names(par)) %>% as.matrix()
#   exp_energies <- exp(mat %*% par)
#
#   observed <- which(x$observed)
#   if (length(observed) > 1) stop("cannot have multiple observed events")
#   if (length(observed) < 1) stop("cannot have zero observed events")
#   if (!x$legal[observed]) stop("cannot observe an illegal event")
#
#   seq_id <- unique(x$seq_id)
#   event_id <- unique(x$event_id)
#   stopifnot(length(seq_id) == 1L,
#             length(event_id) == 1L)
#
#   tibble(
#     seq_id,
#     event_id,
#     symbol = observed,
#     probability = exp_energies[observed] / sum(exp_energies[x$legal, ])
#   )
# }

conduct_regression <- function(observation_matrix, continuation_matrices, legal,
                               corpus, predictors, poly_degree, poly_coefs, moments,
                               max_iter, perm_int, perm_int_seed, perm_int_reps,
                               viewpoint_labels, allow_negative_weights) {

  lower_bound <- if (allow_negative_weights)
    rep(-Inf, times = nrow(predictors)) else
      dplyr::if_else(predictors$discrete, 0, -Inf)

  optim_method <- if (allow_negative_weights) "BFGS" else "L-BFGS-B"
  # par_scale <- NULL
  par_scale <- dplyr::if_else(predictors$discrete, 1, 1000)

  i <- 0L
  get_cost <- function(...) {
    x <- cost(...)
    i <<- i + 1L
    message("i = ", i, ", cost = ", x)
    x
  }

  message("Fitting conditional logit model with optimiser ",
          optim_method, "...")

  x <- optim(par = rep(0, times = nrow(predictors)),
             fn = get_cost,
             gr = gradient,
             method = optim_method,
             lower = lower_bound,
             observation_matrix = observation_matrix,
             continuation_matrices = continuation_matrices,
             legal = legal,
             control = list(maxit = max_iter,
                            parscale = par_scale))
  new_regression_model(par = x$par, cost = x$value,
                       corpus = corpus,
                       observation_matrix = observation_matrix,
                       continuation_matrices = continuation_matrices,
                       legal = legal,
                       predictors = predictors,
                       perm_int = perm_int,
                       perm_int_seed = perm_int_seed,
                       perm_int_reps = perm_int_reps,
                       poly_degree = poly_degree,
                       poly_coefs = poly_coefs,
                       moments = moments,
                       viewpoint_labels = viewpoint_labels,
                       optim_method = optim_method,
                       par_scale = par_scale,
                       lower_bound = lower_bound,
                       optim_report = x)

}

#' @export
new_regression_model <- function(par, cost, corpus, observation_matrix,
                                 continuation_matrices, legal, predictors,
                                 perm_int, perm_int_seed, perm_int_reps,
                                 poly_degree, poly_coefs, moments,
                                 viewpoint_labels, optim_method, par_scale,
                                 lower_bound, optim_report) {
  permutation_importance <- if (perm_int)
    get_perm_int(weights = par,
                 benchmark_cost = cost,
                 corpus = corpus,
                 observation_matrix = observation_matrix,
                 continuation_matrices = continuation_matrices,
                 legal = legal,
                 predictors = predictors,
                 seed = perm_int_seed,
                 reps = perm_int_reps)

  res <- list(predictors = predictors,
              par = par %>% magrittr::set_names(predictors$label),
              poly_degree = poly_degree,
              poly_coefs = poly_coefs,
              moments = moments,
              cost = cost,
              cost_benchmarks = get_cost_benchmarks(predictors,
                                                    observation_matrix),
              perm_int = permutation_importance,
              viewpoint_labels = viewpoint_labels,
              optim_method = optim_method,
              par_scale = par_scale,
              lower_bound = lower_bound,
              optim_report = optim_report
  )
  class(res) <- c("viewpoint_regression", "list")
  res
}

get_cost_benchmarks <- function(predictors,
                                observation_matrix) {
  pred <- predictors %>%
    dplyr::filter(.data$discrete) %>%
    dplyr::pull("label")
  observation_matrix[, pred] %>%
    colMeans() %>%
    magrittr::multiply_by(-1)
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

get_regression_predictors <- function(model_matrix_dir) {
  readRDS(file.path(model_matrix_dir, "predictors.rds"))
}


get_regression_corpus <- function(model_matrix_dir) {
  readRDS(file.path(model_matrix_dir, "corpus.rds"))
}
