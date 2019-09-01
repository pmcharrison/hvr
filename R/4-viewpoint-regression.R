#' Conduct viewpoint regression
#'
#' Fits a viewpoint regression model.
#' The following routines must be run first:
#' 1. \code{\link{compute_viewpoints}}
#' 2. \code{\link{compute_ppm_analyses}}
#' 3. \code{\link{compute_model_matrix}}
#'
#' The optimisation method is "BFGS" if \code{allow_negative_weights}
#' is \code{TRUE} and "L-BFGS-B" otherwise (see \code{\link[stats]{optim}}).
#'
#' @param parent_dir
#' (Character scalar)
#' The parent directory for the output files, shared with functions such as
#' \code{\link{compute_viewpoints}} and \code{\link{compute_ppm_analyses}}.
#' Ignored if all other directory arguments are manually specified.
#'
#' @param max_iter
#' (Integer scalar)
#' Maximum number of iterations for the optimisation routine.
#'
#' @param perm_int
#' (Logical scalar)
#' Whether to compute permutation-based feature importances.
#'
#' @param perm_int_seed
#' (Integer scalar)
#' Random seed for the permutation-based feature importances.
#'
#' @param perm_int_reps
#' (Integer scalar)
#' Number of replicates for the permutation-based feature importances
#' (the final estimates are averages over these replicates).
#'
#' @param allow_negative_weights
#' (Logical scalar)
#' Whether negative weights should be allowed for discrete features
#' (\code{FALSE} by default).
#'
#' @param viewpoint_dir
#' (Character scalar)
#' The directory for the already-generated
#' output files from \code{\link{compute_viewpoints}}.
#' The default should be correct if the user used the
#' default \code{dir} argument in \code{\link{compute_viewpoints}}.
#'
#' @param model_matrix_dir
#' (Character scalar)
#' The directory for the already-generated
#' output files from \code{\link{compute_model_matrix}}.
#' The default should be correct if the user used the
#' default \code{dir} argument in \code{\link{compute_model_matrix}}.
#'
#' @param output_dir
#' (Character scalar)
#' The output directory for the viewpoint regression results.
#' Will be created if it doesn't exist already.
#'
#' @return
#' The primary output is a viewpoint regression model object
#' written to disk in the \code{dir} directory.
#' This object contains the fitted viewpoint regression weights
#' and feature importance analyses.
#' Various plots may be constructed from this object:
#' - \code{\link{plot_costs}}
#' - \code{\link{plot_perm_int}}
#' - \code{\link{plot_marginals}}
#' - \code{\link{plot_discrete_weights}}
#'
#' @md
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

#' Testing for viewpoint regression objects
#'
#' Tests whether a given object is a member of the class
#' \code{viewpoint_regression}.
#'
#' @param x Object to test.
#'
#' @return Logical scalar.
#'
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

add_data <- function(x, data) {
  if (is.null(attr(x, "data")))
    attr(x, "data") <- data else
      warning("cannot use 'data' slot as it is already taken")
  x
}

#' Get discrete weights
#'
#' Extracts weights for discrete viewpoints.
#'
#' @param Object from which weights should be extracted.
#'
#' @return A \code{\link[tibble]{tibble}}.
#'
#' @rdname get_discrete_weights
#' @export
get_discrete_weights <- function(x) {
  UseMethod("get_discrete_weights")
}

#' @rdname get_discrete_weights
#' @export
get_discrete_weights.viewpoint_regression <- function(x) {
  viewpoints <- list_viewpoints(x, continuous = FALSE)
  tibble(label = names(x$par),
         par = as.numeric(x$par)) %>%
    dplyr::left_join(x$predictors, by = "label") %>%
    dplyr::filter(.data$discrete) %>%
    dplyr::left_join(x$viewpoint_labels, by = "viewpoint")
}

#' Plot weights for discrete viewpoints
#'
#' Plots regression weights for discrete viewpoints
#' in a viewpoint regression model.
#'
#' @param x Viewpoint regression model object,
#' as created by \code{\link{viewpoint_regression}}.
#'
#' @param x_lab Label for x axis.
#'
#' @param y_lab Label for y axis.
#'
#' @param colours Vector of colours to use.
#'
#' @param labels \code{\link[tibble]{tibble}} providing
#' textual labels for the viewpoints.
#'
#' @rdname plot_discrete_weights
#'
#' @export
plot_discrete_weights <- function(x,
                                  x_lab = "Viewpoint",
                                  y_lab = "Weight",
                                  colours = c("#B50000", "#11A3FF"),
                                  labels = x$viewpoint_labels) {
  UseMethod("plot_discrete_weights")
}

#' @rdname plot_discrete_weights
#'
#' @export
plot_discrete_weights.viewpoint_regression <- function(
  x,
  x_lab = "Viewpoint",
  y_lab = "Weight",
  colours = c("#B50000", "#11A3FF"),
  labels = x$viewpoint_labels
) {
  if (!requireNamespace("ggplot2")) stop("ggplot2 must be installed first")

  data <- get_discrete_weights(x) %>%
    dplyr::mutate(
      class = dplyr::recode_factor(.data$class,
                                   stm = "Short-term",
                                   ltm = "Long-term"),
      viewpoint_label = plyr::revalue(
        .data$viewpoint,
        labels$viewpoint_label %>% stats::setNames(labels$viewpoint),
        warn_missing = FALSE
      ),
      viewpoint_label = factor(.data$viewpoint_label,
                               levels = sort(unique(.data$viewpoint_label),
                                             decreasing = TRUE))
    )

  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = "viewpoint_label",
                                                 y = "par",
                                                 fill = "class")) +
    ggplot2::geom_col(colour = "black", width = 0.9) +
    ggplot2::scale_fill_manual(NULL, values = colours) +
    ggplot2::scale_x_discrete(x_lab) +
    ggplot2::scale_y_continuous(y_lab) +
    ggplot2::coord_flip()

  add_data(p, data)
}

#' Get marginals
#'
#' Get marginal effects of continuous viewpoints.
#'
#' @param x Object from which to extract the marginal effects.
#'
#' @return
#' A list with two elements:
#' - \code{marginals} - A \code{\link[tibble]{tibble}} of marginal effects
#' for the continuous viewpoints, as derived by \code{\link{get_marginal}}.
#' - \code{moments} - A \code{\link[tibble]{tibble}} providing
#' summary statistics for the different viewpoints
#' (mean, standard deviation, 5th percentile, 95th percentile,
#' minimum, maximum).
#'
#' @md
#' @rdname get_marginals
#' @export
get_marginals <- function(x) {
  UseMethod("get_marginals")
}

#' @rdname get_marginals
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

#' Get marginal
#'
#' Get the marginal effect of a continuous viewpoint.
#'
#' @param x Object from which to extract the marginal effect.
#'
#' @param viewpoint Character string identifying the viewpoint
#' whose marginal effect should be extracted.
#'
#' @return
#' A \code{\link[tibble]{tibble}} with the following columns:
#' - \code{feature_rel} - Relative feature value,
#' where 0 is the minimum theoretically possible feature value found when
#' computing the model matrix (i.e. looking over the full alphabet
#' of possible chord continuations),
#' and 1 is the maximum such value.
#' - \code{feature_raw} - Absolute feature value.
#' - \code{feature_obs} - \code{TRUE} if the feature value falls within
#' the range of actually observed feature values in the model matrix.
#' - \code{effect} - The resulting effect, i.e the resulting
#' contribution to the model's linear predictor.
#'
#' @md
#'
#' @rdname get_marginal
#' @export
get_marginal <- function(x, viewpoint) {
  UseMethod("get_marginal")
}

#' @rdname get_marginal
#' @export
get_marginal.viewpoint_regression <- function(x, viewpoint) {
  checkmate::qassert(viewpoint, "S1")
  if (!viewpoint %in% list_viewpoints(x, discrete = FALSE))
    stop("can only plot marginals for continuous viewpoints")

  ranges_raw <- c("observed", "all_legal") %>%
    purrr::set_names(., .) %>%
    purrr::map(~ x$moments[[.]][[viewpoint]]$quantiles[c("5%", "95%")])

  feature_range_raw <- c(pmin(ranges_raw$observed[1],
                              ranges_raw$all_legal[1]),
                         pmax(ranges_raw$observed[2],
                              ranges_raw$all_legal[2]))
  feature_seq_raw <- seq(from = feature_range_raw[1],
                         to = feature_range_raw[2],
                         length.out = 1000)
  feature_seq_rel <- seq(from = 0, to = 1, length.out = 1000)

  poly_coef <- x$poly_coefs[[viewpoint]]
  poly_degree <- poly_coef$alpha %>% length()
  feature_poly_seq <- poly(feature_seq_raw,
                           degree = poly_degree,
                           coefs = poly_coef)[, seq_len(x$poly_degree), drop = FALSE]
  par <- x$predictors %>%
    dplyr::filter(.data$viewpoint == !!viewpoint) %>%
    dplyr::arrange(.data$poly_degree) %>%
    dplyr::pull(.data$label) %>%
    {x$par[.]}

  feature_effect_seq <- as.numeric(feature_poly_seq %*% matrix(par, ncol = 1))

  res <- tibble(feature_rel = feature_seq_rel,
                feature_raw = feature_seq_raw,
                feature_obs = feature_seq_raw >= ranges_raw$observed[1] &
                  feature_seq_raw <= ranges_raw$observed[2],
                effect = feature_effect_seq)

  attr(res, "moments") <- list(observed = x$moments$observed[[viewpoint]],
                               all_legal = x$moments$all_legal[[viewpoint]])

  res
}

#' Get costs
#'
#' Returns the observed costs for the different
#' discrete viewpoints as well as the cost of
#' the full viewpoint regression model.
#'
#' @param x Viewpoint regression model as created with
#' \code{\link{viewpoint_regression}}.
#'
#' @rdname get_costs
#' @export
get_costs <- function(x) {
  UseMethod("get_costs")
}

#' @rdname get_costs
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

#' Plot costs
#'
#' Plots observed costs for a viewpoint regression
#' as extracted with \code{\link{get_costs}}.
#'
#' @param x Viewpoint regression model as created with
#' \code{\link{viewpoint_regression}}.
#'
#' @param x_lab Label for the x axis.
#'
#' @param y_lab Label for the y axis.
#'
#' @param factor_lab Label for 'model type' factor.
#'
#' @param factor_col Colours for 'model type' factor.
#'
#' @rdname plot_costs
#' @export
plot_costs <- function(x,
                       x_lab = "Cost (bits/chord)",
                       y_lab = "Model",
                       factor_lab = "Model type",
                       factor_col = c("#E8E410", "#11A3FF", "#B50000")) {
  UseMethod("plot_costs")
}

#' @rdname plot_costs
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

#' Plot permutation-based feature importances
#'
#' Plots permutation-based feature importances
#' for a viewpoint regression as extracted with \code{\link{get_perm_int}}.
#'
#' @param x Viewpoint regression model as created with
#' \code{\link{viewpoint_regression}}.
#'
#' @param labels \code{\link[tibble]{tibble}} of viewpoint labels.
#'
#' @param axis_label
#' (Character scalar)
#' Axis label.
#'
#' @param order_by_label
#' (Logical scalar)
#' If \code{TRUE}, bars are ordered by viewpoint label,
#' otherwise they are ordered by quantity.
#'
#' @param error_bars
#' (Logical scalar)
#' Whether or not to plot error bars, which will correspond to the
#' 2.5th/97.5th percentiles of the permutation-importance replicates.
#' These will only be stable for relatively large numbers of replicates
#' (50+).
#'
#' @param fill
#' (Character scalar)
#' Colour for the bars.
#'
#' @rdname plot_perm_int
#' @export
plot_perm_int <- function(
  x,
  labels = x$viewpoint_labels,
  axis_label = "Feature importance (bits/chord)",
  order_by_label = FALSE,
  error_bars = FALSE,
  fill = "#6ba3ff",
  ...
) {
  UseMethod("plot_perm_int")
}

#' @rdname plot_perm_int
#' @export
plot_perm_int.viewpoint_regression <- function(
  x,
  labels = x$viewpoint_labels,
  axis_label = "Feature importance (bits/chord)",
  order_by_label = FALSE,
  error_bars = FALSE,
  fill = "#6ba3ff"
) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 must be installed first")

  data <- x$perm_int %>%
    dplyr::mutate(viewpoint = plyr::revalue(
      .data$viewpoint,
      labels$viewpoint_label %>% stats::setNames(labels$viewpoint),
      warn_missing = FALSE
    )) %>%
    dplyr::arrange(if (order_by_label) viewpoint else mean) %>%
    dplyr::mutate(viewpoint = factor(.data$viewpoint, levels = rev(.data$viewpoint)))

  p <- data %>%
    ggplot2::ggplot(ggplot2::aes_string(x = "viewpoint",
                                        y = "mean",
                                        ymin = "ci_95_low",
                                        ymax = "ci_95_high")) +
    ggplot2::geom_bar(colour = "black", fill = fill, stat = "identity") +
    ggplot2::scale_x_discrete(NULL) +
    ggplot2::scale_y_continuous(axis_label) +
    ggplot2::coord_flip()

  if (error_bars) p <- p + ggplot2::geom_errorbar(width = 0.25)

  add_data(p, data)
}

#' Plot marginals
#'
#' Plots the marginal effects for continuous viewpoints in a
#' viewpoint regression model, as extracted by
#' \code{\link{get_marginal}}.
#'
#' The x axis spans the 5th-95th percentiles
#' of theoretically possible feature values
#' as computed in the derivation of the model matrix
#' (see \code{\link{compute_model_matrix}}).
#' The shaded area identifies the 5th-95th percentiles
#' of observed feature values when computing the model matrix.
#'
#' @param model_1
#' Viewpoint regression model,
#' as created by \code{\link{viewpoint_regression}}.
#'
#' @param model_2
#' Optional second viewpoint regression model,
#' to be plotted for comparison.
#'
#' @param model_labels
#' Only relevant if a second viewpoint regression model is provided,
#' in which case this should be a character vector of length 2
#' corresponding to the plot labels for these two models.
#'
#' @param x_lab
#' (Character scalar)
#' Label for the x axis.
#'
#' @param y_lab
#' (Character scalar)
#' Label for the y axis.
#'
#' @param viewpoint_labels
#' \code{\link[tibble]{tibble}} of viewpoint labels.
#'
#' @param fill
#' (Character scalar)
#' Fill colour for the shaded region identifying the 5th-95th percentiles
#' of observed feature values.
#'
#' @param alpha
#' (Numeric scalar)
#' Alpha value for the shaded region.
#'
#' @param scales
#' Passed to \code{\link[ggplot2]{facet_wrap}}.
#'
#' @param ...
#' Further arguments to pass to \code{\link[ggplot2]{facet_wrap}}.
#'
#' @export
plot_marginals <- function(model_1,
                           model_2 = NULL,
                           model_labels = NULL,
                           x_lab = "Feature value",
                           y_lab = "Effect",
                           viewpoint_labels = x$viewpoint_labels,
                           fill = "blue",
                           alpha = 0.25,
                           scales = "free",
                           ...) {
  UseMethod("plot_marginals")
}

#' @export
plot_marginals.viewpoint_regression <- function(model_1,
                                                model_2 = NULL,
                                                model_labels = NULL,
                                                x_lab = "Feature value",
                                                y_lab = "Effect",
                                                viewpoint_labels = model_1$viewpoint_labels,
                                                fill = "blue",
                                                alpha = 0.25,
                                                scales = "free",
                                                ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 must be installed first")

  stopifnot(is.data.frame(viewpoint_labels),
            all(names(viewpoint_labels) == c("viewpoint", "viewpoint_label")),
            !anyDuplicated(viewpoint_labels$viewpoint),
            !anyDuplicated(viewpoint_labels$viewpoint_label))
  if (!is.null(model_2)) {
    stopifnot(is_viewpoint_regression(model_2))
    checkmate::qassert(model_labels, "S2")
  }

  if (all(model_1$predictors$discrete)) {
    warning("no continuous marginals available to plot")
    return(ggplot2::ggplot())
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

  marginals <- dplyr::left_join(marginals, viewpoint_labels, by = "viewpoint")

  obs_ranges_raw <- marginals %>%
    dplyr::group_by(.data$viewpoint) %>%
    dplyr::summarise(min = min(feature_raw[feature_obs]),
                     max = max(feature_raw[feature_obs])) %>%
    dplyr::left_join(viewpoint_labels, by = "viewpoint")

  aes <- if (is.null(model_2))
    ggplot2::aes_string(x = "feature_raw", y = "effect") else
      ggplot2::aes_string(x = "feature_raw", y = "effect", linetype = "model")

  p <- marginals %>%
    ggplot2::ggplot(aes) +
    ggplot2::geom_rect(data = obs_ranges_raw,
                       mapping = ggplot2::aes_string(xmin = "min",
                                                     xmax = "max",
                                                     ymin = -Inf,
                                                     ymax = Inf),
                       inherit.aes = FALSE,
                       fill = fill,
                       alpha = alpha) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(x_lab) +
    ggplot2::scale_y_continuous(y_lab) +
    ggplot2::facet_wrap(~ viewpoint_label, scales = scales, ...)

  if (!is.null(model_2))
    p <- p + ggplot2::scale_linetype_manual("Model",
                                            values = c("solid", "dotted"))

  add_data(p, marginals)

}

#' List viewpoints
#'
#' Lists the viewpoints in a viewpoint regression model.
#'
#' @param x Viewpoint regression model as created with
#' \code{\link{viewpoint_regression}}.
#'
#' @param discrete
#' (Logical scalar)
#' Whether to include discrete viewpoints.
#'
#' @param continuous
#' (Logical scalar)
#' Whether to include continuous viewpoints.
#'
#' @return
#' A character vector of viewpoint identifiers.
#'
#' @rdname list_viewpoints
#' @export
list_viewpoints <- function(x, discrete = TRUE, continuous = TRUE) {
  UseMethod("list_viewpoints")
}

#' @rdname list_viewpoints
#' @export
list_viewpoints.viewpoint_regression <- function(x,
                                                 discrete = TRUE,
                                                 continuous = TRUE) {
  pred <- x$predictors
  if (!discrete) pred <- dplyr::filter(pred, !.data$discrete)
  if (!continuous) pred <- dplyr::filter(pred, .data$discrete)
  pred$viewpoint %>% unique()
}

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

  new_regression_model(par = x$par,
                       cost = x$value,
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

new_regression_model <- function(par, cost,
                                 corpus, observation_matrix,
                                 continuation_matrices, legal, predictors,
                                 perm_int, perm_int_seed, perm_int_reps,
                                 poly_degree, poly_coefs, moments,
                                 viewpoint_labels, optim_method, par_scale,
                                 lower_bound, optim_report) {

  num_events <- length(continuation_matrices)
  num_par <- length(par)
  log_2_likelihood <- - cost * num_events
  log_e_likelihood <- log_2_likelihood * log(2)
  aic <- 2 * num_par - 2 * log_e_likelihood
  bic <- log(num_events) * num_par - 2 * log_e_likelihood

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
              num_events = num_events,
              par = par %>% magrittr::set_names(predictors$label),
              poly_degree = poly_degree,
              poly_coefs = poly_coefs,
              moments = moments,
              cost = cost,
              cost_benchmarks = get_cost_benchmarks(predictors,
                                                    observation_matrix),
              log_2_likelihood = log_2_likelihood,
              log_e_likelihood = log_e_likelihood,
              aic = aic,
              bic = bic,
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
  observation_matrix[, pred, drop = FALSE] %>%
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
  plyr::llply(viewpoints, function(v) {
    withr::with_seed(seed, {
      plyr::laply(seq_len(reps), function(...) {
        tmp <-  permute_matrices(observation_matrix,
                                 continuation_matrices,
                                 corpus,
                                 predictors,
                                 v)
        cost(weights, tmp$new_obs_matrix, tmp$new_con_matrices, legal) - benchmark_cost
      })
    }) %>% (function(x) {
      tibble::tibble(viewpoint = v,
                     mean = mean(x),
                     n = length(x),
                     sd = sd(x),
                     ci_95_low =  as.numeric(quantile(x, 0.025)),
                     ci_95_high = as.numeric(quantile(x, 0.975)))
    })
  }, .progress = "time") %>%
    dplyr::bind_rows()
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

get_regression_predictors <- function(model_matrix_dir) {
  readRDS(file.path(model_matrix_dir, "predictors.rds"))
}


get_regression_corpus <- function(model_matrix_dir) {
  readRDS(file.path(model_matrix_dir, "corpus.rds"))
}
