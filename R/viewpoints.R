.viewpoints <- list()

get_viewpoint <- function(x) .viewpoints[[x]]

new_viewpoint <- function(label, alphabet_size, discrete, f_obs, f_all) {
  checkmate::qassert(label, "S1")
  checkmate::qassert(alphabet_size, "x1")
  checkmate::qassert(discrete, "B1")
  stopifnot(is.function(f_obs))
  stopifnot(is.function(f_all))
  if (label %in% names(.viewpoints)) stop("viewpoint ", label, " already exists")
  .viewpoints[[label]] <<-
    list(
      alphabet_size = as.integer(alphabet_size),
      discrete = discrete,
      f_obs = f_obs,
      f_all = f_all
    )
  .viewpoints <<- .viewpoints[order(names(.viewpoints))]
}

is_viewpoint_discrete <- function(viewpoints) {
  checkmate::qassert(viewpoints, "S")
  invalid <- !viewpoints %in% names(.viewpoints)
  if (any(invalid))
    stop("the following viewpoint names are invalid: ",
         paste(viewpoints[invalid], collapse = ", "))
  purrr::map_lgl(viewpoints, ~ .viewpoints[[.]]$discrete)
}
