context("test-regression-implementation")

test_that("misc", {
  num_weights <- 4
  num_events <- 100
  alphabet_size <- 200

  weights <- rnorm(num_weights)
  observed <- sample(200, size = num_events, replace = TRUE)
  continuation_matrices <- purrr::map(seq_len(num_events), function(...) {
    matrix(rnorm(num_weights * alphabet_size),
           nrow = alphabet_size,
           ncol = num_weights)
  })
  observation_matrix <- purrr::map2_dfr(
    seq_len(num_events), observed,
    function(event, symbol) {
      continuation_matrices[[event]][symbol, ] %>%
        as.list() %>%
        as.data.frame() %>%
        magrittr::set_names(seq_len(num_weights))
    }) %>% as.matrix()

  cost_r <- function(weights, observation_matrix, continuation_matrices) {
    weights <- matrix(weights, ncol = 1)
    energies <- observation_matrix %*% weights
    partitions <- purrr::map_dbl(continuation_matrices,
                                 ~ sum(exp(. %*% weights)))
    probabilities <- exp(energies) / partitions
    - sum(log(probabilities))
  }

  expect_equal(
    cost_r(weights, observation_matrix, continuation_matrices),
    cost(weights, observation_matrix, continuation_matrices)
  )

  expect_equal(
    numDeriv::jacobian(cost, weights,
                       observation_matrix = observation_matrix,
                       continuation_matrices = continuation_matrices),
    gradient(weights, observation_matrix, continuation_matrices)
  )
})
