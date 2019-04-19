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

  legal <- purrr::map(observed, function(obs) {
    x <- rep(TRUE, times = alphabet_size)
    ind <- sample(seq_len(alphabet_size)[- obs], 5)
    x[ind] <- FALSE
    x
  })

  cost_r <- function(weights, observation_matrix, continuation_matrices, legal) {
    weights <- matrix(weights, ncol = 1)
    energies <- observation_matrix %*% weights
    partitions <- purrr::map2_dbl(continuation_matrices, legal, function(m, l) {
      sum(exp(m[l, ] %*% weights))
    })
    probabilities <- exp(energies) / partitions
    - log2(exp(1)) * sum(log(probabilities)) / nrow(observation_matrix)
  }

  expect_equal(
    cost_r(weights, observation_matrix, continuation_matrices, legal),
    cost(weights, observation_matrix, continuation_matrices, legal)
  )

  expect_equal(
    numDeriv::jacobian(cost, weights,
                       observation_matrix = observation_matrix,
                       continuation_matrices = continuation_matrices,
                       legal = legal) %>%
      as.numeric(),
    gradient(weights, observation_matrix, continuation_matrices, legal)
  )
})
