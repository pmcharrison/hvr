context("test-cosine-similarities")

test_that("examples", {
  x <- rnorm(10)
  y <- rnorm(10)

  expect_equal(
    cosine_similarity(x, y),
    hrep::cosine_similarity(x, y)
  )

  z <- matrix(rnorm(100), ncol = 10)
  expect_equal(
    apply(z, 1, cosine_similarity, x),
    cosine_similarities(x, z)
  )
})
