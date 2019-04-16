context("test-folds")

test_that("example 1", {
  folds <- list(
    1:5,
    6:10,
    11:15
  )
  get_test(folds, 1) %>% expect_equal(1:5)
  get_test(folds, 2) %>% expect_equal(6:10)
  get_test(folds, 3) %>% expect_equal(11:15)

  get_training(folds, 1) %>% expect_equal(6:15)
  get_training(folds, 2) %>% expect_equal(c(1:5, 11:15))
  get_training(folds, 3) %>% expect_equal(1:10)
})

test_that("example 2", {
  folds <- list(
    c(1, 4, 3, 2, 5),
    c(7, 6, 8, 9, 10),
    c(15, 14, 13, 12, 11)
  )
  get_test(folds, 1) %>% expect_equal(1:5)
  get_test(folds, 2) %>% expect_equal(6:10)
  get_test(folds, 3) %>% expect_equal(11:15)

  get_training(folds, 1) %>% expect_equal(6:15)
  get_training(folds, 2) %>% expect_equal(c(1:5, 11:15))
  get_training(folds, 3) %>% expect_equal(1:10)
})
