context("test-pc_dist")

test_that("examples", {
  pc_dist(2, 4) %>% expect_equal(2)
  pc_dist(4, 2) %>% expect_equal(2)
  pc_dist(0, 11) %>% expect_equal(1)
  pc_dist(11, 0) %>% expect_equal(1)
})
