context("test-project-to-basic")

test_that("examples", {
  project_to_basic(vp_dist = c(1, 0, 0, 0, 0),
          vp_vals = c(1L, 4L, 4L, 4L, 5L)) %>%
    expect_equal(c(1, 0, 0, 0, 0))

  project_to_basic(vp_dist = c(0, 0, 0, 1, 0),
          vp_vals = c(1L, 4L, 4L, 4L, 5L)) %>%
    expect_equal(c(0, 1/3, 1/3, 1/3, 0))

  project_to_basic(vp_dist = c(0.5, 0.25, 0.25),
          vp_vals = c(1L, 1L, 2L, 2L, 2L, 3L)) %>%
    expect_equal(c(0.25, 0.25, 0.25 / 3, 0.25 / 3, 0.25 / 3, 0.25))

  project_to_basic(vp_dist = c(0.5, 0.25, 0.25),
          vp_vals = c(2L, 3L, 3L, 3L))

  expect_error(project_to_basic(vp_dist = c(0.25, 0, 0, 0, 0),
                       vp_vals = c(2L, 4L, 4L, 4L, 5L)),
               "no available viewpoint values had non-zero probabilities")
})
