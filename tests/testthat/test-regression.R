context("test-regression")

test_that("regression tests", {
  dir <- tempdir()

  corpus <- list(c(1, 2, 3, 2, 3),
                 c(1, 2, 1, 2, 1)) %>%
    purrr::map(hrep::coded_vec, "pc_chord") %>%
    hrep::corpus("pc_chord")

  capture.output({
    hvr::compute_viewpoints(
      corpus = corpus,
      parent_dir = dir,
      seq_test = 1,
      viewpoints = list(hvr_viewpoints$pc_chord,
                        hvr_viewpoints$hutch_78_roughness,
                        hvr_viewpoints$har_18_harmonicity)
    )
    hvr::compute_ppm_analyses(dir)
  })

  vp_about <- readRDS(file.path(dir, "0-viewpoints", "about.rds"))
  expect_equal(
    vp_about$viewpoint_labels,
    tibble(name = c("pc_chord", "hutch_78_roughness", "har_18_harmonicity"),
           label = c("PC chord", "Spectral interference", "Periodicity"))
  )

  ppm_res <- readRDS(file.path(dir, "1-ppm", "output", "1.rds"))
  stm_res <- ppm_res["stm_pc_chord", , ]
  ltm_res <- ppm_res["ltm_pc_chord", , ]

  stm_obs_probs <- purrr::map2_dbl(seq_along(corpus[[1]]),
                                   corpus[[1]],
                                   ~ stm_res[.x, .y])
  ltm_obs_probs <- purrr::map2_dbl(seq_along(corpus[[1]]),
                                   corpus[[1]],
                                   ~ ltm_res[.x, .y])

  # Reproduce LTM
  ltm_mod <- ppm::new_ppm_simple(hrep::alphabet_size("pc_chord"),
                                 update_exclusion = FALSE)
  ppm::model_seq(ltm_mod,
                 seq = corpus[[2]],
                 train = TRUE,
                 predict = FALSE)
  ltm_obs_probs_2 <- ppm::model_seq(ltm_mod,
                                    seq = corpus[[1]],
                                    train = FALSE,
                                    predict = TRUE,
                                    return_distribution = FALSE,
                                    return_entropy = FALSE) %>%
    dplyr::pull(information_content) %>%
    {2 ^ - (.)}

  expect_equal(ltm_obs_probs,
               ltm_obs_probs_2)

  # Reproduce STM
  stm_mod <- ppm::new_ppm_simple(hrep::alphabet_size("pc_chord"),
                                 update_exclusion = TRUE,
                                 escape = "ax")
  stm_obs_probs_2 <- ppm::model_seq(stm_mod,
                                    seq = corpus[[1]],
                                    train = TRUE,
                                    predict = TRUE,
                                    return_distribution = FALSE,
                                    return_entropy = FALSE) %>%
    dplyr::pull(information_content) %>%
    {2 ^ - (.)}

  expect_equal(stm_obs_probs,
               stm_obs_probs_2)

  capture.output({
    hvr::compute_model_matrix(dir)
    res <- hvr::viewpoint_regression(dir)
  })

  expect_true(!is.null(res$moments))
  expect_true(!is.null(res$poly_coefs))

  plot_marginals(res)
})
