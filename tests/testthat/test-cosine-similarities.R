context("test-cosine-similarities")

test_that("non-musical examples", {
  x <- rnorm(10)
  y <- rnorm(10)

  expect_equal(
    cosine_similarity(x, y),
    hrep::cosine_similarity(x, y)
  )

  z <- matrix(rnorm(100), ncol = 10)
  expect_equal(
    apply(z, 2, cosine_similarity, x),
    cosine_similarities(x, z)
  )
})

test_that("musical example", {
  chord <- hrep::pc_chord(c(0, 4, 7))
  spectrum <- hrep::milne_pc_spectrum(chord)
  sim <- cosine_similarities(spectrum, hvrmap::map_pc_chord$milne_pc_spectrum)

  chord_ids <- c(1, 200, 450, 15000)
  purrr::map(chord_ids, hrep::decode_pc_chord) %>%
    purrr::map(hrep::milne_pc_spectrum) %>%
    purrr::map_dbl(hrep::cosine_similarity, spectrum) %>%
    expect_equal(sim[chord_ids])
})
