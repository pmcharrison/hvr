#' @include viewpoints.R
#' @include viewpoint-pc-chord-rel-prev-bass.R
#' @include viewpoint-spec-sim.R
#' @include viewpoint-root-int.R
#' @include viewpoint-pi-dist.R
#' @include viewpoint-pc-set-rel-prev-bass.R
#' @include viewpoint-bass-int.R
#' @include viewpoints-static.R
NULL

#' Viewpoint regression configurations
#'
#' Provides a list of viewpoint regression model configurations
#' as optimised in previous work.
#'
#' - \code{hvr_1} - Model optimised to predict the sixth chord in short
#' chord sequences extracted from the Billboard popular music dataset
#' (\code{\link[hcorp]{popular_1}}).
#'
#' @docType data
#' @keywords data
#' @md
#' @export
vr_config <- list()

vr_config$hvr_1 <- list(
  weights = c(stm_bass_int = 0.0681929385511573, ltm_bass_int = 0, stm_bass_pc = 0,
              ltm_bass_pc = 0.0846298394095975, stm_bass_pc_rel_root = 0.265703146574338,
              ltm_bass_pc_rel_root = 0.0968449396777696, stm_pc_chord = 0.119261050164929,
              ltm_pc_chord = 0.0986419610970866, stm_pc_chord_rel_prev_bass = 0.0599616858380255,
              ltm_pc_chord_rel_prev_bass = 0.191645545583769, stm_pc_set = 0.102585148521249,
              ltm_pc_set = 0, stm_pc_set_rel_bass = 0.0457486900082337, ltm_pc_set_rel_bass = 0.14610992918269,
              stm_pc_set_rel_prev_bass = 0, ltm_pc_set_rel_prev_bass = 0.0117849469953644,
              stm_pc_set_rel_root = 0.101267464862532, ltm_pc_set_rel_root = 0,
              stm_root_int = 0.212569302930506, ltm_root_int = 0.137111551716583,
              stm_root_pc = 0, ltm_root_pc = 0, har_18_harmonicity_degree_1 = 4213.33143209853,
              har_18_harmonicity_degree_2 = -3719.31271008152, har_18_harmonicity_degree_3 = 805.178528602796,
              har_18_harmonicity_degree_4 = -1573.24171664542, hutch_78_roughness_degree_1 = -4137.70988767465,
              hutch_78_roughness_degree_2 = 1787.99141711069, hutch_78_roughness_degree_3 = 1871.44116521322,
              hutch_78_roughness_degree_4 = -20.1793731837372, pi_dist_degree_1 = 1236.98442241967,
              pi_dist_degree_2 = 1001.06410878609, pi_dist_degree_3 = -63.8086390667929,
              pi_dist_degree_4 = 9.39218003653422, spec_sim_3_degree_1 = 932.022195552584,
              spec_sim_3_degree_2 = -1079.26313857747, spec_sim_3_degree_3 = 306.67735960496,
              spec_sim_3_degree_4 = 268.050561609695),
  viewpoints = hvr_viewpoints[setdiff(names(hvr_viewpoints), "num_pcs")],
  poly_degree = 4L,
  ppm_order = 5L
)
stopifnot(length(vr_config$hvr_1$viewpoints) == 15L)
