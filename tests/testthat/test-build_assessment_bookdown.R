#'
#' @details The purpose is to check the build_assessment_bookdown function works
#' across a suite of models
#'

test_that("build_assessment_bookdown", {
  upper_dir = system.file("testdata", package = "r4Casal2", mustWork = TRUE)
  ## ComplexTag
  build_assessment_bookdown(csl_dir = file.path(upper_dir, "ComplexTag"), output_folder_name = "Bookdown", mpd_filename = "estimate_betadiff.log", config_filename = "config_betadiff.csl2", model_label = "ComplexTag")

  ## SBW
  build_assessment_bookdown(csl_dir = file.path(upper_dir, "SBW"), output_folder_name = "Bookdown", mpd_filename = "estimate_betadiff.log", config_filename = "config_betadiff.csl2", model_label = "SBW")

  ## TwoSex
  build_assessment_bookdown(csl_dir = file.path(upper_dir, "TwoSex"), output_folder_name = "Bookdown", mpd_filename = "estimate_betadiff.log", config_filename = "config_betadiff.csl2", model_label = "TwoSex")

  ## SexedLengthBased
  build_assessment_bookdown(csl_dir = file.path(upper_dir, "SexedLengthBased"), output_folder_name = "Bookdown", mpd_filename = "estimate_betadiff.log", config_filename = "config_betadiff.csl2", model_label = "SexedLengthBased")

})
