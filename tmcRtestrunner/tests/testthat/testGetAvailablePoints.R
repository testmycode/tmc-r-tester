test_resources_dir <- paste(sep = "", getwd(), "/resources")

#projects for testing:
simple_all_tests_fail_project_path <- paste(sep = "", test_resources_dir, "/simple_all_tests_fail")
simple_all_tests_pass_project_path <- paste(sep = "", test_resources_dir, "/simple_all_tests_pass")
simple_some_tests_fail_project_path <- paste(sep = "", test_resources_dir, "/simple_some_tests_fail")


.for_testing_run_tests_project <- function(project_path) {
  .GlobalEnv$points <- list()
  .GlobalEnv$points_for_all_tests <- list()
  setwd(project_path)

  test_results <- list()

  #Lists all the files in the path beginning with "test" and ending in ".R"
  test_files <- list.files(path = "tests/testthat", pattern = "test.*\\.R", full.names = T, recursive = FALSE)

  for (test_file in test_files) {
    test_results <- c(test_results, test_file(test_file, reporter = "silent"))
  }
  return(test_results)
}


test_that("Test", {
  #All tests should return true:
  test_points <- .get_available_points(simple_all_tests_pass_project_path)
  expect_equal(test_points[[1]], "r1")
  expect_equal(test_points[[2]], "r1.1")
  expect_equal(test_points[[3]], "r1.2")
  expect_equal(test_points[[4]], c("r1.3", "r1.4"))
})
