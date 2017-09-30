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


test_that("Test is reported to pass correctly", {
  test_output <- .for_testing_run_tests_project(simple_all_tests_pass_project_path)
  #All tests should return true:
  for (test in test_output) {
    expect_equal(.check_if_test_passed(test), TRUE)
  }
})

test_that("Test is reported to fail correctly", {
  test_output <- .for_testing_run_tests_project(simple_all_tests_fail_project_path)
  #All tests should return false
  for (test in test_output) {
    expect_equal(.check_if_test_passed(test), FALSE)
  }
})

test_that("Points are added correctly", {
  expected_result <- c("r1.1", "r1.2", "r1")
  name <- "point_testing"
  points <- list()
  points[[name]] <- c("r1.1", "r1.2")
  file_point <- c("r1")
  added_points <- .get_points_for_test(name, points, file_point)
  for (point in expected_result) {
    expect_equal(point %in% added_points, TRUE)
  }
  expect_equal(length(expected_result), length(added_points))
})

test_that("Status is added correctly for passing tests", {
  test_output <- .for_testing_run_tests_project(simple_all_tests_pass_project_path)
  #All tests should be given pass:
  for (test in test_output) {
    expect_equal(.get_status_for_test(test), "pass")
  }
})

test_that("Status is added correctly for failing tests", {
  test_output <- .for_testing_run_tests_project(simple_all_tests_fail_project_path)
  for (test in test_output) {
    expect_equal(.get_status_for_test(test), "fail")
  }
})

test_that("A message is given if the test fails", {
  test_output <- .for_testing_run_tests_project(simple_all_tests_fail_project_path)
  for (test in test_output) {
    expect_true(.create_message_for_test(test) != "")
  }
})

test_that("A message is not given if the test passes", {
  test_output <- .for_testing_run_tests_project(simple_all_tests_pass_project_path)
  for (test in test_output) {
    expect_true(.create_message_for_test(test) == "")
  }
})

test_that("A result is created correctly with status", {
  test_output <- .for_testing_run_tests_project(simple_all_tests_pass_project_path)
  results <- .create_file_results(test_output, points, points_for_all_tests)
  for (result in results) {
    expect_equal(result$status, "pass")
  }
})

test_that("A result is created correctly with name", {
  test_output <- .for_testing_run_tests_project(simple_all_tests_fail_project_path)
  results <- .create_file_results(test_output, points, points_for_all_tests)
  expect_equal(results[[1]]$name, "ret_true works")
  expect_equal(results[[2]]$name, "ret_one works")
  expect_equal(results[[3]]$name, "add works")
})
