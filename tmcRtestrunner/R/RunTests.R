# Runs the tests from project directory and writes results JSON to the root of the project
# as .tmc_results.json.
#r
# Args:
#  project_path: The absolute path to the root of the project being tested.
#  print: If TRUE, prints results; if not, not. DEFAULT is FALSE.
#
run_tests <- function(project_path = getwd(), print=FALSE) {
  tmc_r_rest_runner_project_path <- project_path

  #runs test for project, returns testthatOuput with added points.
  test_results <- .run_tests_project(project_path)

  json_results <- .create_json_results(test_results)
  .write_json(json_results, ".results.json")

  if (print) {
    .print_results_from_json(json_results)
  }

  setwd(tmc_r_rest_runner_project_path)
}

.run_tests_project <- function(project_path) {
  setwd(project_path)

  test_results <- list()

  #Lists all the files in the path beginning with "test" and ending in ".R"
  test_files <- list.files(path = "tests/testthat", pattern = "test.*\\.R", full.names = T, recursive = FALSE)

  for (test_file in test_files) {
    file_results <- .run_tests_file(test_file)
    test_results <- c(test_results, file_results)
  }
  return(test_results)
}

.run_tests_file <- function(file_path) {
  .GlobalEnv$points <- list()
  .GlobalEnv$points_for_all_tests <- list()

  test_file_output <- test_file(file_path, reporter = "silent")

  test_file_results <- .create_file_results(test_file_output, points, .GlobalEnv$points_for_all_tests)

  return(test_file_results)
}

run_tests_with_default <- function(bol) {
  run_tests(getwd(), bol)
}

# Checks the available points for all test in the project without running test. Creates
# file .available_points.json in the project root.
run_available_points <- function(project_path) {
  tmc_r_rest_runner_project_path <- getwd()

  available_points <- .get_available_points(project_path)

  json_results <- .create_available_points_json_results(available_points)
  .write_json(json_results, ".available_points.json")

  setwd(tmc_r_rest_runner_project_path)
}
