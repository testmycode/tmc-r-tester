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

remove_old_results_json <- function(project_path) {
  results_json_path <- paste(sep = "", project_path, "/.results.json")
  if (file.exists(results_json_path)) {
    file.remove(results_json_path)
  }
}

remove_old_available_points_json <- function(project_path) {
  available_points_json_path <- paste(sep = "", project_path, "/.available_points.json")
  if (file.exists(available_points_json_path)) {
    file.remove(available_points_json_path)
  }
}
