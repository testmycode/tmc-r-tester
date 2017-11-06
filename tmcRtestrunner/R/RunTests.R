# Runs the tests from project directory and writes results JSON to the root of the project
# as .tmc_results.json.
#
# Args:
#  project_path: The absolute path to the root of the project being tested.
#  print: If TRUE, prints results; if not, not. DEFAULT is FALSE.
#
# Returns:
#   Run results list containing: runStatus (string), backtrace (list), test_results (list)
run_tests <- function(project_path = getwd(), print=FALSE) {
  tmc_r_test_runner_project_path <- project_path

  #Runs tests for project and returns the results.
  #If sourcing_error occurs, .sourcing_error_run_results returns the results.
  run_results <- tryCatch({.run_tests_project(project_path)},
                           sourcing_error = .sourcing_error_run_result)

  json_run_results <- .create_json_run_results(run_results)
  .write_json(json_run_results, ".results.json")

  if (print) {
    .print_results_from_json(json_run_results)
  }

  setwd(tmc_r_test_runner_project_path)

  invisible(run_results)
}

.run_tests_project <- function(project_path) {
  setwd(project_path)
  test_results <- list()
  #Lists all the files in the path beginning with "test" and ending in ".R"
  test_files <- list.files(path = "tests/testthat", pattern = "test.*\\.R", full.names = T, recursive = FALSE)

  for (test_file in test_files) {
    file_results <- .run_tests_file(test_file, test_env)
    test_results <- c(test_results, file_results)
  }
  return(list("run_status" = "success", "backtrace" = list(), "test_results" = test_results))
}

.create_test_env <- function() {
  test_env <- new.env()
  .define_tester_functions(test_env)

  tryCatch({.source_files(test_env)},
           error = .signal_sourcing_error)
  return (test_env)
}

.create_test_env_file <- function(test_file) {
  test_env <- new.env()
  .define_tester_functions(test_env)
  tryCatch({.source_from_test_file(test_file, test_env)},
           error = .signal_sourcing_error)
  return (test_env)
}

.define_tester_functions <- function(test_env) {
  test_env$points_for_all_tests <- function(points) {
    .GlobalEnv$points_for_all_tests <- points
  }

  #The test that wraps around test_that()-method and stores the points
  #to global environment.
  test_env$test <- function(desc, points, code) {
    .GlobalEnv$points[[desc]] <- points
    test_that(desc, code)
  }
}

.source_files <- function(test_env) {
  for (file in list.files(pattern = "[.]R$", path = "R/", full.names = TRUE)) {
    sys.source(file, test_env)
  }
}

.source_from_test_file <- function(test_location, test_env) {
  script_name <- basename(test_location)
  script_name <- substr(script_name, 5, nchar(script_name))
  source_folder <- "R/"
  # Checks whether list is empty and if it is, modifies the first letter of the script to lower case.
  if (length(list.files(path = source_folder, pattern = script_name, full.names = T, recursive = FALSE)) == 0) {
    substr(script_name, 1, 1) <- tolower(substr(script_name, 1, 1))
  }
  sys.source(paste0(source_folder, script_name), test_env)
}

.run_tests_file <- function(file_path, env1) {
  .GlobalEnv$points <- list()
  .GlobalEnv$points_for_all_tests <- list()

  test_file_output <- test_file(file_path, reporter = "silent", env = .create_test_env())

  test_file_results <- .create_file_results(test_file_output, points, .GlobalEnv$points_for_all_tests)

  return(test_file_results)
}

# TODO: add backtrace from error to sourcing_error
.signal_sourcing_error <- function(error) {
  sourcing_error <- simpleError("")
  class(sourcing_error) <- c("sourcing_error", class(sourcing_error))
  signalCondition(sourcing_error)
}

.sourcing_error_run_result <- function(sourcing_error) {
  #TODO: implement backtrace and extract from sourcing_error
  return(list("run_status" = "sourcing_failed", "backtrace" = list(), "test_results" = list()))
}
