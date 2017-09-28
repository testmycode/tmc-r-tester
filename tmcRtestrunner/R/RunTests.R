# Runs the tests from project directory and writes results JSON to the root of the project
# as .tmc_results.json.
#
# Args:
#  project_path: The absolute path to the root of the project being tested.
#  print: If TRUE, prints results; if not, not. DEFAULT is FALSE.
#
runTests <- function(projectPath, print=FALSE) {
  tmcrTestRunnerProjectPath <- getwd()

  #runs test for project, returns testthatOuput with added points.
  test_results <- .run_tests_project(projectPath)

  jsonResults <- .CreateJsonResults(test_results)
  .WriteJson(jsonResults)

  if (print) {
    .PrintResultsFromJson(jsonResults)
  }

  setwd(tmcrTestRunnerProjectPath)
}

.run_tests_project <- function(projectPath) {
  setwd(projectPath)

  test_results <- list()

  #Lists all the files in the path beginning with "test" and ending in ".R"
  testFiles <- list.files(path="tests/testthat", pattern = "test.*\\.R", full.names = T, recursive = FALSE)

  for (testFile in testFiles) {
    file_results <- .run_tests_file(testFile)
    test_results <- c(test_results, file_results)
  }
  return(test_results)
}

.run_tests_file <- function(filePath) {
  .GlobalEnv$points <- list()
  .GlobalEnv$points_for_all_tests <- list()

  testFileOutput <- test_file(filePath, reporter = "silent")

  test_file_results <- .create_file_results(testFileOutput, points, .GlobalEnv$points_for_all_tests)

  return(test_file_results)
}

runTestsWithDefault <- function(bol) {
  runTests(getwd(), bol)
}
