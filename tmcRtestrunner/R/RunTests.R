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
  test_results <- .RunTestsProject(projectPath)

  jsonResults <- .CreateJsonResults(test_results)
  .WriteJson(jsonResults)

  if (print) {
    .PrintResultsFromJson(jsonResults)
  }

  setwd(tmcrTestRunnerProjectPath)
}

.RunTestsProject <- function(projectPath) {
  setwd(projectPath)

  test_results <- list()

  #Lists all the files in the path beginning with "test" and ending in ".R"
  testFiles <- list.files(path="tests/testthat", pattern = "test.*\\.R", full.names = T, recursive = FALSE)

  for (testFile in testFiles) {
    file_results <- .RunTestsFile(testFile)
    test_results <- c(test_results, file_results)
  }
  return(test_results)
}

.RunTestsFile <- function(filePath) {
  .GlobalEnv$points <- list()
  .GlobalEnv$points_for_all_tests <- list()

  testFileOutput <- test_file(filePath, reporter = "silent")

  test_file_results <- .create_file_results(testFileOutput, points, .GlobalEnv$points_for_all_tests)

  return(test_file_results)
}

.AddPointsToTestOutput <- function(testOutput) {
  for (i in 1 : length(testOutput)) {
    testOutput[[i]]$points <- .GetTestPoints(testOutput[[i]]$test)
  }
  return(testOutput)
}

.GetTestPoints <- function(testName) {
  if (is.null(points[[testName]])) {
    testPoints <- vector()
  } else {
    testPoints <- points[[testName]]
  }
  testPoints <- c(.GlobalEnv$points_for_all_tests, testPoints)
  return(testPoints)
}

runTestsWithDefault <- function(bol) {
  runTests(getwd(), bol)
}
