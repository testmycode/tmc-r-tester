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
  testthatOutput <- .RunTestsProject(projectPath)

  jsonResults <- .CreateJsonResults(testthatOutput)
  .WriteJson(jsonResults)

  if (print) {
    .PrintResultsFromJson(jsonResults)
  }

  setwd(tmcrTestRunnerProjectPath)
}

.RunTestsProject <- function(projectPath) {
  setwd(projectPath)

  testthatOutput <- list()

  #Lists all the files in the path beginning with "test" and ending in ".R"
  testFiles <- list.files(path="tests/testthat", pattern = "test.*\\.R", full.names = T, recursive = FALSE)

  for (testFile in testFiles) {
    testFileOutput <- .RunTestsFile(testFile)
    testthatOutput <- c(testthatOutput, testFileOutput)
  }
  return(testthatOutput)
}

.RunTestsFile <- function(filePath) {
  .GlobalEnv$points <- list()
  .GlobalEnv$points_for_all_tests <- list()

  testFileOutput <- test_file(filePath, reporter = "silent")
  testFileOutput <- .AddPointsToTestOutput(testFileOutput)

  return(testFileOutput)
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
