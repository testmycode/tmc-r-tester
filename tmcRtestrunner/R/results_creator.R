.create_file_results <- function(testthat_file_output, tests_points, file_points) {
  results <- list()
  for(test in testthat_file_output) {
    name <- test$test
    message <- ""
    if (.CheckIfTestPassed(test)) {
      status <- "pass"
    } else {
      status <- "fail"
      message <- .CreateMessageForTestWithFailures(test)
    }
    points <- .GetPointsForTest(name, tests_points, file_points)
    test_result <- list("name" = name, "status" = status, "points" = points, "message" = message, backtrace = list())
    results[[length(results) + 1 ]] <- test_result
  }
  return(results)
}

.GetPointsForTest <- function(test_name, tests_points, file_points) {
  if (is.null(tests_points[[test_name]])) {
    test_points <- vector()
  } else {
    test_points <- tests_points[[test_name]]
  }
  test_points <- c(file_points, test_points)
  return(test_points)
}

#Checks if all tests pass in testOutput
.CheckAllTestPassed <- function(testOutput) {
  ret <- TRUE
  for (test in testOutput) {
    if (!.CheckIfTestPassed(test)) {
      ret <- FALSE
      break
    }
  }
  return (ret)
}

#Checks if a single test passed
.CheckIfTestPassed <- function(test) {
  ret <- TRUE
  for (result in test$results) {
    if (!.CheckIfResultPassed(result)) {
      ret <- FALSE
      break
    }
  }
  return (ret)
}

#Check if a single result passed
.CheckIfResultPassed <- function(result) {
  return(format(result) == "As expected")
}

#Returns message from failed results
#Currently supports only results that used calls
.MessageFromFailedResult <- function(result) {
  if (is.null(result$call)) {
    return("")
  }
  #language that failed the test. for example call expect_equal(1,2)
  language <- toString(result$call[[1]])
  return (paste(sep="", "Failed with call: ", language,"\n", result$message))
}

.CreateMessageForTestWithFailures <- function(test) {
  testMessage <- ""
  for (result in test$results) {
    if (format(result) != "As expected") {
      testMessage <- paste(sep = "", testMessage, .MessageFromFailedResult(result))
    }
  }
  return(testMessage)
}
