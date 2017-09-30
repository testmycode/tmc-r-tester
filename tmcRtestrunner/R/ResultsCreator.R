.create_file_results <- function(testthat_file_output,
                                 tests_points,
                                 file_points) {

  results <- list()
  for (test in testthat_file_output) {
    name <- test$test
    status <- .get_status_for_test(test)
    message <- .create_message_for_test(test)
    backtrace <- list()
    points <- .get_points_for_test(name,
                                   tests_points,
                                   file_points)

    test_result <- list("name" = name,
                        "status" = status,
                        "points" = points,
                        "message" = message,
                        "backtrace" = backtrace)

    results[[length(results) + 1]] <- test_result
  }
  return(results)
}

.get_points_for_test <- function(test_name, tests_points, file_points) {
  if (is.null(tests_points[[test_name]])) {
    test_points <- vector()
  } else {
    test_points <- tests_points[[test_name]]
  }
  test_points <- c(file_points, test_points)
  return(test_points)
}

.get_status_for_test <- function(test) {
  if (.check_if_test_passed(test)) {
    status <- "pass"
  } else {
    status <- "fail"
  }
  return(status)
}

#Checks if a single test passed
.check_if_test_passed <- function(test) {
  ret <- TRUE
  for (result in test$results) {
    if (!.check_if_result_passed(result)) {
      ret <- FALSE
      break
    }
  }
  return(ret)
}

#Check if a single result passed
.check_if_result_passed <- function(result) {
  return(format(result) == "As expected")
}

#Returns message from failed results
#Currently supports only results that used calls
.message_from_failed_result <- function(result) {
  if (is.null(result$call)) {
    return("")
  }
  #language that failed the test. for example call expect_equal(1,2)
  language <- toString(result$call[[1]])
  return(paste(sep = "", "Failed with call: ", language, "\n", result$message))
}

.create_message_for_test <- function(test) {
  test_message <- ""
  for (result in test$results) {
    if (format(result) != "As expected") {
      test_message <- paste(sep = "", test_message,
        .message_from_failed_result(result))
    }
  }
  return(test_message)
}
