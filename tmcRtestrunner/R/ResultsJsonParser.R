#Creates JSON containing test names and points availble from them, based on the test file.
.create_available_points_json_results <- function(available_points) {
  results <- list()
  for (desc in names(available_points)) {
    results[[desc]] <- available_points[[desc]]
  }
  return (results)
}

.create_json_run_results <- function(run_results) {
  json_test_results <- list()
  for (test_result in run_results$test_results) {
    json_test_results[[length(json_test_results) + 1]] <- .create_json_test_result(test_result)
  }
  json_run_results <- list("runStatus" = unbox(run_results$run_status),
                       "backtrace" = run_results$backtrace, "testResults" = json_test_results)
  return(json_run_results)
}

#Creates JSON for each different test case.
.create_json_test_result <- function(test_result) {
  test_result <- list(status = unbox(test_result$status),
                     name = unbox(format(test_result$name)),
                     message = unbox(test_result$message),
                     backtrace = test_result$backtrace,
                     points = test_result$points)
  return(test_result)
}

#Writes JSON based on the whole test result.
.write_json <- function(results, filename) {
  #json utf-8 coded:
  json <- enc2utf8(toJSON(results, pretty = FALSE))
  json <- prettify(json)
  #encode json to utf-8 and write file
  write(json, filename)
}

#Prints results.
.print_results_from_json <- function(json_result) {
  for (test in json_result$testResults) {
    cat(sep = "", test$name, ": ", test$status, "\n")
    if (test$message != "") {
      cat(sep = "", "\n", test$message, "\n")
    }
  }
}
