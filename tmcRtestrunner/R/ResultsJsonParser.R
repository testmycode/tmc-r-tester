#Creates JSON containing test names and points availble from them, based on the test file.
.create_available_points_json_results <- function(testthatOutput) {
  results = list()
  names <- names(testthatOutput)
  points_for_all <- testthatOutput$"all"
  for (name in names) {
    if (name != "all") {
      points <- c(testthatOutput[[name]], points_for_all)
      results[[length(results)+1]] <- list(name=unbox(name), points=points)
    }
  }
  # for (test_result in testthatOutput) {
  #   print(test_result)
    # testName <- test_result$test
    # testPoints <- test_result$points

    # results[[length(results)+1]] <- list(name=unbox(testName), points=unbox(testPoints))
  # }
  return (results)
}

#Creates JSON based on the test file.
.create_json_results <- function(test_results) {
  json_results <- list()
  for (test_result in test_results) {
    test_json_result <- .create_json_test_result(test_result)
    json_results[[length(json_results) + 1]] <- test_json_result
  }
  return(json_results)
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
  for (test in json_result) {
    cat(sep = "", test$name, ": ", test$status, "\n")
    if (test$message != "") {
      cat(sep = "", "\n", test$message, "\n")
    }
  }
}
