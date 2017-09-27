#Creates JSON based on the test file.
.CreateJsonResults <- function(test_results) {
  json_results <- list()
  for (test_result in test_results) {
    test_json_result <- .CreateJsonTestResult(test_result)
    json_results[[length(json_results) + 1]] <- test_json_result
  }
  return (json_results)
}

#Creates JSON for each different test case.
.CreateJsonTestResult <- function(test_result) {
  testResult <- list(status=unbox(test_result$status),
                     name=unbox(format(test_result$name)),
                     message=unbox(test_result$message),
                     backtrace=test_result$backtrace,
                     points=test_result$points)
  return(testResult)
}

#Writes JSON based on the whole test result.
.WriteJson <- function(results) {
  #json utf-8 coded:
  json <- enc2utf8(toJSON(results, pretty = FALSE))
  json <- prettify(json)
  #encode json to utf-8 and write file
  write(json, ".results.json")
}

#Prints results.
.PrintResultsFromJson <- function(jsonResult) {
  for (test in jsonResult) {
    cat(sep = "", test$name, ": ", test$status, "\n")
    if (test$message != "") {
      cat(sep = "", "\n", test$message, "\n")
    }
  }
}
