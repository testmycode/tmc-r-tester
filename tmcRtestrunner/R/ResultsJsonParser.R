#Creates JSON based on the test file.
.CreateJsonResults <- function(testthatOutput) {
  results = list()
  for (test in testthatOutput) {
    testName <- test$test
    testPoints <- test$points
    testMessage <- ""

    if (.CheckIfTestPassed(test)) {
      testStatus <- "pass"
    } else {
      testStatus <- "fail"
      testMessage <- .CreateMessageForTestWithFailures(test)
    }

    testResult <- .CreateJsonTestResult(testStatus, testName, testMessage,testPoints, "")
    #Add test result to results
    results[[length(results)+1]] <- testResult
  }
  return (results)
}

#Creates JSON for each different test case.
.CreateJsonTestResult <- function(testStatus, testName, testMessage,
                                  testPoints, backtrace) {
  testResult <- list(status=unbox(testStatus),
                     name=unbox(format(testName)),
                     message=unbox(testMessage),
                     backtrace=unbox(backtrace),
                     points=testPoints)
  return(testResult)
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
