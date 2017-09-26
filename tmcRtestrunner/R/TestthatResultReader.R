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
