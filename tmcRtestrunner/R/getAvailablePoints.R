library("testthat")
###tempOverride
test <- function(a,point,c){
  if(!is.null(point)){
    all_available_points<<-c(all_available_points,point)
  }
}
###tempOverride
pointsForAllTests<-function(point){
  all_available_points<<-c(all_available_points,point)
}

getAvailablePoints <-function(project_path){
  setwd(project_path)
  all_available_points <<- c()
  testFiles <- list.files(path="tests/testthat", pattern = "test.*\\.R", full.names = T, recursive = FALSE)
  for (testFile in testFiles) {
    test_file(testFile, reporter = "silent",load_helpers=FALSE)
  }
  return(all_available_points)
}
