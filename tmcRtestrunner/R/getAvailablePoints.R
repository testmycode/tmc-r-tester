library("testthat")
###tempOverride
test <- function(a, point, c){
  if (!is.null(point)){
    all_available_points<<-c(all_available_points,point)
  }
}
###tempOverride
points_for_all_tests<-function(point){
  all_available_points <<- c(all_available_points, point)
}

get_available_points <-function(project_path){
  setwd(project_path)
  all_available_points <<- c()
  test_files <- list.files(path="tests/testthat", pattern = "test.*\\.R", full.names = T, recursive = FALSE)
  for (test_file in test_files) {
    test_file(test_file, reporter = "silent",load_helpers=FALSE)
  }
  return(all_available_points)
}
