library("testthat")

get_available_points <- function(project_path){
  setwd(project_path)
  all_available_points <<- c()
  test_files <- list.files(path = "tests/testthat", pattern = "test.*\\.R", full.names = T, recursive = FALSE)
  for (test_file in test_files) {
    test_file(test_file, reporter = "silent", env = .create_counter_env())
  }
  return(all_available_points)
}

.create_counter_env <- function() {
  test_env <- new.env()
  .define_counter_functions(test_env)
}

.define_counter_functions <- function(test_env) {
  .source_files(test_env)
  ###tempOverride
  test_env$test <- function(a, point, c){
    if (!is.null(point)){
      all_available_points <<- c(all_available_points, point)
    }
  }
  ###tempOverride
  test_env$points_for_all_tests <- function(point){
    all_available_points <<- c(all_available_points, point)
  }
}
