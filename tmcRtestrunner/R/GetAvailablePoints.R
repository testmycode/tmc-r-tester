library("testthat")

.get_available_points <- function(project_path){
  original_path <- getwd()
  setwd(project_path)

  .GlobalEnv$all_available_points <- list()

  test_files <- list.files(path = "tests/testthat", pattern = "test.*\\.R", full.names = T, recursive = FALSE)
  for (test_file in test_files) {
    test_file(test_file, reporter = "silent", env = .create_counter_env())
  }

  return(.GlobalEnv$all_available_points)
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
      .GlobalEnv$all_available_points[[desc]] <- c(point)
    }
  }
  ###tempOverride
  test_env$points_for_all_tests <- function(points){
    .GlobalEnv$all_available_points[["all"]] <- c(points)
  }
}
