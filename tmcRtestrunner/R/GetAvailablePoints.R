library("testthat")

.get_available_points <- function(project_path){
  original_path <- getwd()
  setwd(project_path)

  .GlobalEnv$test_available_points <- list()
  .GlobalEnv$file_points <- list()
  .GlobalEnv$map_to_desc <- list()
  .GlobalEnv$counter <- 1

  test_files <- list.files(path = "tests/testthat", pattern = "test.*\\.R", full.names = T, recursive = FALSE)
  for (test_file in test_files) {
    .GlobalEnv$map_to_desc[[.GlobalEnv$counter]] = list()
    .GlobalEnv$file_points[[.GlobalEnv$counter]] = list()
    test_file(test_file, reporter = "silent", env = .create_counter_env())
    .GlobalEnv$counter <- .GlobalEnv$counter + 1
  }
  return (.add_points(.GlobalEnv$test_available_points, .GlobalEnv$file_points, .GlobalEnv$map_to_desc))
}

.add_points <- function(test_available_points, file_points, map_to_desc) {
  all_available_points <- list()
  for (i in (1:unlist(.GlobalEnv$counter - 1))) {
    for (desc in map_to_desc[[i]]) {
      all_available_points[[desc]] <- c(test_available_points[[desc]], file_points[[i]])
    }
  }
  return (all_available_points)
}

.create_counter_env <- function() {
  test_env <- new.env()
  .define_counter_functions(test_env)
  return (test_env)
}

.define_counter_functions <- function(test_env) {
  .source_files(test_env)
  ###tempOverride
  test_env$test <- function(desc, point, code){
    .GlobalEnv$test_available_points[[desc]] <- list()
    .GlobalEnv$test_available_points[[desc]] <- c(point)
    .GlobalEnv$map_to_desc[[.GlobalEnv$counter]] <- c(.GlobalEnv$map_to_desc[[counter]], desc)
  }
  ###tempOverride
  test_env$points_for_all_tests <- function(points){
    .GlobalEnv$file_points[[.GlobalEnv$counter]] <- c(points)
  }
}
