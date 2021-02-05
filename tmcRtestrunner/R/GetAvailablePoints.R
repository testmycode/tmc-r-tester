globalVariables(c("points"))

.counter_ref <- function() {
  .GlobalEnv$carry$counter
}
.counter_init <- function() {
  carry <- new.env(parent = emptyenv())
  carry$counter <- 1
  .GlobalEnv$carry <- carry
}
.counter_inc <- function() {
  .counter_set(.counter_ref() + 1)
}
.counter_set <- function(val) {
  .GlobalEnv$carry$counter <- val
}

.map_to_desc_add <- function(idx, val) {
  .map_to_desc_set(idx, c(.map_to_desc_ref(idx), val))
}
.map_to_desc_set <- function(idx, val) {
  .GlobalEnv$carry$map_to_desc[[idx]] <- val
}
.map_to_desc_ref <- function(idx) {
  .GlobalEnv$carry$map_to_desc[[idx]]
}
.map_to_desc_init <- function() {
  .GlobalEnv$carry$map_to_desc <- list()
}
.file_points_init <- function() {
  .GlobalEnv$carry$file_points <- list()
}
.file_points_ref <- function(idx) {
  .GlobalEnv$carry$file_points[[idx]]
}
.file_points_set <- function(idx, val) {
  .GlobalEnv$carry$file_points[[idx]] <- val
}
.test_available_points_init <- function() {
  .GlobalEnv$test_available_points <- list()
  .GlobalEnv$carry$test_available_points <- list()
}
.test_available_points_ref <- function(idx) {
  .GlobalEnv$carry$test_available_points[[idx]]
}
.test_available_points_set <- function(idx, val) {
  .GlobalEnv$carry$test_available_points[[idx]] <- val
}

.get_available_points <- function(project_path) {
  .add_points <- function() {
    all_available_points <- list()
    for (i in (1:unlist(.counter_ref() - 1))) {
      for (desc in .map_to_desc_ref(i)) {
	all_available_points[[desc]] <- c(.file_points_ref(i),
					  .test_available_points_ref(desc))
      }
    }
    return (all_available_points)
  }
  .dcat("A", project_path)
  .init_global_vars()
  .dcat("B", project_path)
  test_files <- list.files(path = paste0(project_path, "/tests/testthat"),
			   pattern = "test.*\\.R",
			   full.names = TRUE, recursive = FALSE)
  .dcat("C", test_files)
  for (test_file in test_files) {
    .dcat("D", test_file)
    .map_to_desc_set(.counter_ref(), list())
    .file_points_set(.counter_ref(), list())
    env <- .create_counter_env(project_path)
    testthat::test_file(test_file, reporter = "silent", env = env)
    .counter_inc()
  }
  return (.add_points())
}

.init_global_vars <- function() {
  .counter_init()
  .test_available_points_init()
  .file_points_init()
  .map_to_desc_init()
}

.create_counter_env <- function(project_path) {
  .dcat("E1", project_path)
  test_env <- new.env()
  .define_counter_functions(test_env, project_path)
  .dcat("E4", ls(test_env))
  return (test_env)
}

.define_counter_functions <- function(test_env, project_path) {
  #test_env <- .source_files(test_env, project_path)
  test_env$test <- function(desc, point, code){
    .dcat("TTTT1", desc)
    .dcat("point", point)
    .test_available_points_set(desc, point)
    .map_to_desc_add(.counter_ref(), desc)
  }
  test_env$points_for_all_tests <- function(points){
    .dcat("SSSS2", points)
    .file_points_set(.counter_ref(), points)
  }
}

#' @title Checks the available point for all tests
#'
#' @description Checks the available points for all test in the project
#' without running test. Creates file .available_points.json in the
#' project root.
#'
#' @usage run_available_points(project_path = getwd())
#'
#' @param project_path The absolute path to the root of the project being tested.
#' Default value is current work directory
#'
#' @return The function does not return values
#'


# Checks the available points for all test in the project without running test. Creates
# file .available_points.json in the project root.
run_available_points <- function(project_path = getwd()) {
  available_points <- .get_available_points(project_path)
  .dcat("HHHHHH", available_points)

  json_results <- .create_available_points_json_results(available_points)
  .write_json(json_results, paste0(project_path, "/.available_points.json"))
}
