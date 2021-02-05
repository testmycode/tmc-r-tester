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

.map_to_desc_add <- function(val) {
  .map_to_desc_set(c(.map_to_desc_ref(), val))
}
.map_to_desc_reset <- function() {
  .map_to_desc_set(list())
}
.map_to_desc_set <- function(val) {
  .GlobalEnv$carry$map_to_desc <- val
}
.map_to_desc_ref <- function() {
  .GlobalEnv$carry$map_to_desc
}
.file_points_ref <- function() {
  .GlobalEnv$carry$file_points
}
.file_points_reset <- function() {
  .file_points_set(list())
}
.file_points_set <- function(val) {
  .GlobalEnv$carry$file_points <- val
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
  .dcat("A", project_path)
  .init_global_vars()
  all_available_points <- list()
  .dcat("B", project_path)
  test_files <- list.files(path = paste0(project_path, "/tests/testthat"),
			   pattern = "test.*\\.R",
			   full.names = TRUE, recursive = FALSE)
  .dcat("C", test_files)
  for (test_file in test_files) {
    .dcat("test_file", test_file)
    .map_to_desc_reset()
    .file_points_reset()
    env <- .create_counter_env(project_path)
    testthat::test_file(test_file, reporter = "silent", env = env)
    for (desc in .map_to_desc_ref()) {
      all_available_points[[desc]] <- c(.file_points_ref(),
					.test_available_points_ref(desc))
    }
    .counter_inc()
  }
  return (all_available_points)
}

.init_global_vars <- function() {
  .counter_init()
  .test_available_points_init()
  .file_points_reset()
  .map_to_desc_reset()
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
    .test_available_points_set(desc, point)
    .map_to_desc_add(desc)
  }
  test_env$points_for_all_tests <- function(points){
    .file_points_set(points)
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
