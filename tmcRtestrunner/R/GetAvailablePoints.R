library("testthat")
### Override helper function
test <- function(desc, point, code){
  if (!is.null(point)) {
    .GlobalEnv$all_available_points[[desc]] <- c(point)
  }
}

### Override helper function
points_for_all_tests <- function(points) {
  .GlobalEnv$all_available_points[["all"]] <- c(points)
}

.get_available_points <- function(project_path){
  original_path <- getwd()
  setwd(project_path)

  .GlobalEnv$all_available_points <- list()

  test_files <- list.files(path = "tests/testthat", pattern = "test.*\\.R", full.names = T, recursive = FALSE)
  for (test_file in test_files) {
    test_file(test_file, reporter = "silent", load_helpers = FALSE)
  }

  return(.GlobalEnv$all_available_points)
}
