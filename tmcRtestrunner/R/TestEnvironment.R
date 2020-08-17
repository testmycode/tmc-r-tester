.create_test_env <- function(project_path, addin_data) {
  override_functions <- function(test_env, project_path) {
    mock_path <- paste(sep = .Platform$file.sep, project_path, "tests",
                        "testthat", "mock.R")
    if (file.exists(mock_path)) {
        sys.source(mock_path, test_env)
    }
  }
  define_tester_functions <- function(test_env) {
    points_for_all_tests  <- function(points) {
      .GlobalEnv$points_for_all_tests <- points
    }
    test <- function(desc, points, code, timeout = 30) {
      .GlobalEnv$points[[desc]] <- points
      value <- withTimeout(timeout = timeout,
                           { test_that(desc, code) })
      value
    }
    #The test that wraps around test_that()-method and stores the points
    #to global environment.
    assign("points_for_all_tests", points_for_all_tests, envir = test_env)
    lockBinding("points_for_all_tests", test_env)
    assign("test", test, envir = test_env)
    lockBinding("test",test_env)
  }
  test_env <- new.env()
  define_tester_functions(test_env)
  tryCatch({ override_functions(test_env, project_path) },
           error = .signal_sourcing_error)
  tryCatch({ .source_files(test_env, project_path, addin_data) },
           error = .signal_sourcing_error)
  return (test_env)
}

.source_files <- function(test_env, project_path, addin_data = NULL) {
  if (!is.null(addin_data) && (addin_data$only_test_names)) {
    # we don't source. This is in the wrong place. This needs to
    # be fixed.
    return()
  }
  for (file in list.files(pattern    = "[.]R$",
                          path       = paste0(project_path, "/R/"),
                          full.names = TRUE)) {
    source(file, test_env, keep.source = getOption("keep.source"))
  }
}




# .source_from_test_file <- function(test_location, test_env) {
#   script_name <- basename(test_location)
#   script_name <- substr(script_name, 5, nchar(script_name))
#   source_folder <- "R/"
#   # Checks whether list is empty and if it is, modifies the first letter of the script to lower case.
#   if (length(list.files(path = source_folder, pattern = script_name, full.names = T, recursive = FALSE)) == 0) {
#     substr(script_name, 1, 1) <- tolower(substr(script_name, 1, 1))
#   }
#   sys.source(paste0(source_folder, script_name), test_env)
# }
#
