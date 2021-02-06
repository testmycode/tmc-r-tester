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
  test_env <- new.env(parent = parent.env(.GlobalEnv))
  define_tester_functions(test_env)
  tryCatch({ override_functions(test_env, project_path) },
           error = .signal_sourcing_error)
  tryCatch({ test_env <- .source_files(test_env, project_path, addin_data) },
	   error = .signal_sourcing_error)
  return (test_env)
}

.short_name <- function(filename) {
  xx <- filename
  ll <- unlist(gregexpr("/", xx))
  substr(xx, ll[length(ll)] + 1, nchar(xx))
}

.test_name_match <- function(test_filename) {
  xx <- test_filename
  xx <- sub(pattern = "^testE",    replacement = "e",  xx)
  xx <- sub(pattern = "Helper.R$", replacement = ".R", xx)
  xx <- sub(pattern = "Hidden.R$", replacement = ".R", xx)
  xx
}

.source_files <- function(test_env, project_path, addin_data = NULL) {
  if (addin_data$only_test_names) {
    # we don't source. This is in the wrong place. This needs to
    # be fixed.
    return(test_env)
  }
  source_safely <- function(file, test_env) {
    safe_fn <- function() {
      if (!is.null(.Platform$OS.type) && .Platform$OS.type == "windows" &&
	  .file_encoding(file) == "UTF-8") {
	source(file, test_env, keep.source = getOption("keep.source"),
	       encoding = "UTF-8")
      } else if (!is.null(.Platform$OS.type) && .Platform$OS.type != "windows" &&
	  .file_encoding(file) == "ISO-8859") {
	source(file, test_env, keep.source = getOption("keep.source"),
	       encoding = "latin1")
      } else {
	source(file, test_env, keep.source = getOption("keep.source"))
      }
      return(test_env)
    }
    test_env <- safe_fn()
    return(test_env)
  }

  source_safely2 <- function(file, test_env) {
    wrapper_fn <- function() {
      test_env <- source_safely(file, test_env)
      return(list(env = test_env, error_msg = NULL))
    }
    error_handler <- function(err) {
      return(list(env = test_env, error_msg = err$message))
    }
    test_env <- tryCatch({ wrapper_fn() }, error = error_handler)
    return(test_env)
  }
  test_files         <- addin_data$test_files
  test_files_short   <- sapply(test_files, FUN = .short_name)
  test_files_matches <- sapply(test_files_short, FUN = .test_name_match)
  test_env_list      <- vector("list", length(test_files) + 1)
  for (file in list.files(pattern    = "[.]R$",
                          path       = paste0(project_path, "/R/"),
                          full.names = TRUE)) {
    test_env <- source_safely2(file, test_env)
    for (ind in which(test_files_matches == .short_name(file))) {
      test_env_list[[ind]] <- test_env
    }
    test_env <- new.env(parent = test_env$env)
  }
  test_env <- test_env_list
  return(test_env)
}

.file_encoding <- function(fname) {
  pre_file_type <- tryCatch(system2("file", fname, stdout = TRUE, stderr = FALSE),
                            error   = function(e) "",
                            warning = function(e) "")
  pre_file_type2 <- strsplit(pre_file_type, split = ":")[[1]]
  if (length(pre_file_type2) == 0) return("")
  recognizers <- c("ISO-8859", "ASCII", "UTF-8")
  matches <- recognizers[sapply(recognizers,
                                function(pattern) {
                                  grepl(pattern, pre_file_type2[length(pre_file_type2)])
                                })]
  ifelse(length(matches), matches, "")
}
