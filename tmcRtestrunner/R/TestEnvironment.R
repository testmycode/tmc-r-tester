#' @importFrom R.utils withTimeout
#' @importFrom testthat test_that
#' @importFrom utils str
.define_tester_functions <- function(test_env) {
  points_for_all_tests  <- function(points) {
    .GlobalEnv$points_for_all_tests <- points
  }
  setup_testthat_env <- function(call_env) {
    if (!("expect_equal" %in% ls(call_env))) {
      testthat_env <- as.environment(which("package:testthat" == search()))
      for (name in ls(testthat_env)) {
        val <- get(name, envir = testthat_env)
	assign(name, value = val, envir = call_env)
      }
    }
  }
  test <- function(desc, points, code, timeout = 30) {
    call_env <- as.environment(-1L)
    setup_testthat_env(call_env)
    .GlobalEnv$points[[desc]] <- points
    value <- withTimeout(timeout = timeout,
                         expr    = testthat::test_that(desc, code))
    value
  }
  # The test that wraps around test_that()-method and stores the points
  # to global environment.
  assign("points_for_all_tests", points_for_all_tests, envir = test_env)
  assign("test", test, envir = test_env)
}

.path_to_excersise_files <- function(project_path) {
  exercise_files <- list.files(pattern    = "[.]R$",
                               path       = paste0(project_path, "/", "R"),
                               full.names = TRUE)
  exercise_files
}

.create_test_env <- function(project_path, addin_data) {
  disable_interactive_on_server <- function(test_env) {
    if (!is.null(addin_data$server_mode)) {
      skip_view_fn <- function(...) {
        cat("No data viewer.\nSkipping.\n")
      }
      assign("View", skip_view_fn, envir = test_env)
    }
  }
  override_functions <- function(test_env, project_path) {
    mock_path <- paste(sep = .Platform$file.sep,
                       project_path,
                       "tests", "testthat", "mock.R")
    if (file.exists(mock_path)) {
        sys.source(mock_path, test_env)
    }
  }
  .include_libraries <- function(exercise_files) {
    library_expr <- function(expr) {
      deparse(expr[[1]]) == "library"
    }
    for (ex in exercise_files) {
      code_exprs <- parse(file = ex)
      for (expr in code_exprs) {
        if (library_expr(expr)) {
          eval(expr)
        }
      }
    }
  }
  exercise_files <- .path_to_excersise_files(project_path)
  tryCatch(expr  = .include_libraries(exercise_files),
           error = function(err) {
             # silently skipping error, so that it will be signalled properly
             42
           })


  test_env <- new.env(parent = parent.env(.GlobalEnv))
  tryCatch(expr  = override_functions(test_env, project_path),
           error = .signal_sourcing_error)
  disable_interactive_on_server(test_env)
  tryCatch(expr = {
             test_env_list <- .source_files(test_env, project_path, addin_data)
           },
           error     = .signal_sourcing_error,
           interrupt = .signal_sourcing_interrupt)
  return(test_env_list)
}

.short_name <- function(filename) {
  xx <- filename
  ll <- unlist(gregexpr("/", xx))
  substr(xx, ll[length(ll)] + 1, nchar(xx))
}

.test_name_match <- function(test_filename) {
  xx <- test_filename
  xx <- sub(pattern = "^test",     replacement = "",  xx)
  xx <- sub(pattern = "^(.[a-z])", replacement = "\\L\\1", xx, perl = TRUE)
  xx <- sub(pattern = "^[A-Z]",    replacement = "",  xx)
  xx <- sub(pattern = "Helper.R$", replacement = ".R", xx)
  xx <- sub(pattern = "Hidden.R$", replacement = ".R", xx)
  xx
}

.source_safely2 <- function(file, test_env) {
  source_safely <- function(file, test_env) {
    safe_fn <- function() {
      if (!is.null(.Platform$OS.type) && .Platform$OS.type == "windows" &&
          file_encoding(file) == "UTF-8") {
        source(file, test_env, keep.source = getOption("keep.source"),
               encoding = "UTF-8")
      } else if (!is.null(.Platform$OS.type) &&
                 .Platform$OS.type != "windows" &&
                 file_encoding(file) == "ISO-8859") {
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
  wrapper_fn <- function() {
    return(list(env       = source_safely(file, test_env),
                error_msg = NULL))
  }
  error_handler <- function(err) {
    cat(paste("Sourcing tests failed:", err$message, sep = "\n"))
    return(list(env       = test_env,
                error_msg = err$message))
  }
  interrupt_handler <- function(int) {
    cat(paste("Sourcing interrupted:", "<interrupt:>", sep = "\n"))
    return(list(env       = test_env,
                error_msg = "<User interrupted tests>"))
  }
  test_env <- tryCatch(expr	 = wrapper_fn(),
		       error	 = error_handler,
		       interrupt = interrupt_handler)
  return(test_env)
}

.source_files <- function(test_env, project_path, addin_data = NULL) {
  if (addin_data$only_test_names) {
    # we don't source. This is in the wrong place. This needs to
    # be fixed.
    return(list(env = test_env, error_msg = NULL))
  }

  exercise_files     <- .path_to_excersise_files(project_path)
  test_files         <- addin_data$test_files
  test_files_short   <- sapply(test_files, FUN = .short_name)
  test_files_matches <- sapply(test_files_short, FUN = .test_name_match)
  test_env_list      <- vector("list", length(test_files) + 1)
  # initialising the list, so that improper files
  # will not cause too much damage
  errmsg <- paste("Project folder broken.",
                  "You need to redownload this exercise set",
                  "from the server. NOTE: copy your code for safety",
                  "since this will wipe out the template files for",
                  "this exercise set")


  for (ind in seq_along(test_files_matches)) {
    test_env_list[[ind]] <- list(env = test_env, error_msg = errmsg)
  }

  for (file in exercise_files) {
    matching_files_inds <- which(test_files_matches == .short_name(file))
    if (!is.null(addin_data$print) && addin_data$print) {
      if (!length(matching_files_inds)) {
        cat("Improper exercise file:\t",
             paste0("...",
                    sub(pattern = dirname(dirname(dirname(dirname(file)))),
                        replacement = "",
                        file,
                        fixed = TRUE)),
             "\n")
      } else {
       cat("Testing file:\t",
           paste0("...",
                  sub(pattern = dirname(dirname(dirname(dirname(file)))),
                      replacement = "",
                      file,
                      fixed = TRUE)),
           "\n")
      }
    }
    test_env <- .source_safely2(file, test_env)

    for (ind in matching_files_inds) {
      test_env_list[[ind]] <- test_env
    }
    test_env <- new.env(parent = test_env$env)
  }
  test_env <- test_env_list
  return(test_env)
}

#' @title File encoding of exercise file
#'
#' @description This function tries to determine the file encoding of
#' the exercise file. It is a wrapper around 'file' executable and if
#' that is missing, it will return unrecognized value.
#'
#' @usage file_encoding(filename)
#'
#' @param filename a string which is the name of the file tested for
#' encoding.
#'
#' @return a string which is either "ISO-8859", "ASCII", "UTF-8" or ""
#' depending on the encoding of the filename. The empty string means
#' either other file encoding or it can mean that the 'file' executable
#' was not found from the operating system PATH of executable files.
#'

#' @export
file_encoding <- function(filename) {
  pre_file_type <- tryCatch(system2("file", filename,
                                    stdout = TRUE,
                                    stderr = FALSE),
                            error   = function(e) "",
                            warning = function(e) "")
  pre_file_type2 <- strsplit(pre_file_type, split = ":")[[1]]
  if (length(pre_file_type2) == 0) return("")
  recognizers <- c("ISO-8859", "ASCII", "UTF-8")
  matches <- recognizers[sapply(recognizers,
                                function(pattern) {
                                  grepl(pattern,
                                        pre_file_type2[length(pre_file_type2)])
                                })]
  ifelse(length(matches), matches, "")
}
