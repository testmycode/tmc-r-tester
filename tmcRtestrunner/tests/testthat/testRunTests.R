test_resources_dir <- paste(sep = "", getwd(), "/resources")

#projects for testing:
simple_all_tests_pass_project_path <- paste(sep = "", test_resources_dir, "/simple_all_tests_pass")
simple_some_tests_fail_project_path <- paste(sep = "", test_resources_dir, "/simple_some_tests_fail")
simple_source_code_error_project_path <- paste(sep = "", test_resources_dir, "/simple_source_code_error")

test_that("Test pass in simple_all_tests_pass", {
  test_results <- .run_tests_project(simple_all_tests_pass_project_path)

  #All tests should pass:
  for (i in length(test_results)) {
    expect_equal(test_results[[i]]$status, "pass")
  }
})

#Tests that all exercise entrys store the point for all tests.
test_that("Tests that pass in simple_all_tests_pass all have the point for all tests", {
  test_results <- .run_tests_project(simple_all_tests_pass_project_path)
  point <- "r1"
  for (i in 1:3) {
    vec1 <- test_results[[i]]$points
    expect_true(point %in% vec1)
  }
})

test_that(".run_tests_project adds points accordingly for simple_all_tests_pass", {
  test_results <- .run_tests_project(simple_all_tests_pass_project_path)
  #"RetTrue works." points
  expect_equal(test_results[[1]]$points, c("r1", "r1.1"))
  #"RetOne works." points
  expect_equal(test_results[[2]]$points, c("r1", "r1.2"))
  #"Add works." points
  expect_equal(test_results[[3]]$points, c("r1", "r1.3", "r1.4"))
})

test_that("RunTests works as intended", {
  run_tests(simple_all_tests_pass_project_path)
  expect_true(file.exists(paste(sep = "", simple_all_tests_pass_project_path, "/.results.json")))
})

test_that("Not all tests pass in simple_some_tests_fail.", {
  test_results <- .run_tests_project(simple_some_tests_fail_project_path)

  #"RetTrue works." should pass
  expect_equal(test_results[[1]]$status, "pass")
  #"RetOne works." should pass
  expect_equal(test_results[[2]]$status, "pass")
  #"Add works." should pass
  expect_equal(test_results[[3]]$status, "pass")
  #"RetFalse returns true" should FAIL
  expect_equal(test_results[[4]]$status, "fail")
  #"RetTrue works but there asre no points." should pass
  expect_equal(test_results[[5]]$status, "pass")
})

test_that("run_results returns and writes.results.json as expected for simple_some_tests_fail", {
  remove_old_results_json(simple_some_tests_fail_project_path)

  test_results <- run_tests(simple_some_tests_fail_project_path)
  results_json <- read_json(paste(sep = "", simple_some_tests_fail_project_path, "/.results.json"))
  test_results_json <- results_json$testResults

  #runStatus should be true and backtrace empty
  expect_equal(results_json$runStatus, "success")
  expect_equal(results_json$backtrace, list())

  #expected results for simple_some_tests_fail
  expected_test_result <- list()
  expected_test_result[[1]] <- list(status = "pass", name = "ret_true works.",
                                    message = "", backtrace = list(), points = list("r1", "r1.1"))
  expected_test_result[[2]] <- list(status = "pass", name = "ret_one works.",
                                    message = "", backtrace = list(), points = list("r1", "r1.2"))
  expected_test_result[[3]] <- list(status = "pass", name = "add works.",
                                    message = "", backtrace = list(), points = list("r1", "r1.3", "r1.4"))
  expected_test_result[[4]] <- list(status = "fail", name = "ret_false returns true",
                                    message = "Failed with call: expect_true, ret_false()\nret_false() isn't true.\n",
                                    backtrace = list(), points = list("r1", "r1.5"))
  expected_test_result[[5]] <- list(status = "pass", name = "ret_true works but there are no points.",
                                    message = "", backtrace = list(), points = list("r1"))

  #.results.json is as expected
  for (i in 1:5) expect_equal(test_results_json[[i]], expected_test_result
                          [[i]])
  #test_results returns as expected
  for (i in 1:5) {
    expect_equal(test_results[[i]]$status, expected_test_result[[i]]$status)
    expect_equal(test_results[[i]]$name, expected_test_result[[i]]$name)
    expect_equal(test_results[[i]]$message, expected_test_result[[i]]$message)
    expect_equal(test_results[[i]]$backtrace, expected_test_result[[i]]$backtrace)
    expect_equal(as.list(test_results[[i]]$points), expected_test_result[[i]]$points)
  }
})

test_that("RunTests does print on print = TRUE", {
  #simple_all_tests_pass prints as expected
  expect_output(run_tests(simple_all_tests_pass_project_path, print = TRUE),
                "ret_true works.: pass\nret_one works.: pass\nadd works.: pass")
})

test_that("RunTests doesn't print on print = FALSE", {
  expect_silent(run_tests(simple_all_tests_pass_project_path, print = FALSE))
})

test_that("run_tests handles simple_source_code_error accordingly.", {
  remove_old_results_json(simple_source_code_error_project_path)

  #expecting an error message when running source code with error:
  expect_error(run_tests(simple_source_code_error_project_path))

  results_json <- read_json(paste(sep = "", simple_source_code_error_project_path, "/.results.json"))

  #runStatus whould be "sourcing_failed", backtrace empty and testResults empty
  expect_equal(results_json$runStatus, "sourcing_failed")
  expect_equal(results_json$backtrace, list())
  expect_equal(results_json$testResults, list())
})

test_that("run_available_points works and runs available_points", {

  ##Call run_available_points
  run_available_points(simple_all_tests_pass_project_path)

  ##Get the path to the supposed file.
  available_points_path <- paste(sep = "", simple_all_tests_pass_project_path, "/.available_points.json")

  #Check that the file exists
  expect_equal(T, file.exists(available_points_path))

  #Delete the file afterwards.
  file.remove(available_points_path)
})

test_that("/.available_points.json has correct values", {

  ##Call run_available_points
  run_available_points(simple_all_tests_pass_project_path)

  ##Get the path to the supposed file.
  available_points_path <- paste(sep="", simple_all_tests_pass_project_path, "/.available_points.json")

  #Create json-object from .available_points.json.
  json <- read_json(available_points_path)

  #Test that json has correct values.
  expect_equal(json[[1]]$name, "ret_true works.")
  expect_true(length(json[[1]]$points) > 0)

  #Delete the file afterwards.
  file.remove(available_points_path)
})
