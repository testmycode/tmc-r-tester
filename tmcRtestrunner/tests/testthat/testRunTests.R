test_resources_dir <- paste(sep = "", getwd(), "/resources")

#projects for testing:
simple_all_tests_pass_project_path <- paste(sep = "", test_resources_dir, "/simple_all_tests_pass")
simple_some_tests_fail_project_path <- paste(sep = "", test_resources_dir, "/simple_some_tests_fail")

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

test_that("RunTests works even when some of the tests are failing", {
  run_tests(simple_some_tests_fail_project_path)
  expect_true(file.exists(paste(sep = "", simple_some_tests_fail_project_path, "/.results.json")))
})

test_that("RunTests does print on print = TRUE", {
  #simple_all_tests_pass prints as expected
  expect_output(run_tests(simple_all_tests_pass_project_path, print = TRUE), "ret_true works.: pass\nret_one works.: pass\nadd works.: pass")
})

test_that("RunTests doesn't print on print = FALSE", {
  expect_silent(run_tests(simple_all_tests_pass_project_path, print = FALSE))
})
