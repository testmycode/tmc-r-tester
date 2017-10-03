test_resources_dir <- paste(sep = "", getwd(), "/resources")

#projects for testing:
simple_all_tests_fail_project_path <- paste(sep = "", test_resources_dir, "/simple_all_tests_fail")
simple_all_tests_pass_project_path <- paste(sep = "", test_resources_dir, "/simple_all_tests_pass")
simple_some_tests_fail_project_path <- paste(sep = "", test_resources_dir, "/simple_some_tests_fail")

test_that("First test in all passing testMain returns correct points", {
  #All tests should return true:
  test_points <- .get_available_points(simple_all_tests_pass_project_path)
  points <- list()
  points <- test_points[["ret_true works."]]
  expect_true("r1" %in% points)
  expect_true("r1.1" %in% points)
  expect_true(!("r2" %in% points))
})

test_that("Second test in all passing testMain returns correct points", {
  test_points <- .get_available_points(simple_all_tests_pass_project_path)
  points <- test_points[["ret_one works."]]
  expect_true("r1" %in% points)
  expect_true("r1.2" %in% points)
  expect_true(!("r2" %in% points))
})

test_that("Third test in all passing testMain returns correct points", {
  test_points <- .get_available_points(simple_all_tests_pass_project_path)
  points <- test_points[["add works."]]
  expect_true("r1" %in% points)
  expect_true("r1.3" %in% points)
  expect_true("r1.4" %in% points)
  expect_true(!("r2" %in% points))
})

test_that("First test in all passing testMain returns correct points", {
  test_points <- .get_available_points(simple_all_tests_pass_project_path)
  points <- test_points[["minus works"]]
  expect_true("r2" %in% points)
  expect_true("r2.1" %in% points)
  expect_true(!("r1" %in% points))
})
