test_resources_dir <- paste(sep = "", getwd(), "/resources")

#projects for testing:
simple_all_tests_fail_project_path <- paste(sep = "", test_resources_dir, "/simple_all_tests_fail")
simple_all_tests_pass_project_path <- paste(sep = "", test_resources_dir, "/simple_all_tests_pass")
simple_some_tests_fail_project_path <- paste(sep = "", test_resources_dir, "/simple_some_tests_fail")

test_that("It is possible to get all available points with simple_all_tests_pass-project", {
  #All tests should return true:
  test_points <- .get_available_points(simple_all_tests_pass_project_path)
  expect_equal(test_points[[1]], "r1")
  expect_equal(test_points[[2]], "r1.1")
  expect_equal(test_points[[3]], "r1.2")
  expect_equal(test_points[[4]], c("r1.3", "r1.4"))
})
