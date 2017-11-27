test_resources_dir <- paste(sep = "", getwd(), "/resources")

#projects for testing:
simple_all_tests_pass_project_path <- paste(sep = "", test_resources_dir, "/simple_all_tests_pass")

test_that("test_env is created correctly for simple_all_tests_pass", {
  test_env <- .create_test_env(simple_all_tests_pass_project_path)

  #Test functions should exist:
  expect_true(exists("test", where = test_env, mode = "function"))
  expect_true(exists("points_for_all_tests", where = test_env, mode = "function"))

  #Functions from main.R and second.R should exist:
  expect_true(exists("ret_true", where = test_env, mode = "function"))
  expect_true(exists("ret_one", where = test_env, mode = "function"))
  expect_true(exists("add", where = test_env, mode = "function"))
  expect_true(exists("minus", where = test_env, mode = "function"))
})
