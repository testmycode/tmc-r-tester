library('testthat')
source('../../R/strings.R')

pointsForAllTests(c("r3"))

test("Constant string works", c("r3.1"), {
    expect_equal(constant_string(), "jono")
})
