library('testthat')
source('../../R/matrices.R')

pointsForAllTests(c("r2"))

test("Matrix transpose with [[1,2]] works", c("r2.1"), {
  A<- matrix(c(1,2),nrow=1)
  expect_equal(transposeMatrix(A),matrix(c(1,2),nrow=2))
})

test("Matrix transpose with [[1,2],[3,4]] works", c("r2.2"), {
  A<- matrix(c(1,2,3,4),nrow=2)
  expect_equal(transposeMatrix(A),matrix(c(1,3,2,4),nrow=2))
})
