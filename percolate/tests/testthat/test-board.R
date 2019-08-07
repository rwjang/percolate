test_that("board returns a matrix with the same exact matrix when a matrix is provided", {
  expect_equivalent(unclass(board(mat = matrix(c(0,1,1,0), 2, 2))), matrix(c(0,1,1,0), 2, 2))
})

test_that("board returns a matrix with correct attributes when a matrix is provided", {
  res <- board(mat = matrix(c(0, 1, 1, 0), 2, 2))
  expect_true(all((attr(res, "n") == 2) & (attr(res, "p") == 0.5)))
})

test_that("board throws error for mat not being a matrix", {
  expect_error(board(mat = c(1, 2)))
})

test_that("board throws error for mat not being a square matrix", {
  expect_error(board(mat = matrix(c(1, 0, 1, 0, 1, 0), 2, 3)))
})

test_that("board throws error for mat not containing correct values", {
  expect_error(board(mat = matrix(c(1, 0, 2, 3), 2, 2)))
})
