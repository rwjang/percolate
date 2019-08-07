

test_that("generate_board_mat has correct dimensions for initial parameters", {
  res <- generate_board_mat()

  expect_true(all(dim(res) == c(5, 5)))
})

test_that("generate_board_mat has correct matrix elements for initial parameters", {
  res <- generate_board_mat()

  expect_true(all(length(which(res != 0 & res != 1)) == 0))
})

test_that("generate_board_mat has correct dimensions for different parameter for n", {
  res <- generate_board_mat(n = 8)

  expect_true(all(dim(res) == c(8, 8)))
})

test_that("generate_board_mat has correct matrix elements for different parameter for n", {
  res <- generate_board_mat(n = 8)

  expect_true(all(length(which(res != 0 & res != 1)) == 0))
})

test_that("generate_board_mat returns all 1's when p = 0", {
  res <- generate_board_mat(p = 0)

  expect_true(all(length(which(res != 1)) == 0))
})

test_that("generate_board_mat returns all 0's when p = 1", {
  res <- generate_board_mat(p = 1)

  expect_true(all(length(which(res != 0)) == 0))
})

test_that("generate_board_mat throws error for vector parameter for n", {
  expect_error(generate_board_mat(n = c(1,2)))
})

test_that("generate_board_mat throws error for vector parameter for n", {
  expect_error(generate_board_mat(n = c(1,2)))
})

test_that("generate_board_mat throws error for string parameter for n", {
  expect_error(generate_board_mat(n = "asdf"))
})

test_that("generate_board_mat throws error for double parameter for n", {
  expect_error(generate_board_mat(n = 5.4))
})

test_that("generate_board_mat throws error for negative parameter for n", {
  expect_error(generate_board_mat(n = -5))
})

test_that("is_valid throws error for mat not being a matrix", {
  expect_error(is_valid(mat = c(1, 2)))
})

test_that("is_valid throws error for mat not being a square matrix", {
  expect_error(is_valid(mat = matrix(c(1, 0, 1, 0, 1, 0), 2, 3)))
})

test_that("is_valid throws error for mat not containing correct values", {
  expect_error(is_valid(mat = matrix(c(1, 0, 2, 3), 2, 2)))
})
