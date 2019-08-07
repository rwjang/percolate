#' Returns a randomly generated matrix for the percolation
#'
#' @param n, Dimension of row/col for matrix
#' @param p, Fraction of squares shaded
#'
#' @return n x n matrix that contains floor(p * n^2) shaded squares
#' @export
#'
#' @examples
#' generate_board_mat(n = 8, p = 0.75)
generate_board_mat <- function(n = 5, p = 0.25) {
  assert_that((length(n) == 1) & (is.numeric(n)) & (n %% 1 == 0) & (n > 0), msg = "n must be a positive integer")
  assert_that((is.numeric(p) & (p >= 0) & (p <= 1)), msg = "p must be a double between 0 and 1")
  black_sq <- sample(1:n^2, floor(p*n^2))
  matrix_vec <- rep(c(1), n*n)
  matrix_vec[black_sq] <- 0
  return(matrix(matrix_vec, n, n))
}

#' Checks if matrix is valid
#'
#' @param mat, a matrix
#'
#' @return Return TRUE if mat is valid, otherwise throw an error
#' @export
#'
#' @examples
#' is_valid(generate_board_mat())
#' is_valid(generate_board_mat(n=1))
is_valid <- function(mat) {
  assert_that(is.matrix(mat), msg = "mat is not a matrix")
  assert_that(nrow(mat) == ncol(mat), msg = "mat must be a square matrix")
  assert_that(length(which((mat != 0) & (mat != 1) & (mat != 2))) == 0, msg = "mat can only contain 0, 1, or 2 as values")
  return(TRUE)
}

