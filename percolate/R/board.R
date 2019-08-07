#' Makes an object board b based on parameters
#'
#' @param mat, the matrix if povided
#' @param n, the dimension of the matrix (row/col)
#' @param p, fraction of squares to be shaded
#'
#' @return An object of class board with subclass matrix and attributes n and p
#' @export
#'
#' @examples
#' board(n = 8, p = 0.75)
#' board(n = 10, p = 0.8)
board <- function(mat = NULL, n = 5, p = 0.25) {
  if(!is.null(mat)) {
    assert_that(is_valid(mat), msg = "mat is not a matrix")
    board_res <- mat
    class(board_res) <- "board"
    n_board <- dim(mat)[1]
    p_board <- length(which(mat == 0))/length(mat)
    attr(board_res, "n") <- n_board
    attr(board_res, "p") <- p_board
  }
  else {
    board_res <- structure(generate_board_mat(n, p), class = "board")
    attr(board_res, "n") <- n
    attr(board_res, "p") <- p
  }
  return(board_res)
}

