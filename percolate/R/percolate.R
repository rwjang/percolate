percolate <- function(x) {
  UseMethod("percolate")
}

#' Percolates the board
#'
#' @param start_board, starting board
#'
#' @return result_board, which is final board after percolation, and result if percolation occured or not
#' @export
#'
#' @examples
percolate.board <- function(start_board) {
  assert_that(is_valid(start_board), msg = "board is valid")
  assert_that(length(which((start_board != 1) & (start_board != 0))) == 0, msg = "board is not 0s and 1s")
  start_board[1, (which(start_board[1, ] == 1))] <- 2
  n <- attr(start_board, "n")
  result <- FALSE
  result_board <- start_board
  count0 <- TRUE
  count1 <- 0
  while(count0 & (count1 < (n^2))) {
    count0 <- FALSE
    for (i in 1:n) {
      for(j in 1:n) {
        if(start_board[i, j] == 2) {
          if((i < n) && (start_board[i + 1, j] == 1)) {
            start_board[i + 1, j] <- 2
            count0 <- TRUE
          }
          if((j > 1) && (start_board[i, (j - 1)] == 1)) {
            start_board[i, (j - 1)] <- 2
            count0 <- TRUE
          }
          if((j < n) && (start_board[i, (j + 1)] == 1)) {
            start_board[i, (j + 1)] <- 2
            count0 <- TRUE
          }
          if((i > 1) && (start_board[i - 1, j] == 1)) {
            start_board[(i - 1), j] <- 2
            count0 <- TRUE
          }
        }
      }
    }
    result_board <- start_board
    count1 <- count1 + 1
  }
  if((length(which(start_board[n, ] == 2))) > 0) {
    result <- TRUE
  }
  return(list("result_board" = result_board, "result" = result))
}
