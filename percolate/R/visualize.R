#' Plots the board x
#'
#' @param x, the board
#'
#' @return A plot of the board, with the correct colors and dimensions
#' @export
#'
#' @examples
plot.board <- function(x) {
  assert_that(is_valid(unclass(x)), msg = "x is not a matrix")
  board_plot <- tidyr::gather(data.frame(row = 1:attr(x, "n"), unclass(x)), key = "column", value = "value", -row)
  cols <- c("0" = "black", "1" = "white", "2" = "lightblue3")
  board_plot$column <- as.numeric(substr(board_plot$column, 2, nchar(board_plot$column)))
  ggplot(data = board_plot, aes(x = column, y = max(row) - row, fill = factor(value))) + geom_tile() + labs(title = paste("Size:", attr(x, "n"), sep = " ")) + theme(legend.position = "none") + theme_void() + scale_fill_manual(values = cols, guide = FALSE) + coord_equal() + theme(plot.title = element_text(hjust = 0.5))
}
