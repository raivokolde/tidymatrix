#' Activate different components of a tidymatrix
#'
#' Switch the active context of a tidymatrix to operate on rows, columns, or
#' the matrix itself. This determines which component will be affected by
#' subsequent dplyr operations.
#'
#' @param .data A tidymatrix object
#' @param what Which component to activate. One of "rows", "columns", or "matrix"
#'
#' @return A tidymatrix object with the specified component activated
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(12), nrow = 4, ncol = 3)
#' row_data <- data.frame(id = 1:4, group = c("A", "A", "B", "B"))
#' col_data <- data.frame(id = 1:3, type = c("x", "y", "z"))
#' tm <- tidymatrix(mat, row_data, col_data)
#'
#' # Activate rows to filter/mutate row metadata
#' tm %>% activate(rows)
#'
#' # Activate columns to work with column metadata
#' tm %>% activate(columns)
#'
#' # Activate matrix to work with the matrix directly
#' tm %>% activate(matrix)
activate <- function(.data, what) {
  if (!is_tidymatrix(.data)) {
    stop("activate() can only be used on tidymatrix objects", call. = FALSE)
  }

  what <- as.character(substitute(what))

  if (!what %in% c("rows", "columns", "matrix")) {
    stop(
      "`what` must be one of 'rows', 'columns', or 'matrix'\n",
      "  You provided: ", what,
      call. = FALSE
    )
  }

  .data$active <- what
  .data
}

#' Get the active component of a tidymatrix
#'
#' @param .data A tidymatrix object
#' @return A character string indicating the active component
#' @export
active <- function(.data) {
  if (!is_tidymatrix(.data)) {
    stop("active() can only be used on tidymatrix objects", call. = FALSE)
  }
  .data$active
}
