#' Create a tidymatrix object
#'
#' A tidymatrix combines a matrix with row and column metadata, enabling
#' tidyverse-style manipulation. The object stores three components:
#' the matrix data, row annotations, and column annotations.
#'
#' @param matrix A numeric matrix
#' @param row_data A data.frame with row metadata. Must have same number of rows
#'   as the matrix. If NULL, a data.frame with row indices is created.
#' @param col_data A data.frame with column metadata. Must have same number of
#'   rows as the matrix has columns. If NULL, a data.frame with column indices
#'   is created.
#'
#' @return A tidymatrix object
#' @export
#'
#' @examples
#' # Create a simple tidymatrix
#' mat <- matrix(rnorm(12), nrow = 4, ncol = 3)
#' row_data <- data.frame(
#'   person_id = 1:4,
#'   age = c(25, 30, 35, 40),
#'   gender = c("M", "F", "F", "M")
#' )
#' col_data <- data.frame(
#'   question_id = 1:3,
#'   type = c("numeric", "categorical", "numeric")
#' )
#' tm <- tidymatrix(mat, row_data, col_data)
tidymatrix <- function(matrix, row_data = NULL, col_data = NULL) {
  # Validate matrix
  if (!is.matrix(matrix)) {
    stop("`matrix` must be a matrix object", call. = FALSE)
  }

  # Create default row_data if not provided
  if (is.null(row_data)) {
    row_data <- data.frame(.row_id = seq_len(nrow(matrix)))
  } else {
    row_data <- as.data.frame(row_data)
    if (nrow(row_data) != nrow(matrix)) {
      stop(
        "`row_data` must have the same number of rows as `matrix`\n",
        "  matrix rows: ", nrow(matrix), "\n",
        "  row_data rows: ", nrow(row_data),
        call. = FALSE
      )
    }
  }

  # Create default col_data if not provided
  if (is.null(col_data)) {
    col_data <- data.frame(.col_id = seq_len(ncol(matrix)))
  } else {
    col_data <- as.data.frame(col_data)
    if (nrow(col_data) != ncol(matrix)) {
      stop(
        "`col_data` must have the same number of rows as `matrix` has columns\n",
        "  matrix columns: ", ncol(matrix), "\n",
        "  col_data rows: ", nrow(col_data),
        call. = FALSE
      )
    }
  }

  # Create the tidymatrix object
  structure(
    list(
      matrix = matrix,
      row_data = row_data,
      col_data = col_data,
      active = "matrix"
    ),
    class = "tidymatrix"
  )
}

#' Check if an object is a tidymatrix
#'
#' @param x An object to test
#' @return TRUE if the object is a tidymatrix, FALSE otherwise
#' @export
is_tidymatrix <- function(x) {
  inherits(x, "tidymatrix")
}

#' Validate a tidymatrix object
#'
#' @param tm A tidymatrix object
#' @return The tidymatrix object (invisibly) if valid, otherwise throws an error
#' @keywords internal
validate_tidymatrix <- function(tm) {
  if (!is_tidymatrix(tm)) {
    stop("Object is not a tidymatrix", call. = FALSE)
  }

  if (!is.matrix(tm$matrix)) {
    stop("tidymatrix$matrix must be a matrix", call. = FALSE)
  }

  if (!is.data.frame(tm$row_data)) {
    stop("tidymatrix$row_data must be a data.frame", call. = FALSE)
  }

  if (!is.data.frame(tm$col_data)) {
    stop("tidymatrix$col_data must be a data.frame", call. = FALSE)
  }

  if (nrow(tm$row_data) != nrow(tm$matrix)) {
    stop(
      "Number of rows in row_data does not match matrix rows",
      call. = FALSE
    )
  }

  if (nrow(tm$col_data) != ncol(tm$matrix)) {
    stop(
      "Number of rows in col_data does not match matrix columns",
      call. = FALSE
    )
  }

  if (!tm$active %in% c("matrix", "rows", "columns")) {
    stop(
      "`active` must be one of 'matrix', 'rows', or 'columns'",
      call. = FALSE
    )
  }

  invisible(tm)
}

#' Print a tidymatrix object
#'
#' @param x A tidymatrix object
#' @param ... Additional arguments (currently unused)
#' @export
print.tidymatrix <- function(x, ...) {
  cat("# A tidymatrix: ", nrow(x$matrix), " x ", ncol(x$matrix), " matrix\n", sep = "")
  cat("# Active: ", x$active, "\n", sep = "")
  cat("#\n")
  cat("# Row data: ", nrow(x$row_data), " rows x ", ncol(x$row_data), " columns\n", sep = "")
  cat("# Column data: ", nrow(x$col_data), " rows x ", ncol(x$col_data), " columns\n", sep = "")
  cat("#\n")

  if (x$active == "rows") {
    cat("# Active data (rows):\n")
    print(utils::head(x$row_data))
  } else if (x$active == "columns") {
    cat("# Active data (columns):\n")
    print(utils::head(x$col_data))
  } else {
    cat("# Matrix preview:\n")
    print(utils::head(x$matrix))
  }

  invisible(x)
}
