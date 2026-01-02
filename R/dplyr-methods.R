#' @importFrom dplyr filter select mutate arrange
NULL

#' Filter rows or columns of a tidymatrix
#'
#' Filter the active component of a tidymatrix object. When rows are active,
#' filters row_data and the corresponding matrix rows. When columns are active,
#' filters col_data and the corresponding matrix columns. Cannot filter when
#' matrix is active.
#'
#' @param .data A tidymatrix object
#' @param ... Logical predicates for filtering
#' @param .preserve Not used (for compatibility with dplyr)
#'
#' @return A tidymatrix object with filtered data
#' @export
filter.tidymatrix <- function(.data, ..., .preserve = FALSE) {
  if (.data$active == "matrix") {
    stop(
      "Cannot filter when matrix is active.\n",
      "  Use activate(rows) or activate(columns) first.",
      call. = FALSE
    )
  }

  if (.data$active == "rows") {
    # Add temporary index to track original positions
    .data$row_data$.tidymatrix_index <- seq_len(nrow(.data$row_data))
    filtered_rows <- dplyr::filter(.data$row_data, ...)
    row_indices <- filtered_rows$.tidymatrix_index
    filtered_rows$.tidymatrix_index <- NULL

    .data$row_data <- filtered_rows
    .data$matrix <- .data$matrix[row_indices, , drop = FALSE]
  } else if (.data$active == "columns") {
    # Add temporary index to track original positions
    .data$col_data$.tidymatrix_index <- seq_len(nrow(.data$col_data))
    filtered_cols <- dplyr::filter(.data$col_data, ...)
    col_indices <- filtered_cols$.tidymatrix_index
    filtered_cols$.tidymatrix_index <- NULL

    .data$col_data <- filtered_cols
    .data$matrix <- .data$matrix[, col_indices, drop = FALSE]
  }

  .data
}

#' Select columns from row or column metadata
#'
#' Select columns from the active metadata component. When rows are active,
#' selects from row_data. When columns are active, selects from col_data.
#' Cannot select when matrix is active.
#'
#' @param .data A tidymatrix object
#' @param ... Column selection expressions
#'
#' @return A tidymatrix object with selected metadata columns
#' @export
select.tidymatrix <- function(.data, ...) {
  if (.data$active == "matrix") {
    stop(
      "Cannot select when matrix is active.\n",
      "  Use activate(rows) or activate(columns) first.",
      call. = FALSE
    )
  }

  if (.data$active == "rows") {
    .data$row_data <- dplyr::select(.data$row_data, ...)
  } else if (.data$active == "columns") {
    .data$col_data <- dplyr::select(.data$col_data, ...)
  }

  .data
}

#' Mutate the active component of a tidymatrix
#'
#' Add or modify columns in the active metadata component. When rows are active,
#' mutates row_data. When columns are active, mutates col_data. Cannot mutate
#' when matrix is active.
#'
#' @param .data A tidymatrix object
#' @param ... Name-value pairs for new or modified columns
#'
#' @return A tidymatrix object with mutated metadata
#' @export
mutate.tidymatrix <- function(.data, ...) {
  if (.data$active == "matrix") {
    stop(
      "Cannot mutate when matrix is active.\n",
      "  Use activate(rows) or activate(columns) first.",
      call. = FALSE
    )
  }

  if (.data$active == "rows") {
    .data$row_data <- dplyr::mutate(.data$row_data, ...)
  } else if (.data$active == "columns") {
    .data$col_data <- dplyr::mutate(.data$col_data, ...)
  }

  .data
}

#' Arrange the active component of a tidymatrix
#'
#' Reorder rows based on the active metadata component. When rows are active,
#' arranges by row_data and reorders matrix rows accordingly. When columns are
#' active, arranges by col_data and reorders matrix columns accordingly.
#' Cannot arrange when matrix is active.
#'
#' @param .data A tidymatrix object
#' @param ... Variables to arrange by
#'
#' @return A tidymatrix object with reordered data
#' @export
arrange.tidymatrix <- function(.data, ...) {
  if (.data$active == "matrix") {
    stop(
      "Cannot arrange when matrix is active.\n",
      "  Use activate(rows) or activate(columns) first.",
      call. = FALSE
    )
  }

  if (.data$active == "rows") {
    # Create a temporary column to track original order
    .data$row_data$.orig_order <- seq_len(nrow(.data$row_data))
    arranged <- dplyr::arrange(.data$row_data, ...)
    new_order <- arranged$.orig_order
    arranged$.orig_order <- NULL

    .data$row_data <- arranged
    .data$matrix <- .data$matrix[new_order, , drop = FALSE]
  } else if (.data$active == "columns") {
    # Create a temporary column to track original order
    .data$col_data$.orig_order <- seq_len(nrow(.data$col_data))
    arranged <- dplyr::arrange(.data$col_data, ...)
    new_order <- arranged$.orig_order
    arranged$.orig_order <- NULL

    .data$col_data <- arranged
    .data$matrix <- .data$matrix[, new_order, drop = FALSE]
  }

  .data
}
