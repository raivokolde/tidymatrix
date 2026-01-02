#' @importFrom dplyr slice slice_head slice_tail slice_sample slice_min slice_max rename pull relocate
NULL

#' Slice rows or columns by position
#'
#' Select rows or columns by their integer positions. When rows are active,
#' slices row_data and the corresponding matrix rows. When columns are active,
#' slices col_data and the corresponding matrix columns.
#'
#' @param .data A tidymatrix object
#' @param ... Integer positions or expressions to select
#' @param .preserve Not used (for compatibility with dplyr)
#'
#' @return A tidymatrix object with sliced data
#' @export
slice.tidymatrix <- function(.data, ..., .preserve = FALSE) {
  if (.data$active == "matrix") {
    stop(
      "Cannot slice when matrix is active.\n",
      "  Use activate(rows) or activate(columns) first.",
      call. = FALSE
    )
  }

  if (.data$active == "rows") {
    # Add temporary index to track original positions
    .data$row_data$.tidymatrix_index <- seq_len(nrow(.data$row_data))
    sliced_rows <- dplyr::slice(.data$row_data, ...)
    row_indices <- sliced_rows$.tidymatrix_index
    sliced_rows$.tidymatrix_index <- NULL

    .data$row_data <- sliced_rows
    .data$matrix <- .data$matrix[row_indices, , drop = FALSE]
  } else if (.data$active == "columns") {
    # Add temporary index to track original positions
    .data$col_data$.tidymatrix_index <- seq_len(nrow(.data$col_data))
    sliced_cols <- dplyr::slice(.data$col_data, ...)
    col_indices <- sliced_cols$.tidymatrix_index
    sliced_cols$.tidymatrix_index <- NULL

    .data$col_data <- sliced_cols
    .data$matrix <- .data$matrix[, col_indices, drop = FALSE]
  }

  .data
}

#' Select first or last rows/columns
#'
#' @param .data A tidymatrix object
#' @param n Number of rows/columns to select
#' @param prop Proportion of rows/columns to select
#' @param ... Not used
#'
#' @return A tidymatrix object with selected data
#' @export
slice_head.tidymatrix <- function(.data, n, prop, ...) {
  if (.data$active == "matrix") {
    stop(
      "Cannot slice when matrix is active.\n",
      "  Use activate(rows) or activate(columns) first.",
      call. = FALSE
    )
  }

  if (.data$active == "rows") {
    .data$row_data$.tidymatrix_index <- seq_len(nrow(.data$row_data))
    if (missing(n) && missing(prop)) {
      sliced_rows <- dplyr::slice_head(.data$row_data)
    } else if (!missing(n)) {
      sliced_rows <- dplyr::slice_head(.data$row_data, n = n)
    } else {
      sliced_rows <- dplyr::slice_head(.data$row_data, prop = prop)
    }
    row_indices <- sliced_rows$.tidymatrix_index
    sliced_rows$.tidymatrix_index <- NULL

    .data$row_data <- sliced_rows
    .data$matrix <- .data$matrix[row_indices, , drop = FALSE]
  } else if (.data$active == "columns") {
    .data$col_data$.tidymatrix_index <- seq_len(nrow(.data$col_data))
    if (missing(n) && missing(prop)) {
      sliced_cols <- dplyr::slice_head(.data$col_data)
    } else if (!missing(n)) {
      sliced_cols <- dplyr::slice_head(.data$col_data, n = n)
    } else {
      sliced_cols <- dplyr::slice_head(.data$col_data, prop = prop)
    }
    col_indices <- sliced_cols$.tidymatrix_index
    sliced_cols$.tidymatrix_index <- NULL

    .data$col_data <- sliced_cols
    .data$matrix <- .data$matrix[, col_indices, drop = FALSE]
  }

  .data
}

#' @rdname slice_head.tidymatrix
#' @export
slice_tail.tidymatrix <- function(.data, n, prop, ...) {
  if (.data$active == "matrix") {
    stop(
      "Cannot slice when matrix is active.\n",
      "  Use activate(rows) or activate(columns) first.",
      call. = FALSE
    )
  }

  if (.data$active == "rows") {
    .data$row_data$.tidymatrix_index <- seq_len(nrow(.data$row_data))
    if (missing(n) && missing(prop)) {
      sliced_rows <- dplyr::slice_tail(.data$row_data)
    } else if (!missing(n)) {
      sliced_rows <- dplyr::slice_tail(.data$row_data, n = n)
    } else {
      sliced_rows <- dplyr::slice_tail(.data$row_data, prop = prop)
    }
    row_indices <- sliced_rows$.tidymatrix_index
    sliced_rows$.tidymatrix_index <- NULL

    .data$row_data <- sliced_rows
    .data$matrix <- .data$matrix[row_indices, , drop = FALSE]
  } else if (.data$active == "columns") {
    .data$col_data$.tidymatrix_index <- seq_len(nrow(.data$col_data))
    if (missing(n) && missing(prop)) {
      sliced_cols <- dplyr::slice_tail(.data$col_data)
    } else if (!missing(n)) {
      sliced_cols <- dplyr::slice_tail(.data$col_data, n = n)
    } else {
      sliced_cols <- dplyr::slice_tail(.data$col_data, prop = prop)
    }
    col_indices <- sliced_cols$.tidymatrix_index
    sliced_cols$.tidymatrix_index <- NULL

    .data$col_data <- sliced_cols
    .data$matrix <- .data$matrix[, col_indices, drop = FALSE]
  }

  .data
}

#' Select a random sample of rows/columns
#'
#' @param .data A tidymatrix object
#' @param n Number of rows/columns to select
#' @param prop Proportion of rows/columns to select
#' @param weight_by Sampling weights (not yet implemented)
#' @param replace Sample with replacement
#' @param ... Not used
#'
#' @return A tidymatrix object with sampled data
#' @export
slice_sample.tidymatrix <- function(.data, n, prop, weight_by = NULL, replace = FALSE, ...) {
  if (.data$active == "matrix") {
    stop(
      "Cannot slice when matrix is active.\n",
      "  Use activate(rows) or activate(columns) first.",
      call. = FALSE
    )
  }

  if (.data$active == "rows") {
    .data$row_data$.tidymatrix_index <- seq_len(nrow(.data$row_data))
    if (missing(n) && missing(prop)) {
      sampled_rows <- dplyr::slice_sample(.data$row_data, replace = replace)
    } else if (!missing(n)) {
      sampled_rows <- dplyr::slice_sample(.data$row_data, n = n, replace = replace)
    } else {
      sampled_rows <- dplyr::slice_sample(.data$row_data, prop = prop, replace = replace)
    }
    row_indices <- sampled_rows$.tidymatrix_index
    sampled_rows$.tidymatrix_index <- NULL

    .data$row_data <- sampled_rows
    .data$matrix <- .data$matrix[row_indices, , drop = FALSE]
  } else if (.data$active == "columns") {
    .data$col_data$.tidymatrix_index <- seq_len(nrow(.data$col_data))
    if (missing(n) && missing(prop)) {
      sampled_cols <- dplyr::slice_sample(.data$col_data, replace = replace)
    } else if (!missing(n)) {
      sampled_cols <- dplyr::slice_sample(.data$col_data, n = n, replace = replace)
    } else {
      sampled_cols <- dplyr::slice_sample(.data$col_data, prop = prop, replace = replace)
    }
    col_indices <- sampled_cols$.tidymatrix_index
    sampled_cols$.tidymatrix_index <- NULL

    .data$col_data <- sampled_cols
    .data$matrix <- .data$matrix[, col_indices, drop = FALSE]
  }

  .data
}

#' Rename columns in metadata
#'
#' Rename columns in the active metadata component. When rows are active,
#' renames columns in row_data. When columns are active, renames columns in
#' col_data.
#'
#' @param .data A tidymatrix object
#' @param ... Name-value pairs for renaming (new_name = old_name)
#'
#' @return A tidymatrix object with renamed metadata columns
#' @export
rename.tidymatrix <- function(.data, ...) {
  if (.data$active == "matrix") {
    stop(
      "Cannot rename when matrix is active.\n",
      "  Use activate(rows) or activate(columns) first.",
      call. = FALSE
    )
  }

  if (.data$active == "rows") {
    .data$row_data <- dplyr::rename(.data$row_data, ...)
  } else if (.data$active == "columns") {
    .data$col_data <- dplyr::rename(.data$col_data, ...)
  }

  .data
}

#' Extract a column from metadata as a vector
#'
#' Extract a single column from the active metadata component as a vector.
#'
#' @param .data A tidymatrix object
#' @param var Column name to extract (can be unquoted)
#' @param ... Not used
#'
#' @return A vector containing the column values
#' @export
pull.tidymatrix <- function(.data, var = -1, ...) {
  if (.data$active == "matrix") {
    stop(
      "Cannot pull when matrix is active.\n",
      "  Use activate(rows) or activate(columns) first.",
      call. = FALSE
    )
  }

  if (.data$active == "rows") {
    dplyr::pull(.data$row_data, {{ var }}, ...)
  } else if (.data$active == "columns") {
    dplyr::pull(.data$col_data, {{ var }}, ...)
  }
}

#' Relocate columns in metadata
#'
#' Change the order of columns in the active metadata component.
#'
#' @param .data A tidymatrix object
#' @param ... Columns to move
#' @param .before Column to move before
#' @param .after Column to move after
#'
#' @return A tidymatrix object with reordered metadata columns
#' @export
relocate.tidymatrix <- function(.data, ..., .before = NULL, .after = NULL) {
  if (.data$active == "matrix") {
    stop(
      "Cannot relocate when matrix is active.\n",
      "  Use activate(rows) or activate(columns) first.",
      call. = FALSE
    )
  }

  if (.data$active == "rows") {
    .data$row_data <- dplyr::relocate(.data$row_data, ..., .before = {{ .before }}, .after = {{ .after }})
  } else if (.data$active == "columns") {
    .data$col_data <- dplyr::relocate(.data$col_data, ..., .before = {{ .before }}, .after = {{ .after }})
  }

  .data
}
