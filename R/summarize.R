#' @importFrom dplyr summarize summarise n_groups
NULL

#' Summarize grouped tidymatrix
#'
#' Aggregate grouped rows or columns, applying summary functions to metadata
#' and aggregating the matrix. For numeric matrices, the default aggregation
#' is \code{mean()}. For non-numeric matrices, you must specify \code{.matrix_fn}.
#'
#' @param .data A grouped_tidymatrix object
#' @param ... Name-value pairs of summary functions for metadata
#' @param .matrix_fn Function to aggregate matrix values within each group.
#'   Default is \code{mean} for numeric matrices. Required for non-numeric matrices.
#' @param .matrix_args List of additional arguments to pass to \code{.matrix_fn}
#'   (e.g., \code{list(na.rm = TRUE)})
#' @param .groups Grouping structure of result (same as dplyr::summarize)
#'
#' @return An ungrouped tidymatrix object with aggregated data
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(20), nrow = 10, ncol = 2)
#' row_data <- data.frame(
#'   id = 1:10,
#'   group = rep(c("A", "B"), each = 5)
#' )
#' tm <- tidymatrix(mat, row_data)
#'
#' # Summarize with default (mean) for numeric matrix
#' tm %>%
#'   activate(rows) %>%
#'   group_by(group) %>%
#'   summarize(n = n(), avg_id = mean(id))
#'
#' # Use different aggregation function
#' tm %>%
#'   activate(rows) %>%
#'   group_by(group) %>%
#'   summarize(n = n(), .matrix_fn = median)
#'
#' # With additional arguments
#' mat_na <- mat
#' mat_na[1, 1] <- NA
#' tm_na <- tidymatrix(mat_na, row_data)
#' tm_na %>%
#'   activate(rows) %>%
#'   group_by(group) %>%
#'   summarize(n = n(), .matrix_fn = mean, .matrix_args = list(na.rm = TRUE))
summarize.grouped_tidymatrix <- function(.data, ..., .matrix_fn = NULL,
                                         .matrix_args = list(), .groups = NULL) {
  if (!is_grouped_tidymatrix(.data)) {
    stop("summarize.grouped_tidymatrix can only be used on grouped tidymatrix objects", call. = FALSE)
  }

  # Determine matrix aggregation function
  if (is.null(.matrix_fn)) {
    # Check if matrix is numeric
    if (is.numeric(.data$matrix)) {
      .matrix_fn <- mean
    } else {
      # Non-numeric matrix requires explicit function
      matrix_type <- typeof(.data$matrix)
      if (is.logical(.data$matrix)) {
        matrix_type <- "logical"
      } else if (is.character(.data$matrix)) {
        matrix_type <- "character"
      } else if (is.factor(.data$matrix)) {
        matrix_type <- "factor"
      }

      suggestions <- get_matrix_fn_suggestions(matrix_type)

      stop(
        "Cannot use default aggregation (mean) on non-numeric matrix.\n",
        "Matrix type: ", matrix_type, "\n",
        "Please specify .matrix_fn explicitly. Suggestions for ", matrix_type, " matrices:\n",
        suggestions,
        call. = FALSE
      )
    }
  }

  # Aggregate based on active context
  if (.data$active == "rows") {
    result <- summarize_rows(.data, ..., .matrix_fn = .matrix_fn,
                            .matrix_args = .matrix_args, .groups = .groups)
  } else if (.data$active == "columns") {
    result <- summarize_columns(.data, ..., .matrix_fn = .matrix_fn,
                               .matrix_args = .matrix_args, .groups = .groups)
  } else {
    stop("Cannot summarize when matrix is active", call. = FALSE)
  }

  # Ungroup the result
  ungroup(result)
}

#' @rdname summarize.grouped_tidymatrix
#' @export
summarise.grouped_tidymatrix <- summarize.grouped_tidymatrix

#' Summarize rows
#' @keywords internal
summarize_rows <- function(.data, ..., .matrix_fn, .matrix_args, .groups) {
  # Add temporary index to track original positions
  .data$row_data$.tidymatrix_index <- seq_len(nrow(.data$row_data))

  # Summarize metadata
  if (is.null(.groups)) {
    summarized_meta <- dplyr::summarize(.data$row_data, ..., .tidymatrix_index = list(.tidymatrix_index))
  } else {
    summarized_meta <- dplyr::summarize(.data$row_data, ..., .tidymatrix_index = list(.tidymatrix_index), .groups = .groups)
  }

  # Extract group indices
  group_indices <- summarized_meta$.tidymatrix_index
  summarized_meta$.tidymatrix_index <- NULL

  # Aggregate matrix rows for each group
  aggregated_matrix <- matrix(NA, nrow = length(group_indices), ncol = ncol(.data$matrix))

  for (i in seq_along(group_indices)) {
    idx <- group_indices[[i]]
    if (length(idx) == 1) {
      # Single row - just take it
      aggregated_matrix[i, ] <- .data$matrix[idx, ]
    } else {
      # Multiple rows - aggregate
      group_matrix <- .data$matrix[idx, , drop = FALSE]
      # Apply function column by column
      for (j in seq_len(ncol(group_matrix))) {
        aggregated_matrix[i, j] <- do.call(.matrix_fn, c(list(group_matrix[, j]), .matrix_args))
      }
    }
  }

  # Create result tidymatrix
  .data$matrix <- aggregated_matrix
  .data$row_data <- as.data.frame(summarized_meta)
  .data
}

#' Summarize columns
#' @keywords internal
summarize_columns <- function(.data, ..., .matrix_fn, .matrix_args, .groups) {
  # Add temporary index to track original positions
  .data$col_data$.tidymatrix_index <- seq_len(nrow(.data$col_data))

  # Summarize metadata
  if (is.null(.groups)) {
    summarized_meta <- dplyr::summarize(.data$col_data, ..., .tidymatrix_index = list(.tidymatrix_index))
  } else {
    summarized_meta <- dplyr::summarize(.data$col_data, ..., .tidymatrix_index = list(.tidymatrix_index), .groups = .groups)
  }

  # Extract group indices
  group_indices <- summarized_meta$.tidymatrix_index
  summarized_meta$.tidymatrix_index <- NULL

  # Aggregate matrix columns for each group
  aggregated_matrix <- matrix(NA, nrow = nrow(.data$matrix), ncol = length(group_indices))

  for (i in seq_along(group_indices)) {
    idx <- group_indices[[i]]
    if (length(idx) == 1) {
      # Single column - just take it
      aggregated_matrix[, i] <- .data$matrix[, idx]
    } else {
      # Multiple columns - aggregate
      group_matrix <- .data$matrix[, idx, drop = FALSE]
      # Apply function row by row
      for (j in seq_len(nrow(group_matrix))) {
        aggregated_matrix[j, i] <- do.call(.matrix_fn, c(list(group_matrix[j, ]), .matrix_args))
      }
    }
  }

  # Create result tidymatrix
  .data$matrix <- aggregated_matrix
  .data$col_data <- as.data.frame(summarized_meta)
  .data
}

#' Get suggestions for matrix aggregation functions
#' @keywords internal
get_matrix_fn_suggestions <- function(type) {
  suggestions <- switch(
    type,
    logical = paste(
      "  - any: TRUE if any value is TRUE\n",
      "  - all: TRUE if all values are TRUE\n",
      "  - mean: proportion of TRUE values\n",
      "  - sum: count of TRUE values\n",
      "  - first: take first value in each group",
      sep = ""
    ),
    character = paste(
      "  - first: take first value in each group\n",
      "  - last: take last value in each group\n",
      "  - A mode function: most common value in each group",
      sep = ""
    ),
    factor = paste(
      "  - first: take first value in each group\n",
      "  - last: take last value in each group\n",
      "  - A mode function: most common value in each group",
      sep = ""
    ),
    "  - Specify an appropriate aggregation function"
  )

  suggestions
}
