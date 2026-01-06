# Matrix reconstruction helpers for joins

#' Reconstruct matrix for row joins
#'
#' @param matrix Original matrix
#' @param indices Vector of row indices (may contain NA for new rows)
#' @param ncol Number of columns in result matrix
#' @return Reconstructed matrix with NA-filled rows for new entries
#' @noRd
matrix_reconstruct_rows <- function(matrix, indices, ncol) {
  nrow_new <- length(indices)
  new_matrix <- matrix(NA_real_, nrow = nrow_new, ncol = ncol)

  # Set dimnames if original matrix had them
  if (!is.null(dimnames(matrix))) {
    if (!is.null(colnames(matrix))) {
      colnames(new_matrix) <- colnames(matrix)
    }
  }

  # Fill in values from original matrix
  for (i in seq_len(nrow_new)) {
    if (!is.na(indices[i])) {
      # This row existed in original matrix
      new_matrix[i, ] <- matrix[indices[i], ]
    }
    # else: leave as NA (new row from join)
  }

  return(new_matrix)
}

#' Reconstruct matrix for column joins
#'
#' @param matrix Original matrix
#' @param indices Vector of column indices (may contain NA for new columns)
#' @param nrow Number of rows in result matrix
#' @return Reconstructed matrix with NA-filled columns for new entries
#' @noRd
matrix_reconstruct_cols <- function(matrix, indices, nrow) {
  ncol_new <- length(indices)
  new_matrix <- matrix(NA_real_, nrow = nrow, ncol = ncol_new)

  # Set dimnames if original matrix had them
  if (!is.null(dimnames(matrix))) {
    if (!is.null(rownames(matrix))) {
      rownames(new_matrix) <- rownames(matrix)
    }
  }

  # Fill in values from original matrix
  for (j in seq_len(ncol_new)) {
    if (!is.na(indices[j])) {
      # This column existed in original matrix
      new_matrix[, j] <- matrix[, indices[j]]
    }
    # else: leave as NA (new column from join)
  }

  return(new_matrix)
}

# Core join logic

#' Perform join operation on tidymatrix
#'
#' @param .data A tidymatrix object
#' @param y A data.frame or tibble to join with
#' @param by Join key columns
#' @param join_type Type of join to perform
#' @param suffix Suffix for duplicate column names
#' @param copy If TRUE, copy y before joining
#' @param keep Control which join keys to preserve
#' @param ... Additional arguments passed to dplyr join functions
#' @return Modified tidymatrix with joined metadata and updated matrix
#' @noRd
perform_join <- function(.data, y, by = NULL,
                        join_type = c("left", "right", "inner", "full", "semi", "anti"),
                        suffix = c(".x", ".y"), copy = FALSE, keep = NULL, ...) {

  # Validate inputs
  if (!is_tidymatrix(.data)) {
    stop("`.data` must be a tidymatrix object", call. = FALSE)
  }

  if (.data$active == "matrix") {
    stop("Cannot join when matrix is active. Use activate(rows) or activate(columns) first.",
         call. = FALSE)
  }

  if (!is.data.frame(y)) {
    stop("`y` must be a data.frame or tibble", call. = FALSE)
  }

  join_type <- match.arg(join_type)

  # Determine which dimension to work on
  if (.data$active == "rows") {
    metadata <- .data$row_data
    n_other_dim <- ncol(.data$matrix)
  } else {  # columns
    metadata <- .data$col_data
    n_other_dim <- nrow(.data$matrix)
  }

  # Add tracking indices
  # Index for tidymatrix rows (existing)
  metadata$.tidymatrix_index <- seq_len(nrow(metadata))
  # Index for external data (to identify new rows)
  y$.tidymatrix_new <- seq_len(nrow(y))

  # Perform the appropriate join
  joined_data <- switch(join_type,
    left = dplyr::left_join(metadata, y, by = by, suffix = suffix, copy = copy, keep = keep, ...),
    right = dplyr::right_join(metadata, y, by = by, suffix = suffix, copy = copy, keep = keep, ...),
    inner = dplyr::inner_join(metadata, y, by = by, suffix = suffix, copy = copy, keep = keep, ...),
    full = dplyr::full_join(metadata, y, by = by, suffix = suffix, copy = copy, keep = keep, ...),
    semi = dplyr::semi_join(metadata, y, by = by, copy = copy, ...),
    anti = dplyr::anti_join(metadata, y, by = by, copy = copy, ...)
  )

  # Extract tracking information
  tidymatrix_indices <- joined_data$.tidymatrix_index

  # Clean up tracking columns
  joined_data$.tidymatrix_index <- NULL
  joined_data$.tidymatrix_new <- NULL

  # Reconstruct matrix
  if (.data$active == "rows") {
    new_matrix <- matrix_reconstruct_rows(.data$matrix, tidymatrix_indices, n_other_dim)
    .data$row_data <- joined_data
    .data$matrix <- new_matrix
  } else {  # columns
    new_matrix <- matrix_reconstruct_cols(.data$matrix, tidymatrix_indices, n_other_dim)
    .data$col_data <- joined_data
    .data$matrix <- new_matrix
  }

  # Invalidate analyses
  .data <- remove_all_analyses(.data, sprintf("%s_join", join_type))

  return(.data)
}

# Join methods

#' Join tidymatrix with another data frame
#'
#' @description
#' These functions are tidymatrix methods for dplyr's join functions.
#' They join the row_data or col_data (depending on which is active) with
#' an external data.frame, and appropriately update the matrix dimensions.
#'
#' @details
#' Joins work on the active dimension (rows or columns). Use \code{activate()}
#' to specify which metadata to join.
#'
#' When joins add new rows/columns (e.g., \code{right_join}, \code{full_join}),
#' the matrix is expanded with NA values for the new entries.
#'
#' When joins remove rows/columns (e.g., \code{inner_join}, \code{semi_join},
#' \code{anti_join}), the matrix is subset accordingly.
#'
#' All joins invalidate stored analyses, as the matrix dimensions may have changed.
#'
#' @param x A tidymatrix object
#' @param y A data frame or tibble to join with
#' @param by A character vector of variables to join by. If NULL, uses all
#'   variables that appear in both tables.
#' @param copy If \code{y} is not a data frame or tibble, this controls whether
#'   to copy it or not.
#' @param suffix If there are non-joined duplicate variables in \code{x} and
#'   \code{y}, these suffixes will be added to disambiguate them.
#' @param ... Additional arguments passed to the corresponding dplyr join function
#' @param keep Control which join keys to preserve in the output (see
#'   \code{\link[dplyr]{left_join}}).
#'
#' @return A tidymatrix object with joined metadata and updated matrix
#'
#' @examples
#' # Create example tidymatrix
#' mat <- matrix(rnorm(50), nrow = 10, ncol = 5)
#' row_data <- data.frame(gene_id = paste0("Gene_", 1:10))
#' col_data <- data.frame(sample_id = paste0("Sample_", 1:5))
#' tm <- tidymatrix(mat, row_data, col_data)
#'
#' # Create external annotation data
#' annotations <- data.frame(
#'   gene_id = paste0("Gene_", c(1:8, 15:17)),
#'   pathway = sample(c("A", "B"), 11, replace = TRUE)
#' )
#'
#' # Left join - keep all genes from tidymatrix
#' tm_left <- tm |>
#'   activate(rows) |>
#'   left_join(annotations, by = "gene_id")
#'
#' # Inner join - keep only matching genes
#' tm_inner <- tm |>
#'   activate(rows) |>
#'   inner_join(annotations, by = "gene_id")
#'
#' # Full join - keep all genes from both
#' tm_full <- tm |>
#'   activate(rows) |>
#'   full_join(annotations, by = "gene_id")
#'
#' @name joins
#' @importFrom dplyr left_join right_join inner_join full_join semi_join anti_join
NULL

#' @rdname joins
#' @export
left_join.tidymatrix <- function(x, y, by = NULL, copy = FALSE,
                                 suffix = c(".x", ".y"), ..., keep = NULL) {
  perform_join(x, y, by = by, join_type = "left", suffix = suffix,
               copy = copy, keep = keep, ...)
}

#' @rdname joins
#' @export
right_join.tidymatrix <- function(x, y, by = NULL, copy = FALSE,
                                  suffix = c(".x", ".y"), ..., keep = NULL) {
  perform_join(x, y, by = by, join_type = "right", suffix = suffix,
               copy = copy, keep = keep, ...)
}

#' @rdname joins
#' @export
inner_join.tidymatrix <- function(x, y, by = NULL, copy = FALSE,
                                  suffix = c(".x", ".y"), ..., keep = NULL) {
  perform_join(x, y, by = by, join_type = "inner", suffix = suffix,
               copy = copy, keep = keep, ...)
}

#' @rdname joins
#' @export
full_join.tidymatrix <- function(x, y, by = NULL, copy = FALSE,
                                 suffix = c(".x", ".y"), ..., keep = NULL) {
  perform_join(x, y, by = by, join_type = "full", suffix = suffix,
               copy = copy, keep = keep, ...)
}

#' @rdname joins
#' @export
semi_join.tidymatrix <- function(x, y, by = NULL, copy = FALSE, ...) {
  perform_join(x, y, by = by, join_type = "semi", copy = copy, ...)
}

#' @rdname joins
#' @export
anti_join.tidymatrix <- function(x, y, by = NULL, copy = FALSE, ...) {
  perform_join(x, y, by = by, join_type = "anti", copy = copy, ...)
}
