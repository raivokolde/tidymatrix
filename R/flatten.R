#' Convert tidymatrix to long-format data.frame
#'
#' Converts a tidymatrix into a long-format data.frame where each row represents
#' a single matrix cell with its associated row and column metadata. This is
#' useful for plotting individual data points or performing analyses that
#' require long-format data.
#'
#' @param .data A tidymatrix object
#' @param return_tibble Logical. If TRUE (default), returns tibble. If FALSE,
#'   returns data.frame.
#'
#' @return A data.frame/tibble with m*n rows (where m and n are matrix dimensions)
#'   containing:
#'   \itemize{
#'     \item All row_data columns (with "row." prefix if name conflicts exist)
#'     \item All col_data columns (with "col." prefix if name conflicts exist)
#'     \item A "value" column containing the matrix values
#'   }
#'
#' @details
#' The flatten operation always converts the entire matrix regardless of which
#' component is active. If column names conflict between row_data and col_data,
#' all row metadata columns are prefixed with "row." and all column metadata
#' columns are prefixed with "col." to avoid ambiguity.
#'
#' Matrix values are unwrapped in column-major order (R's default), meaning
#' all values from column 1, then all values from column 2, etc.
#'
#' @export
#'
#' @examples
#' # Basic flattening
#' mat <- matrix(1:12, nrow = 4, ncol = 3)
#' row_data <- data.frame(
#'   gene_id = paste0("Gene", 1:4),
#'   gene_type = c("A", "A", "B", "B")
#' )
#' col_data <- data.frame(
#'   sample_id = paste0("Sample", 1:3),
#'   condition = c("Control", "Treatment", "Control")
#' )
#' tm <- tidymatrix(mat, row_data, col_data)
#'
#' flat <- flatten(tm)
#' head(flat)
#'
#' \dontrun{
#' # Use in ggplot2 workflow
#' library(ggplot2)
#' tm |>
#'   flatten() |>
#'   ggplot(aes(x = sample_id, y = value, color = condition)) +
#'   geom_point() +
#'   facet_wrap(~gene_id)
#'
#' # After statistical analysis
#' results <- tm |>
#'   activate(rows) |>
#'   compute_ttest(group_col = "condition", add_to_data = TRUE) |>
#'   flatten()
#'
#' # Now filter to significant genes and plot
#' results |>
#'   filter(p.adj < 0.05) |>
#'   ggplot(aes(x = sample, y = value, color = condition)) +
#'   geom_point() +
#'   facet_wrap(~gene)
#' }
flatten <- function(.data, return_tibble = TRUE) {
  # 1. Validate input
  if (!is_tidymatrix(.data)) {
    stop("flatten() can only be used on tidymatrix objects", call. = FALSE)
  }

  # 2. Extract dimensions
  m <- nrow(.data$matrix)
  n <- ncol(.data$matrix)

  # 3. Convert matrix to long format
  # as.vector() in R converts column-by-column (column-major order)
  values <- as.vector(.data$matrix)
  row_indices <- rep(seq_len(m), times = n)
  col_indices <- rep(seq_len(n), each = m)

  # 4. Detect column name conflicts
  row_names <- names(.data$row_data)
  col_names <- names(.data$col_data)
  conflicts <- intersect(row_names, col_names)
  has_conflicts <- length(conflicts) > 0

  # 5. Prepare metadata with prefixes if needed
  if (has_conflicts) {
    # Prefix ALL columns to maintain consistency
    row_meta <- .data$row_data[row_indices, , drop = FALSE]
    col_meta <- .data$col_data[col_indices, , drop = FALSE]
    names(row_meta) <- paste0("row.", names(row_meta))
    names(col_meta) <- paste0("col.", names(col_meta))
  } else {
    # No conflicts - keep original names
    row_meta <- .data$row_data[row_indices, , drop = FALSE]
    col_meta <- .data$col_data[col_indices, , drop = FALSE]
  }

  # 6. Combine into data.frame
  result <- cbind(row_meta, col_meta, data.frame(value = values))

  # Reset row names to avoid confusion
  rownames(result) <- NULL

  # 7. Return based on return_tibble parameter
  if (return_tibble) {
    return(tibble::as_tibble(result))
  } else {
    return(result)
  }
}
