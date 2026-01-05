#' Extract the active component from a tidymatrix
#'
#' Returns the currently active component of a tidymatrix object. This can be
#' the matrix itself, the row metadata, or the column metadata, depending on
#' what is currently activated.
#'
#' @param .data A tidymatrix object
#'
#' @return The active component:
#'   - If "matrix" is active: returns the numeric matrix
#'   - If "rows" is active: returns the row_data data.frame
#'   - If "columns" is active: returns the col_data data.frame
#'
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' row_data <- data.frame(id = 1:10, group = rep(c("A", "B"), each = 5))
#' tm <- tidymatrix(mat, row_data)
#'
#' # Extract matrix
#' tm |>
#'   activate(matrix) |>
#'   pull_active()
#'
#' # Extract row metadata for plotting
#' tm |>
#'   activate(rows) |>
#'   compute_prcomp(center = TRUE, scale. = TRUE) |>
#'   pull_active() |>
#'   ggplot(aes(x = row_pca_PC1, y = row_pca_PC2, color = group)) +
#'     geom_point()
pull_active <- function(.data) {
  if (!is_tidymatrix(.data)) {
    stop("pull_active() can only be used on tidymatrix objects", call. = FALSE)
  }

  if (.data$active == "matrix") {
    .data$matrix
  } else if (.data$active == "rows") {
    .data$row_data
  } else if (.data$active == "columns") {
    .data$col_data
  } else {
    stop(
      "Invalid active component: ", .data$active,
      call. = FALSE
    )
  }
}

#' Convert tidymatrix metadata to tibble
#'
#' Converts the active metadata component (row_data or col_data) to a tibble.
#' This method only works when rows or columns are active, not when the matrix
#' is active.
#'
#' @param x A tidymatrix object
#' @param ... Additional arguments (currently unused)
#'
#' @return A tibble containing the active metadata
#'
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' row_data <- data.frame(id = 1:10, group = rep(c("A", "B"), each = 5))
#' tm <- tidymatrix(mat, row_data)
#'
#' # Convert row metadata to tibble for plotting
#' tm |>
#'   activate(rows) |>
#'   compute_prcomp(center = TRUE, scale. = TRUE) |>
#'   as_tibble() |>
#'   ggplot(aes(x = row_pca_PC1, y = row_pca_PC2, color = group)) +
#'     geom_point()
as_tibble.tidymatrix <- function(x, ...) {
  if (!is_tidymatrix(x)) {
    stop("x must be a tidymatrix object", call. = FALSE)
  }

  if (x$active == "matrix") {
    stop(
      "Cannot convert to tibble when matrix is active.\n",
      "  Use activate(rows) or activate(columns) first.\n",
      "  Or use pull_active() to extract the matrix directly.",
      call. = FALSE
    )
  }

  if (x$active == "rows") {
    tibble::as_tibble(x$row_data)
  } else if (x$active == "columns") {
    tibble::as_tibble(x$col_data)
  } else {
    stop(
      "Invalid active component: ", x$active,
      call. = FALSE
    )
  }
}

#' Convert tidymatrix metadata to data.frame
#'
#' Converts the active metadata component (row_data or col_data) to a data.frame.
#' This method only works when rows or columns are active, not when the matrix
#' is active.
#'
#' @param x A tidymatrix object
#' @param ... Additional arguments (currently unused)
#'
#' @return A data.frame containing the active metadata
#'
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' row_data <- data.frame(id = 1:10, group = rep(c("A", "B"), each = 5))
#' tm <- tidymatrix(mat, row_data)
#'
#' # Convert row metadata to data.frame
#' df <- tm |>
#'   activate(rows) |>
#'   compute_prcomp(center = TRUE, scale. = TRUE) |>
#'   as.data.frame()
as.data.frame.tidymatrix <- function(x, ...) {
  if (!is_tidymatrix(x)) {
    stop("x must be a tidymatrix object", call. = FALSE)
  }

  if (x$active == "matrix") {
    stop(
      "Cannot convert to data.frame when matrix is active.\n",
      "  Use activate(rows) or activate(columns) first.\n",
      "  Or use pull_active() to extract the matrix directly.",
      call. = FALSE
    )
  }

  if (x$active == "rows") {
    as.data.frame(x$row_data)
  } else if (x$active == "columns") {
    as.data.frame(x$col_data)
  } else {
    stop(
      "Invalid active component: ", x$active,
      call. = FALSE
    )
  }
}
