#' Matrix Operations for tidymatrix
#'
#' Functions for transforming and manipulating the matrix component of
#' tidymatrix objects.
#'
#' @name matrix-operations
NULL

#' @export
center <- function(x, ...) {
  UseMethod("center")
}

# Note: scale() is already a base R generic, so we don't need to create it
# Note: transform() is also a base R generic
# Note: t() is a base R generic, so we register t.tidymatrix directly

#' Transpose a tidymatrix
#'
#' Transpose the matrix and swap row and column metadata. This operation
#' works regardless of which component is active. Uses base R's \code{t()}
#' generic, so it works correctly even when tidyverse is loaded.
#'
#' @param x A tidymatrix object
#'
#' @return A tidymatrix object with transposed matrix and swapped metadata
#' @export
#'
#' @examples
#' mat <- matrix(1:12, nrow = 3, ncol = 4)
#' row_data <- data.frame(gene = paste0("G", 1:3))
#' col_data <- data.frame(sample = paste0("S", 1:4))
#' tm <- tidymatrix(mat, row_data, col_data)
#'
#' # Transpose: genes × samples → samples × genes
#' tm_t <- t(tm)
#' dim(tm_t$matrix)  # Now 4 × 3
t.tidymatrix <- function(x) {
  # Transpose the matrix
  x$matrix <- t(x$matrix)

  # Swap row and column metadata
  temp <- x$row_data
  x$row_data <- x$col_data
  x$col_data <- temp

  # Update active if it was rows or columns
  if (x$active == "rows") {
    x$active <- "columns"
  } else if (x$active == "columns") {
    x$active <- "rows"
  }

  # Remove all stored analyses since structure changed
  x <- remove_all_analyses(x, "transpose")

  x
}

#' Scale rows or columns
#'
#' Perform z-score scaling (center and scale to unit variance) on the active
#' dimension. Requires rows or columns to be active.
#'
#' @param x A tidymatrix object with rows or columns active
#' @param center If TRUE (default), center to mean = 0
#' @param scale If TRUE (default), scale to sd = 1
#'
#' @return A tidymatrix object with scaled matrix
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(20, mean = 10, sd = 5), nrow = 4, ncol = 5)
#' tm <- tidymatrix(mat)
#'
#' # Scale rows (z-score per row)
#' tm_scaled <- tm |>
#'   activate(rows) |>
#'   scale()
#'
#' # Scale columns
#' tm_scaled <- tm |>
#'   activate(columns) |>
#'   scale()
#'
#' # Only center, don't scale
#' tm_centered <- tm |>
#'   activate(rows) |>
#'   scale(center = TRUE, scale = FALSE)
scale.tidymatrix <- function(x, center = TRUE, scale = TRUE) {
  if (x$active == "matrix") {
    stop(
      "Cannot scale when matrix is active.\n",
      "  Use activate(rows) or activate(columns) to specify which dimension to scale.",
      call. = FALSE
    )
  }

  if (x$active == "rows") {
    # Scale each row
    x$matrix <- t(scale(t(x$matrix), center = center, scale = scale))
  } else if (x$active == "columns") {
    # Scale each column
    x$matrix <- scale(x$matrix, center = center, scale = scale)
  }

  # Remove stored analyses since matrix values changed
  x <- remove_all_analyses(x, "scale")

  x
}

#' Center rows or columns
#'
#' Center the active dimension by subtracting the mean. Requires rows or
#' columns to be active.
#'
#' @param x A tidymatrix object with rows or columns active
#'
#' @return A tidymatrix object with centered matrix
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(20, mean = 10), nrow = 4, ncol = 5)
#' tm <- tidymatrix(mat)
#'
#' # Center rows (mean of each row = 0)
#' tm_centered <- tm |>
#'   activate(rows) |>
#'   center()
#'
#' # Center columns (mean of each column = 0)
#' tm_centered <- tm |>
#'   activate(columns) |>
#'   center()
center.tidymatrix <- function(x) {
  if (x$active == "matrix") {
    stop(
      "Cannot center when matrix is active.\n",
      "  Use activate(rows) or activate(columns) to specify which dimension to center.",
      call. = FALSE
    )
  }

  if (x$active == "rows") {
    # Center each row
    row_means <- rowMeans(x$matrix, na.rm = TRUE)
    x$matrix <- x$matrix - row_means
  } else if (x$active == "columns") {
    # Center each column
    col_means <- colMeans(x$matrix, na.rm = TRUE)
    x$matrix <- sweep(x$matrix, 2, col_means, "-")
  }

  # Remove stored analyses since matrix values changed
  x <- remove_all_analyses(x, "center")

  x
}

#' Transform matrix values
#'
#' Apply an element-wise transformation to the entire matrix. Requires
#' matrix to be active.
#'
#' @param .data A tidymatrix object with matrix active
#' @param ... Transformation expression using \code{.} to refer to the matrix
#'
#' @return A tidymatrix object with transformed matrix
#' @export
#'
#' @examples
#' mat <- matrix(1:12, nrow = 3, ncol = 4)
#' tm <- tidymatrix(mat)
#'
#' # Log transform
#' tm_log <- tm |>
#'   activate(matrix) |>
#'   transform(log2(. + 1))
#'
#' # Square root
#' tm_sqrt <- tm |>
#'   activate(matrix) |>
#'   transform(sqrt(.))
#'
#' # Custom transformation
#' tm_custom <- tm |>
#'   activate(matrix) |>
#'   transform(pmin(., 10))  # Cap at 10
transform.tidymatrix <- function(.data, ...) {
  if (.data$active != "matrix") {
    stop(
      "Cannot transform when matrix is not active.\n",
      "  Use activate(matrix) first.",
      call. = FALSE
    )
  }

  # Capture the transformation expression
  dots <- rlang::enquos(...)
  if (length(dots) != 1) {
    stop("transform requires exactly one expression", call. = FALSE)
  }
  expr <- dots[[1]]

  # Apply transformation using . as placeholder for matrix
  .data$matrix <- rlang::eval_tidy(expr, data = list(. = .data$matrix))

  # Remove stored analyses since matrix values changed
  .data <- remove_all_analyses(.data, "transform")

  .data
}

#' Add statistics to metadata
#'
#' Compute statistics for the active dimension and add them as columns to
#' the corresponding metadata. Requires rows or columns to be active.
#'
#' @param .data A tidymatrix object with rows or columns active
#' @param ... Functions to compute statistics (unquoted names like mean, var, sd)
#' @param .fns Alternative way to specify functions as a list
#' @param .names Names for the new columns. If NULL, uses function names.
#'
#' @return A tidymatrix object with statistics added to metadata
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(20), nrow = 4, ncol = 5)
#' tm <- tidymatrix(mat)
#'
#' # Add row statistics
#' tm <- tm |>
#'   activate(rows) |>
#'   add_stats(mean, var, sd)
#'
#' # Now row_data has columns: mean, var, sd
#'
#' # Add column statistics
#' tm <- tm |>
#'   activate(columns) |>
#'   add_stats(median, min, max)
#'
#' # Custom names
#' tm <- tm |>
#'   activate(rows) |>
#'   add_stats(mean, var, .names = c("row_mean", "row_var"))
add_stats <- function(.data, ..., .fns = NULL, .names = NULL) {
  if (!inherits(.data, "tidymatrix")) {
    stop(".data must be a tidymatrix object", call. = FALSE)
  }

  if (.data$active == "matrix") {
    stop(
      "Cannot add_stats when matrix is active.\n",
      "  Use activate(rows) or activate(columns) to specify which dimension.",
      call. = FALSE
    )
  }

  # Capture function arguments
  fns <- rlang::enquos(...)

  # Use .fns if provided
  if (!is.null(.fns)) {
    if (!is.list(.fns)) {
      stop(".fns must be a list of functions", call. = FALSE)
    }
    fn_list <- .fns
    fn_names <- names(.fns)
    if (is.null(fn_names)) {
      fn_names <- paste0("stat", seq_along(.fns))
    }
  } else {
    # Extract function names from enquos
    fn_names <- names(fns)
    if (is.null(fn_names) || any(fn_names == "")) {
      # Use deparsed expressions as names
      fn_names <- vapply(fns, function(x) rlang::as_label(x), character(1))
    }
    fn_list <- lapply(fns, rlang::eval_tidy)
  }

  # Override with custom names if provided
  if (!is.null(.names)) {
    if (length(.names) != length(fn_list)) {
      stop(".names must have same length as number of functions", call. = FALSE)
    }
    fn_names <- .names
  }

  # Compute statistics
  if (.data$active == "rows") {
    # Compute for each row
    for (i in seq_along(fn_list)) {
      fn <- fn_list[[i]]
      stat_name <- fn_names[i]
      .data$row_data[[stat_name]] <- apply(.data$matrix, 1, fn)
    }
  } else if (.data$active == "columns") {
    # Compute for each column
    for (i in seq_along(fn_list)) {
      fn <- fn_list[[i]]
      stat_name <- fn_names[i]
      .data$col_data[[stat_name]] <- apply(.data$matrix, 2, fn)
    }
  }

  .data
}

#' Log transform matrix
#'
#' Apply log transformation to matrix values. Requires matrix to be active.
#' This is a convenience wrapper around \code{transform()}.
#'
#' @param .data A tidymatrix object with matrix active
#' @param base Logarithm base (2, 10, or "natural" for ln)
#' @param offset Value to add before log transform (default 1, for log(x + 1))
#'
#' @return A tidymatrix object with log-transformed matrix
#' @export
#'
#' @examples
#' mat <- matrix(abs(rnorm(20)), nrow = 4, ncol = 5)
#' tm <- tidymatrix(mat)
#'
#' # Log2 transform with pseudocount
#' tm_log <- tm |>
#'   activate(matrix) |>
#'   log_transform(base = 2, offset = 1)
#'
#' # Natural log
#' tm_ln <- tm |>
#'   activate(matrix) |>
#'   log_transform(base = "natural")
log_transform <- function(.data, base = 2, offset = 1) {
  if (!inherits(.data, "tidymatrix")) {
    stop(".data must be a tidymatrix object", call. = FALSE)
  }

  if (.data$active != "matrix") {
    stop(
      "Cannot log_transform when matrix is not active.\n",
      "  Use activate(matrix) first.",
      call. = FALSE
    )
  }

  # Apply log transformation
  if (base == 2) {
    .data$matrix <- log2(.data$matrix + offset)
  } else if (base == 10) {
    .data$matrix <- log10(.data$matrix + offset)
  } else if (base == "natural") {
    .data$matrix <- log(.data$matrix + offset)
  } else {
    .data$matrix <- log(.data$matrix + offset, base = base)
  }

  # Remove stored analyses since matrix values changed
  .data <- remove_all_analyses(.data, "log_transform")

  .data
}

#' Clip matrix values
#'
#' Cap matrix values at specified minimum and/or maximum. Requires matrix
#' to be active.
#'
#' @param .data A tidymatrix object with matrix active
#' @param min Minimum value (values below are set to this)
#' @param max Maximum value (values above are set to this)
#'
#' @return A tidymatrix object with clipped matrix values
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(20), nrow = 4, ncol = 5)
#' tm <- tidymatrix(mat)
#'
#' # Clip to [-2, 2] range
#' tm_clipped <- tm |>
#'   activate(matrix) |>
#'   clip_values(min = -2, max = 2)
#'
#' # Only set floor
#' tm_floor <- tm |>
#'   activate(matrix) |>
#'   clip_values(min = 0)
clip_values <- function(.data, min = NULL, max = NULL) {
  if (!inherits(.data, "tidymatrix")) {
    stop(".data must be a tidymatrix object", call. = FALSE)
  }

  if (.data$active != "matrix") {
    stop(
      "Cannot clip_values when matrix is not active.\n",
      "  Use activate(matrix) first.",
      call. = FALSE
    )
  }

  if (is.null(min) && is.null(max)) {
    warning("No min or max specified, returning unchanged", call. = FALSE)
    return(.data)
  }

  # Clip values
  if (!is.null(min)) {
    .data$matrix[.data$matrix < min] <- min
  }
  if (!is.null(max)) {
    .data$matrix[.data$matrix > max] <- max
  }

  # Remove stored analyses since matrix values changed
  .data <- remove_all_analyses(.data, "clip_values")

  .data
}
