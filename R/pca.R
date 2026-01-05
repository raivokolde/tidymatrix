#' Compute PCA on tidymatrix
#'
#' Perform Principal Component Analysis on the matrix, adding PC scores to
#' metadata and optionally storing the full prcomp object.
#'
#' This function wraps \code{stats::prcomp()} and passes all additional
#' parameters directly to it. The PC scores are added as columns to the
#' active metadata (row_data or col_data).
#'
#' @param x A tidymatrix object
#' @param name Name for this analysis. Default is "row_pca" or "column_pca"
#'   depending on active component. Used as prefix for column names.
#' @param n_components Number of PC components to add to metadata. Default is
#'   all components. Use a smaller number for large datasets.
#' @param store If TRUE, stores the full prcomp object for later retrieval
#'   with \code{get_analysis()}. Default is TRUE.
#' @param ... Additional arguments passed to \code{stats::prcomp()}, such as
#'   \code{center}, \code{scale.}, \code{tol}, etc.
#'
#' @return A tidymatrix object with PC scores added to metadata
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' row_data <- data.frame(id = 1:10, group = rep(c("A", "B"), each = 5))
#' col_data <- data.frame(id = 1:10, type = rep(c("x", "y"), 5))
#' tm <- tidymatrix(mat, row_data, col_data)
#'
#' # PCA on columns (samples)
#' tm <- tm |>
#'   activate(columns) |>
#'   compute_prcomp(center = TRUE, scale. = TRUE)
#'
#' # Now col_data has column_pca_PC1, column_pca_PC2, etc.
#'
#' # Custom name and limited components
#' tm <- tm |>
#'   activate(rows) |>
#'   compute_prcomp(name = "gene_pca", n_components = 3, center = TRUE)
#'
#' # Get full prcomp object
#' pca_obj <- get_analysis(tm, "gene_pca")
#' summary(pca_obj)
#' plot(pca_obj$sdev^2 / sum(pca_obj$sdev^2))  # Variance explained
compute_prcomp <- function(x, name = NULL, n_components = NULL, store = TRUE, ...) {
  if (!inherits(x, "tidymatrix")) {
    stop("x must be a tidymatrix object", call. = FALSE)
  }

  if (x$active == "matrix") {
    stop(
      "Cannot compute PCA when matrix is active.\\n",
      "  Use activate(rows) or activate(columns) first.",
      call. = FALSE
    )
  }

  # Default name based on active component
  if (is.null(name)) {
    # Convert "rows" to "row" and "columns" to "column"
    active_singular <- if (x$active == "rows") "row" else "column"
    name <- paste0(active_singular, "_pca")
  }

  # Perform PCA
  # For rows: PCA on matrix (genes × samples -> genes in PC space)
  # For columns: PCA on transposed matrix (samples × genes -> samples in PC space)
  if (x$active == "rows") {
    pca_result <- stats::prcomp(x$matrix, ...)
  } else {
    pca_result <- stats::prcomp(t(x$matrix), ...)
  }

  # Determine how many components to add
  n_pc <- ncol(pca_result$x)
  if (is.null(n_components)) {
    n_components <- n_pc
  } else {
    n_components <- min(n_components, n_pc)
  }

  # Extract PC scores
  pc_scores <- pca_result$x[, 1:n_components, drop = FALSE]

  # Create column names: {name}_PC1, {name}_PC2, etc.
  pc_colnames <- paste0(name, "_PC", 1:n_components)
  colnames(pc_scores) <- pc_colnames

  # Add to appropriate metadata
  if (x$active == "rows") {
    x$row_data <- cbind(x$row_data, as.data.frame(pc_scores))
  } else {
    x$col_data <- cbind(x$col_data, as.data.frame(pc_scores))
  }

  # Store full prcomp object if requested
  if (store) {
    x <- store_analysis(x, name, pca_result, x$active)
  }

  x
}

#' Add PCA scores to metadata (simple version)
#'
#' A lightweight version of \code{compute_prcomp} that only adds PC scores
#' to metadata without storing the full prcomp object.
#'
#' @param x A tidymatrix object
#' @param n Number of components to compute. Default is 2.
#' @param name Name prefix for PC columns. Default is "PC".
#' @param ... Additional arguments passed to \code{stats::prcomp()}
#'
#' @return A tidymatrix object with PC scores added to metadata
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' tm <- tidymatrix(mat)
#'
#' # Quick PCA for visualization
#' tm <- tm |>
#'   activate(columns) |>
#'   add_pca_scores(n = 2, center = TRUE, scale. = TRUE)
#'
#' # Now col_data has PC_PC1, PC_PC2
add_pca_scores <- function(x, n = 2, name = "PC", ...) {
  compute_prcomp(x, name = name, n_components = n, store = FALSE, ...)
}
