#' Create a pheatmap from tidymatrix
#'
#' Generate a heatmap using the pheatmap package, automatically using
#' tidymatrix metadata for annotations and stored clustering results.
#'
#' @param x A tidymatrix object
#' @param row_names Column name from row_data to use for row names.
#'   If NULL, uses sequential numbers.
#' @param col_names Column name from col_data to use for column names.
#'   If NULL, uses sequential numbers.
#' @param row_annotation Character vector of column names from row_data to
#'   include as row annotations. If NULL, includes all columns except the
#'   name column. Set to FALSE to exclude row annotations.
#' @param col_annotation Character vector of column names from col_data to
#'   include as column annotations. If NULL, includes all columns except the
#'   name column. Set to FALSE to exclude column annotations.
#' @param row_cluster Name of stored hclust analysis to use for row clustering,
#'   or TRUE to let pheatmap cluster, or FALSE for no clustering, or NULL to
#'   auto-detect stored clustering. Default NULL (auto-detect).
#' @param col_cluster Name of stored hclust analysis to use for column clustering,
#'   or TRUE to let pheatmap cluster, or FALSE for no clustering, or NULL to
#'   auto-detect stored clustering. Default NULL (auto-detect).
#' @param ... Additional arguments passed to \code{pheatmap::pheatmap()}
#'
#' @return A pheatmap object
#' @export
#'
#' @examples
#' \dontrun{
#' library(pheatmap)
#' mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' row_data <- data.frame(
#'   gene = paste0("Gene_", 1:10),
#'   type = rep(c("A", "B"), each = 5)
#' )
#' col_data <- data.frame(
#'   sample = paste0("Sample_", 1:10),
#'   condition = rep(c("Control", "Treatment"), 5)
#' )
#' tm <- tidymatrix(mat, row_data, col_data)
#'
#' # Basic heatmap (auto-detects stored clustering if available)
#' plot_pheatmap(tm, row_names = "gene", col_names = "sample")
#'
#' # With stored clustering (auto-detected)
#' tm <- tm |>
#'   activate(rows) |>
#'   compute_hclust(k = 2, name = "gene_clusters") |>
#'   activate(columns) |>
#'   compute_hclust(k = 2, name = "sample_clusters")
#'
#' # Auto-detects and uses gene_clusters and sample_clusters
#' plot_pheatmap(tm, row_names = "gene", col_names = "sample")
#'
#' # Explicitly specify which clustering to use
#' plot_pheatmap(tm,
#'   row_names = "gene",
#'   col_names = "sample",
#'   row_cluster = "gene_clusters",
#'   col_cluster = "sample_clusters"
#' )
#'
#' # No clustering (explicit)
#' plot_pheatmap(tm,
#'   row_names = "gene",
#'   col_names = "sample",
#'   row_cluster = FALSE,
#'   col_cluster = FALSE
#' )
#'
#' # Automatic pheatmap clustering (not using stored)
#' plot_pheatmap(tm,
#'   row_names = "gene",
#'   col_names = "sample",
#'   row_cluster = TRUE,
#'   col_cluster = TRUE
#' )
#' }
plot_pheatmap <- function(x,
                          row_names = NULL,
                          col_names = NULL,
                          row_annotation = NULL,
                          col_annotation = NULL,
                          row_cluster = NULL,
                          col_cluster = NULL,
                          ...) {
  if (!inherits(x, "tidymatrix")) {
    stop("x must be a tidymatrix object", call. = FALSE)
  }

  # Check if pheatmap is available
  if (!requireNamespace("pheatmap", quietly = TRUE)) {
    stop(
      "Package 'pheatmap' is required for this function.\n",
      "Install it with: install.packages('pheatmap')",
      call. = FALSE
    )
  }

  # Extract matrix
  mat <- x$matrix

  # Set row names
  if (!is.null(row_names)) {
    if (!row_names %in% names(x$row_data)) {
      stop("row_names '", row_names, "' not found in row_data", call. = FALSE)
    }
    rownames(mat) <- as.character(x$row_data[[row_names]])
  } else {
    rownames(mat) <- as.character(seq_len(nrow(mat)))
  }

  # Set column names
  if (!is.null(col_names)) {
    if (!col_names %in% names(x$col_data)) {
      stop("col_names '", col_names, "' not found in col_data", call. = FALSE)
    }
    colnames(mat) <- as.character(x$col_data[[col_names]])
  } else {
    colnames(mat) <- as.character(seq_len(ncol(mat)))
  }

  # Prepare row annotations
  annotation_row <- NULL
  if (!identical(row_annotation, FALSE)) {
    if (is.null(row_annotation)) {
      # Use all columns except the name column
      exclude_cols <- c(row_names)
      row_annotation <- setdiff(names(x$row_data), exclude_cols)
    }

    if (length(row_annotation) > 0) {
      annotation_row <- x$row_data[, row_annotation, drop = FALSE]
      rownames(annotation_row) <- rownames(mat)

      # Convert character columns to factors for better colors
      for (col in names(annotation_row)) {
        if (is.character(annotation_row[[col]])) {
          annotation_row[[col]] <- factor(annotation_row[[col]])
        }
      }
    }
  }

  # Prepare column annotations
  annotation_col <- NULL
  if (!identical(col_annotation, FALSE)) {
    if (is.null(col_annotation)) {
      # Use all columns except the name column
      exclude_cols <- c(col_names)
      col_annotation <- setdiff(names(x$col_data), exclude_cols)
    }

    if (length(col_annotation) > 0) {
      annotation_col <- x$col_data[, col_annotation, drop = FALSE]
      rownames(annotation_col) <- colnames(mat)

      # Convert character columns to factors
      for (col in names(annotation_col)) {
        if (is.character(annotation_col[[col]])) {
          annotation_col[[col]] <- factor(annotation_col[[col]])
        }
      }
    }
  }

  # Handle row clustering
  cluster_rows <- FALSE
  if (is.null(row_cluster)) {
    # Auto-detect: look for stored hclust analyses on rows
    analyses <- attr(x, "analyses")
    if (!is.null(analyses) && length(analyses) > 0) {
      # Find hclust objects computed on rows
      row_hclusts <- character(0)
      for (name in names(analyses)) {
        analysis <- analyses[[name]]
        if (inherits(analysis$object, "hclust") &&
            !is.null(analysis$metadata$active) &&
            analysis$metadata$active == "rows") {
          row_hclusts <- c(row_hclusts, name)
        }
      }

      if (length(row_hclusts) == 1) {
        # Found exactly one row hclust, use it
        cluster_rows <- get_analysis(x, row_hclusts[1])
      } else if (length(row_hclusts) > 1) {
        # Multiple row clusterings found, use the first one
        cluster_rows <- get_analysis(x, row_hclusts[1])
        message("Multiple row clusterings found, using: ", row_hclusts[1])
      }
      # If length is 0, cluster_rows stays FALSE
    }
  } else if (is.character(row_cluster)) {
    # Use specified stored hclust
    if (!row_cluster %in% list_analyses(x)) {
      stop(
        "Row cluster '", row_cluster, "' not found in stored analyses.\n",
        "Available: ", paste(list_analyses(x), collapse = ", "),
        call. = FALSE
      )
    }
    hc_row <- get_analysis(x, row_cluster)
    if (!inherits(hc_row, "hclust")) {
      stop("Analysis '", row_cluster, "' is not an hclust object", call. = FALSE)
    }
    cluster_rows <- hc_row
  } else if (is.logical(row_cluster)) {
    cluster_rows <- row_cluster
  }

  # Handle column clustering
  cluster_cols <- FALSE
  if (is.null(col_cluster)) {
    # Auto-detect: look for stored hclust analyses on columns
    analyses <- attr(x, "analyses")
    if (!is.null(analyses) && length(analyses) > 0) {
      # Find hclust objects computed on columns
      col_hclusts <- character(0)
      for (name in names(analyses)) {
        analysis <- analyses[[name]]
        if (inherits(analysis$object, "hclust") &&
            !is.null(analysis$metadata$active) &&
            analysis$metadata$active == "columns") {
          col_hclusts <- c(col_hclusts, name)
        }
      }

      if (length(col_hclusts) == 1) {
        # Found exactly one column hclust, use it
        cluster_cols <- get_analysis(x, col_hclusts[1])
      } else if (length(col_hclusts) > 1) {
        # Multiple column clusterings found, use the first one
        cluster_cols <- get_analysis(x, col_hclusts[1])
        message("Multiple column clusterings found, using: ", col_hclusts[1])
      }
      # If length is 0, cluster_cols stays FALSE
    }
  } else if (is.character(col_cluster)) {
    # Use specified stored hclust
    if (!col_cluster %in% list_analyses(x)) {
      stop(
        "Column cluster '", col_cluster, "' not found in stored analyses.\n",
        "Available: ", paste(list_analyses(x), collapse = ", "),
        call. = FALSE
      )
    }
    hc_col <- get_analysis(x, col_cluster)
    if (!inherits(hc_col, "hclust")) {
      stop("Analysis '", col_cluster, "' is not an hclust object", call. = FALSE)
    }
    cluster_cols <- hc_col
  } else if (is.logical(col_cluster)) {
    cluster_cols <- col_cluster
  }

  # Call pheatmap
  pheatmap::pheatmap(
    mat,
    annotation_row = annotation_row,
    annotation_col = annotation_col,
    cluster_rows = cluster_rows,
    cluster_cols = cluster_cols,
    ...
  )
}
