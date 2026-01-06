#' Compute hierarchical clustering on tidymatrix
#'
#' Perform hierarchical clustering on the matrix, adding cluster assignments
#' to metadata and optionally storing the full hclust object.
#'
#' This function wraps \code{stats::hclust()} and \code{stats::dist()},
#' passing additional parameters directly to them.
#'
#' @param x A tidymatrix object
#' @param k Number of clusters to cut the tree into. If NULL, no cluster
#'   assignments are added (only dendrogram is stored).
#' @param h Height at which to cut the tree. Alternative to \code{k}.
#' @param name Name for this analysis. Default is "row_hclust" or "column_hclust"
#'   depending on active component.
#' @param store If TRUE, stores the full hclust object for later retrieval
#'   with \code{get_analysis()}. Default is TRUE.
#' @param method Agglomeration method for hclust. Default is "complete".
#'   Options: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty",
#'   "median", "centroid".
#' @param dist_method Distance method for dist(). Default is "euclidean".
#'   Options: "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski".
#' @param ... Additional arguments passed to \code{stats::dist()}
#'
#' @return A tidymatrix object with cluster assignments added to metadata
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' row_data <- data.frame(id = 1:10, group = rep(c("A", "B"), each = 5))
#' tm <- tidymatrix(mat, row_data)
#'
#' # Cluster rows into 3 groups
#' tm <- tm |>
#'   activate(rows) |>
#'   compute_hclust(k = 3, method = "ward.D2")
#'
#' # Now row_data has row_hclust_cluster column
#'
#' # Get full hclust object for plotting
#' hc <- get_analysis(tm, "row_hclust")
#' plot(hc)
#'
#' # Multiple clusterings with different k
#' tm <- tm |>
#'   activate(rows) |>
#'   compute_hclust(k = 3, name = "gene_k3") |>
#'   compute_hclust(k = 5, name = "gene_k5")
compute_hclust <- function(x, k = NULL, h = NULL, name = NULL, store = TRUE,
                           method = "complete", dist_method = "euclidean", ...) {
  if (!inherits(x, "tidymatrix")) {
    stop("x must be a tidymatrix object", call. = FALSE)
  }

  if (x$active == "matrix") {
    stop(
      "Cannot compute clustering when matrix is active.\\n",
      "  Use activate(rows) or activate(columns) first.",
      call. = FALSE
    )
  }

  # Default name based on active component
  if (is.null(name)) {
    # Convert "rows" to "row" and "columns" to "column"
    active_singular <- if (x$active == "rows") "row" else "column"
    name <- paste0(active_singular, "_hclust")
  }

  # Compute distance matrix
  # For rows: distance between rows (genes)
  # For columns: distance between columns (samples)
  if (x$active == "rows") {
    dist_mat <- stats::dist(x$matrix, method = dist_method, ...)
  } else {
    dist_mat <- stats::dist(t(x$matrix), method = dist_method, ...)
  }

  # Perform hierarchical clustering
  hc_result <- stats::hclust(dist_mat, method = method)

  # Cut tree if k or h specified
  if (!is.null(k) || !is.null(h)) {
    clusters <- stats::cutree(hc_result, k = k, h = h)

    # Create column name: {name}_cluster
    cluster_colname <- paste0(name, "_cluster")

    # Add to appropriate metadata
    if (x$active == "rows") {
      x$row_data[[cluster_colname]] <- as.factor(clusters)
    } else {
      x$col_data[[cluster_colname]] <- as.factor(clusters)
    }
  }

  # Store full hclust object if requested
  if (store) {
    x <- store_analysis(x, name, hc_result, x$active)
  }

  x
}

#' Compute k-means clustering on tidymatrix
#'
#' Perform k-means clustering on the matrix, adding cluster assignments
#' to metadata and optionally storing the full kmeans object.
#'
#' This function wraps \code{stats::kmeans()}, passing additional parameters
#' directly to it.
#'
#' @param x A tidymatrix object
#' @param centers Number of clusters (k) or a set of initial cluster centers.
#' @param name Name for this analysis. Default is "row_kmeans" or "column_kmeans"
#'   depending on active component.
#' @param store If TRUE, stores the full kmeans object for later retrieval
#'   with \code{get_analysis()}. Default is TRUE.
#' @param ... Additional arguments passed to \code{stats::kmeans()}, such as
#'   \code{iter.max}, \code{nstart}, \code{algorithm}, etc.
#'
#' @return A tidymatrix object with cluster assignments added to metadata
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' row_data <- data.frame(id = 1:10)
#' tm <- tidymatrix(mat, row_data)
#'
#' # K-means clustering with k=3
#' tm <- tm |>
#'   activate(rows) |>
#'   compute_kmeans(centers = 3, nstart = 25)
#'
#' # Now row_data has row_kmeans_cluster column
#'
#' # Get full kmeans object
#' km <- get_analysis(tm, "row_kmeans")
#' km$tot.withinss  # Total within-cluster sum of squares
compute_kmeans <- function(x, centers, name = NULL, store = TRUE, ...) {
  if (!inherits(x, "tidymatrix")) {
    stop("x must be a tidymatrix object", call. = FALSE)
  }

  if (x$active == "matrix") {
    stop(
      "Cannot compute k-means when matrix is active.\\n",
      "  Use activate(rows) or activate(columns) first.",
      call. = FALSE
    )
  }

  # Default name based on active component
  if (is.null(name)) {
    # Convert "rows" to "row" and "columns" to "column"
    active_singular <- if (x$active == "rows") "row" else "column"
    name <- paste0(active_singular, "_kmeans")
  }

  # Perform k-means
  # For rows: cluster rows (genes)
  # For columns: cluster columns (samples)
  if (x$active == "rows") {
    km_result <- stats::kmeans(x$matrix, centers = centers, ...)
  } else {
    km_result <- stats::kmeans(t(x$matrix), centers = centers, ...)
  }

  # Create column name: {name}_cluster
  cluster_colname <- paste0(name, "_cluster")

  # Add to appropriate metadata
  if (x$active == "rows") {
    x$row_data[[cluster_colname]] <- as.factor(km_result$cluster)
  } else {
    x$col_data[[cluster_colname]] <- as.factor(km_result$cluster)
  }

  # Store full kmeans object if requested
  if (store) {
    x <- store_analysis(x, name, km_result, x$active)
  }

  x
}
