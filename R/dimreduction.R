# Dimensionality Reduction Methods for tidymatrix

#' Compute MDS on tidymatrix
#'
#' Perform Classical Multidimensional Scaling on the matrix, adding MDS
#' coordinates to metadata and optionally storing the distance matrix and result.
#'
#' This function wraps \code{stats::cmdscale()} and \code{stats::dist()}.
#'
#' @param x A tidymatrix object
#' @param name Name for this analysis. Default is "row_mds" or "column_mds"
#'   depending on active component.
#' @param k Number of dimensions for MDS embedding. Default is 2.
#' @param store If TRUE, stores the MDS result for later retrieval
#'   with \code{get_analysis()}. Default is TRUE.
#' @param dist_method Distance method for dist(). Default is "euclidean".
#'   Options: "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski".
#' @param eig If TRUE, return eigenvalues and GOF statistics (passed to cmdscale).
#' @param ... Additional arguments passed to \code{stats::dist()} or
#'   \code{stats::cmdscale()}
#'
#' @return A tidymatrix object with MDS coordinates added to metadata
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(500), nrow = 50, ncol = 10)
#' row_data <- data.frame(id = 1:50)
#' tm <- tidymatrix(mat, row_data)
#'
#' # MDS on rows
#' tm <- tm |>
#'   activate(rows) |>
#'   compute_mds(k = 2)
#'
#' # Now row_data has row_mds_1, row_mds_2 columns
#'
#' # Get MDS result
#' mds_obj <- get_analysis(tm, "row_mds")
compute_mds <- function(x, name = NULL, k = 2, store = TRUE,
                        dist_method = "euclidean", eig = FALSE, ...) {
  if (!inherits(x, "tidymatrix")) {
    stop("x must be a tidymatrix object", call. = FALSE)
  }

  if (x$active == "matrix") {
    stop(
      "Cannot compute MDS when matrix is active.\n",
      "  Use activate(rows) or activate(columns) first.",
      call. = FALSE
    )
  }

  # Default name based on active component
  if (is.null(name)) {
    # Convert "rows" to "row" and "columns" to "column"
    active_singular <- if (x$active == "rows") "row" else "column"
    name <- paste0(active_singular, "_mds")
  }

  # Compute distance matrix
  if (x$active == "rows") {
    dist_mat <- stats::dist(x$matrix, method = dist_method)
  } else {
    dist_mat <- stats::dist(t(x$matrix), method = dist_method)
  }

  # Perform MDS
  mds_result <- stats::cmdscale(dist_mat, k = k, eig = eig, ...)

  # Extract coordinates (cmdscale returns matrix or list depending on eig)
  if (eig) {
    mds_coords <- mds_result$points
  } else {
    mds_coords <- mds_result
  }

  # Create column names: {name}_1, {name}_2, etc.
  coord_colnames <- paste0(name, "_", 1:k)
  colnames(mds_coords) <- coord_colnames

  # Add to appropriate metadata
  if (x$active == "rows") {
    x$row_data <- cbind(x$row_data, as.data.frame(mds_coords))
  } else {
    x$col_data <- cbind(x$col_data, as.data.frame(mds_coords))
  }

  # Store result if requested
  if (store) {
    # Store both distance matrix and MDS result for completeness
    store_obj <- list(dist = dist_mat, mds = mds_result)
    x <- store_analysis(x, name, store_obj, x$active)
  }

  x
}

#' Compute t-SNE on tidymatrix
#'
#' Perform t-distributed Stochastic Neighbor Embedding on the matrix,
#' adding t-SNE coordinates to metadata and optionally storing the full
#' Rtsne object.
#'
#' This function wraps \code{Rtsne::Rtsne()} and passes all additional
#' parameters directly to it. Note that t-SNE is stochastic, so use
#' \code{set.seed()} before calling for reproducible results.
#'
#' @param x A tidymatrix object
#' @param name Name for this analysis. Default is "row_tsne" or "column_tsne"
#'   depending on active component.
#' @param dims Number of dimensions for t-SNE embedding. Default is 2.
#' @param store If TRUE, stores the full Rtsne object for later retrieval
#'   with \code{get_analysis()}. Default is TRUE.
#' @param perplexity Perplexity parameter (default 30). Should be less than
#'   the number of samples. Typical values are between 5 and 50.
#' @param ... Additional arguments passed to \code{Rtsne::Rtsne()}, such as
#'   \code{theta}, \code{max_iter}, \code{verbose}, etc.
#'
#' @return A tidymatrix object with t-SNE coordinates added to metadata
#' @export
#'
#' @examples
#' \dontrun{
#' mat <- matrix(rnorm(500), nrow = 50, ncol = 10)
#' row_data <- data.frame(id = 1:50)
#' tm <- tidymatrix(mat, row_data)
#'
#' # t-SNE on rows
#' set.seed(42)  # For reproducibility
#' tm <- tm |>
#'   activate(rows) |>
#'   compute_tsne(dims = 2, perplexity = 10)
#'
#' # Now row_data has row_tsne_1, row_tsne_2 columns
#'
#' # Get full Rtsne object
#' tsne_obj <- get_analysis(tm, "row_tsne")
#' }
compute_tsne <- function(x, name = NULL, dims = 2, store = TRUE,
                         perplexity = 30, ...) {
  if (!inherits(x, "tidymatrix")) {
    stop("x must be a tidymatrix object", call. = FALSE)
  }

  if (x$active == "matrix") {
    stop(
      "Cannot compute t-SNE when matrix is active.\n",
      "  Use activate(rows) or activate(columns) first.",
      call. = FALSE
    )
  }

  # Check that Rtsne package is available
  if (!requireNamespace("Rtsne", quietly = TRUE)) {
    stop(
      "Package 'Rtsne' is required for t-SNE.\n",
      "  Install it with: install.packages('Rtsne')",
      call. = FALSE
    )
  }

  # Default name based on active component
  if (is.null(name)) {
    # Convert "rows" to "row" and "columns" to "column"
    active_singular <- if (x$active == "rows") "row" else "column"
    name <- paste0(active_singular, "_tsne")
  }

  # Perform t-SNE
  if (x$active == "rows") {
    tsne_result <- Rtsne::Rtsne(x$matrix, dims = dims,
                                 perplexity = perplexity, ...)
  } else {
    tsne_result <- Rtsne::Rtsne(t(x$matrix), dims = dims,
                                 perplexity = perplexity, ...)
  }

  # Extract coordinates
  tsne_coords <- tsne_result$Y

  # Create column names: {name}_1, {name}_2, etc.
  coord_colnames <- paste0(name, "_", 1:dims)
  colnames(tsne_coords) <- coord_colnames

  # Add to appropriate metadata
  if (x$active == "rows") {
    x$row_data <- cbind(x$row_data, as.data.frame(tsne_coords))
  } else {
    x$col_data <- cbind(x$col_data, as.data.frame(tsne_coords))
  }

  # Store full Rtsne object if requested
  if (store) {
    x <- store_analysis(x, name, tsne_result, x$active)
  }

  x
}

#' Compute UMAP on tidymatrix
#'
#' Perform Uniform Manifold Approximation and Projection on the matrix,
#' adding UMAP coordinates to metadata and optionally storing the full
#' umap object.
#'
#' This function wraps \code{umap::umap()} and passes additional parameters
#' through the config parameter. UMAP is generally faster than t-SNE and
#' better preserves global structure.
#'
#' @param x A tidymatrix object
#' @param name Name for this analysis. Default is "row_umap" or "column_umap"
#'   depending on active component.
#' @param n_components Number of dimensions for UMAP embedding. Default is 2.
#' @param store If TRUE, stores the full umap object for later retrieval
#'   with \code{get_analysis()}. Default is TRUE.
#' @param n_neighbors Size of local neighborhood (default 15). Larger values
#'   preserve more global structure, smaller values preserve more local structure.
#' @param min_dist Minimum distance between points in low-dimensional space
#'   (default 0.1). Smaller values create tighter, more separated clusters.
#' @param metric Distance metric to use (default "euclidean"). Options include
#'   "manhattan", "cosine", "correlation", etc.
#' @param random_state Seed for reproducibility (default NULL). Set to an
#'   integer for reproducible results.
#' @param ... Additional configuration passed via \code{umap.defaults}
#'
#' @return A tidymatrix object with UMAP coordinates added to metadata
#' @export
#'
#' @examples
#' \dontrun{
#' mat <- matrix(rnorm(500), nrow = 50, ncol = 10)
#' row_data <- data.frame(id = 1:50)
#' tm <- tidymatrix(mat, row_data)
#'
#' # UMAP on rows
#' tm <- tm |>
#'   activate(rows) |>
#'   compute_umap(n_components = 2, random_state = 42)
#'
#' # Now row_data has row_umap_1, row_umap_2 columns
#'
#' # Get full umap object
#' umap_obj <- get_analysis(tm, "row_umap")
#' }
compute_umap <- function(x, name = NULL, n_components = 2, store = TRUE,
                         n_neighbors = 15, min_dist = 0.1,
                         metric = "euclidean", random_state = NULL, ...) {
  if (!inherits(x, "tidymatrix")) {
    stop("x must be a tidymatrix object", call. = FALSE)
  }

  if (x$active == "matrix") {
    stop(
      "Cannot compute UMAP when matrix is active.\n",
      "  Use activate(rows) or activate(columns) first.",
      call. = FALSE
    )
  }

  # Check that umap package is available
  if (!requireNamespace("umap", quietly = TRUE)) {
    stop(
      "Package 'umap' is required for UMAP.\n",
      "  Install it with: install.packages('umap')",
      call. = FALSE
    )
  }

  # Default name based on active component
  if (is.null(name)) {
    # Convert "rows" to "row" and "columns" to "column"
    active_singular <- if (x$active == "rows") "row" else "column"
    name <- paste0(active_singular, "_umap")
  }

  # Create configuration
  config <- umap::umap.defaults
  config$n_components <- n_components
  config$n_neighbors <- n_neighbors
  config$min_dist <- min_dist
  config$metric <- metric
  if (!is.null(random_state)) {
    config$random_state <- random_state
  }

  # Apply any additional config from ...
  extra_config <- list(...)
  for (param in names(extra_config)) {
    config[[param]] <- extra_config[[param]]
  }

  # Perform UMAP
  if (x$active == "rows") {
    umap_result <- umap::umap(x$matrix, config = config)
  } else {
    umap_result <- umap::umap(t(x$matrix), config = config)
  }

  # Extract coordinates
  umap_coords <- umap_result$layout

  # Create column names: {name}_1, {name}_2, etc.
  coord_colnames <- paste0(name, "_", 1:n_components)
  colnames(umap_coords) <- coord_colnames

  # Add to appropriate metadata
  if (x$active == "rows") {
    x$row_data <- cbind(x$row_data, as.data.frame(umap_coords))
  } else {
    x$col_data <- cbind(x$col_data, as.data.frame(umap_coords))
  }

  # Store full umap object if requested
  if (store) {
    x <- store_analysis(x, name, umap_result, x$active)
  }

  x
}
