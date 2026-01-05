#' Analysis Management for tidymatrix
#'
#' Functions to store, retrieve, and manage analysis results attached to
#' tidymatrix objects.
#'
#' @name analysis-management
NULL

#' Get stored analysis object
#'
#' Retrieve a full analysis object (prcomp, hclust, etc.) that was stored
#' using a compute_* function.
#'
#' @param x A tidymatrix object
#' @param name Name of the analysis to retrieve
#'
#' @return The stored analysis object (class depends on analysis type)
#' @export
#'
#' @examples
#' \dontrun{
#' tm <- tidymatrix(matrix(rnorm(100), 10, 10))
#' tm <- tm |>
#'   activate(columns) |>
#'   compute_prcomp(name = "pca")
#'
#' # Get the full prcomp object
#' pca_obj <- get_analysis(tm, "pca")
#' summary(pca_obj)
#' plot(pca_obj$sdev)  # Scree plot
#' }
get_analysis <- function(x, name) {
  if (!inherits(x, "tidymatrix")) {
    stop("x must be a tidymatrix object", call. = FALSE)
  }

  analyses <- attr(x, "analyses")
  if (is.null(analyses) || !name %in% names(analyses)) {
    stop(
      "No analysis named '", name, "' found.\\n",
      "  Available analyses: ",
      if (is.null(analyses) || length(analyses) == 0) {
        "none"
      } else {
        paste(names(analyses), collapse = ", ")
      },
      call. = FALSE
    )
  }

  analyses[[name]]$object
}

#' List stored analyses
#'
#' Get the names of all analyses stored in a tidymatrix object.
#'
#' @param x A tidymatrix object
#'
#' @return Character vector of analysis names
#' @export
#'
#' @examples
#' \dontrun{
#' tm <- tidymatrix(matrix(rnorm(100), 10, 10))
#' tm <- tm |>
#'   activate(columns) |>
#'   compute_prcomp(name = "pca") |>
#'   compute_hclust(k = 3, name = "clusters")
#'
#' list_analyses(tm)
#' # [1] "pca" "clusters"
#' }
list_analyses <- function(x) {
  if (!inherits(x, "tidymatrix")) {
    stop("x must be a tidymatrix object", call. = FALSE)
  }

  analyses <- attr(x, "analyses")
  if (is.null(analyses) || length(analyses) == 0) {
    return(character(0))
  }

  names(analyses)
}

#' Remove stored analysis
#'
#' Remove a stored analysis object from a tidymatrix. This removes the full
#' analysis object but keeps any metadata columns that were added.
#'
#' @param x A tidymatrix object
#' @param name Name of the analysis to remove. If NULL, removes all analyses.
#'
#' @return A tidymatrix object with the analysis removed
#' @export
#'
#' @examples
#' \dontrun{
#' tm <- tidymatrix(matrix(rnorm(100), 10, 10))
#' tm <- tm |>
#'   activate(columns) |>
#'   compute_prcomp(name = "pca")
#'
#' # Remove specific analysis
#' tm <- remove_analysis(tm, "pca")
#'
#' # Remove all analyses
#' tm <- remove_analysis(tm, NULL)
#' }
remove_analysis <- function(x, name = NULL) {
  if (!inherits(x, "tidymatrix")) {
    stop("x must be a tidymatrix object", call. = FALSE)
  }

  analyses <- attr(x, "analyses")

  if (is.null(name)) {
    # Remove all analyses
    attr(x, "analyses") <- list()
    return(x)
  }

  if (is.null(analyses) || !name %in% names(analyses)) {
    warning(
      "No analysis named '", name, "' found. Nothing removed.",
      call. = FALSE
    )
    return(x)
  }

  # Remove the specific analysis
  analyses[[name]] <- NULL
  attr(x, "analyses") <- analyses

  x
}

#' Check analysis validity
#'
#' Check if stored analyses are still valid given the current data dimensions.
#'
#' @param x A tidymatrix object
#'
#' @return Invisibly returns x. Prints status of all analyses.
#' @export
#'
#' @examples
#' \dontrun{
#' tm <- tidymatrix(matrix(rnorm(100), 10, 10))
#' tm <- tm |>
#'   activate(columns) |>
#'   compute_prcomp(name = "pca")
#'
#' check_analyses(tm)
#' # Analysis 'pca': VALID (10 rows x 10 columns)
#'
#' tm <- tm |> activate(rows) |> filter(row_number() <= 5)
#' check_analyses(tm)
#' # Analysis 'pca': dimensions changed (was 10 rows, now 5 rows)
#' }
check_analyses <- function(x) {
  if (!inherits(x, "tidymatrix")) {
    stop("x must be a tidymatrix object", call. = FALSE)
  }

  analyses <- attr(x, "analyses")
  if (is.null(analyses) || length(analyses) == 0) {
    cat("No stored analyses\\n")
    return(invisible(x))
  }

  current_nrow <- nrow(x$matrix)
  current_ncol <- ncol(x$matrix)

  for (name in names(analyses)) {
    analysis <- analyses[[name]]
    original_nrow <- analysis$metadata$n_rows
    original_ncol <- analysis$metadata$n_cols

    if (original_nrow == current_nrow && original_ncol == current_ncol) {
      cat("Analysis '", name, "': VALID (", current_nrow, " rows x ",
          current_ncol, " columns)\\n", sep = "")
    } else {
      cat("Analysis '", name, "': dimensions changed ", sep = "")
      if (original_nrow != current_nrow) {
        cat("(was ", original_nrow, " rows, now ", current_nrow, " rows) ", sep = "")
      }
      if (original_ncol != current_ncol) {
        cat("(was ", original_ncol, " columns, now ", current_ncol, " columns)", sep = "")
      }
      cat("\\n")
    }
  }

  invisible(x)
}

#' Store an analysis object
#'
#' Internal function to store analysis results as an attribute.
#'
#' @param x A tidymatrix object
#' @param name Name for the analysis
#' @param object The analysis object to store
#' @param active Which component was active ("rows" or "columns")
#'
#' @return A tidymatrix object with the analysis stored
#' @keywords internal
store_analysis <- function(x, name, object, active) {
  analyses <- attr(x, "analyses")
  if (is.null(analyses)) {
    analyses <- list()
  }

  analyses[[name]] <- list(
    object = object,
    metadata = list(
      active = active,
      n_rows = nrow(x$matrix),
      n_cols = ncol(x$matrix),
      computed_on = Sys.time()
    )
  )

  attr(x, "analyses") <- analyses
  x
}

#' Remove all stored analyses
#'
#' Internal helper to remove all analyses when data is modified.
#' Used by filter, slice, and other data modification operations.
#'
#' @param x A tidymatrix object
#' @param operation Name of the operation causing removal (for warning message)
#'
#' @return A tidymatrix object with analyses removed
#' @keywords internal
remove_all_analyses <- function(x, operation = "data modification") {
  analyses <- attr(x, "analyses")

  if (!is.null(analyses) && length(analyses) > 0) {
    analysis_names <- names(analyses)
    attr(x, "analyses") <- list()

    warning(
      "Removed ", length(analysis_names), " stored analysis object(s) due to ", operation, ": ",
      paste(analysis_names, collapse = ", "), "\\n",
      "Metadata columns are preserved.",
      call. = FALSE
    )
  }

  x
}
