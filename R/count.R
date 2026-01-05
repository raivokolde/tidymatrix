#' @importFrom dplyr count tally n
NULL

#' Count observations by group
#'
#' Count the number of observations in each group, aggregating the matrix.
#' Returns a tidymatrix (not a tibble).
#'
#' @param x A tidymatrix object
#' @param ... Variables to group by
#' @param wt Frequency weights (not yet implemented)
#' @param sort If TRUE, sort output in descending order of n
#' @param name Name of count column (default: "n")
#' @param .drop Drop groups with zero observations
#' @param .matrix_fn Function to aggregate matrix values. Default is \code{mean}
#'   for numeric matrices. Required for non-numeric matrices.
#' @param .matrix_args List of additional arguments to pass to \code{.matrix_fn}
#'
#' @return A tidymatrix object with counts and aggregated matrix
#' @export
#'
#' @examples
#' \dontrun{
#' mat <- matrix(rnorm(20), nrow = 10, ncol = 2)
#' row_data <- data.frame(
#'   id = 1:10,
#'   group = rep(c("A", "B"), each = 5),
#'   subgroup = rep(c("x", "y"), 5)
#' )
#' tm <- tidymatrix(mat, row_data)
#'
#' # Count by single variable
#' tm |>
#'   activate(rows) |>
#'   count(group)
#'
#' # Count by multiple variables
#' tm |>
#'   activate(rows) |>
#'   count(group, subgroup)
#'
#' # Use different aggregation for matrix
#' tm |>
#'   activate(rows) |>
#'   count(group, .matrix_fn = median)
#' }
count.tidymatrix <- function(x, ..., wt = NULL, sort = FALSE, name = NULL,
                             .drop = TRUE, .matrix_fn = NULL, .matrix_args = list()) {
  if (x$active == "matrix") {
    stop(
      "Cannot count when matrix is active.\n",
      "  Use activate(rows) or activate(columns) first.",
      call. = FALSE
    )
  }

  if (!missing(wt)) {
    stop("Weighted counts not yet implemented for tidymatrix", call. = FALSE)
  }

  # Determine name for count column
  if (is.null(name)) {
    name <- "n"
  }

  # Group by the specified variables
  x_grouped <- group_by(x, ..., .drop = .drop)

  # Create the count expression using rlang
  count_name <- rlang::sym(name)

  # Summarize with count
  result <- summarize(
    x_grouped,
    !!count_name := dplyr::n(),
    .matrix_fn = .matrix_fn,
    .matrix_args = .matrix_args
  )

  # Sort if requested
  if (sort) {
    if (x$active == "rows") {
      result$row_data <- dplyr::arrange(result$row_data, dplyr::desc(!!rlang::sym(name)))
    } else {
      result$col_data <- dplyr::arrange(result$col_data, dplyr::desc(!!rlang::sym(name)))
    }
  }

  result
}

#' Count observations in groups
#'
#' Count observations within existing groups. This is a wrapper around
#' \code{summarize(n = n())}.
#'
#' @param x A grouped_tidymatrix object
#' @param wt Frequency weights (not yet implemented)
#' @param sort If TRUE, sort output in descending order of n
#' @param name Name of count column (default: "n")
#' @param .matrix_fn Function to aggregate matrix values. Default is \code{mean}
#'   for numeric matrices. Required for non-numeric matrices.
#' @param .matrix_args List of additional arguments to pass to \code{.matrix_fn}
#'
#' @return A tidymatrix object with counts and aggregated matrix
#' @export
#'
#' @examples
#' \dontrun{
#' mat <- matrix(rnorm(20), nrow = 10, ncol = 2)
#' row_data <- data.frame(
#'   id = 1:10,
#'   group = rep(c("A", "B"), each = 5)
#' )
#' tm <- tidymatrix(mat, row_data)
#'
#' # Tally within groups
#' tm |>
#'   activate(rows) |>
#'   group_by(group) |>
#'   tally()
#' }
tally.grouped_tidymatrix <- function(x, wt = NULL, sort = FALSE, name = NULL,
                                     .matrix_fn = NULL, .matrix_args = list()) {
  if (!missing(wt)) {
    stop("Weighted tallies not yet implemented for tidymatrix", call. = FALSE)
  }

  # Determine name for count column
  if (is.null(name)) {
    name <- "n"
  }

  # Create the count expression using rlang
  count_name <- rlang::sym(name)

  # Summarize with count
  result <- summarize(
    x,
    !!count_name := dplyr::n(),
    .matrix_fn = .matrix_fn,
    .matrix_args = .matrix_args
  )

  # Sort if requested
  if (sort) {
    if (x$active == "rows") {
      result$row_data <- dplyr::arrange(result$row_data, dplyr::desc(!!rlang::sym(name)))
    } else {
      result$col_data <- dplyr::arrange(result$col_data, dplyr::desc(!!rlang::sym(name)))
    }
  }

  result
}
