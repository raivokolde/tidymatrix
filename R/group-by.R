#' @importFrom dplyr group_by ungroup groups group_vars
NULL

#' Group a tidymatrix by variables in metadata
#'
#' Create a grouped tidymatrix for use with \code{summarize()}. Groups are
#' created based on the active metadata component (row_data or col_data).
#'
#' @param .data A tidymatrix object
#' @param ... Variables to group by (unquoted names or expressions)
#' @param .add When FALSE (default), group_by() will override existing groups.
#'   When TRUE, add to existing groups.
#' @param .drop Drop groups with zero observations
#'
#' @return A grouped_tidymatrix object
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(20), nrow = 5, ncol = 4)
#' row_data <- data.frame(
#'   id = 1:5,
#'   group = c("A", "A", "B", "B", "C")
#' )
#' tm <- tidymatrix(mat, row_data)
#'
#' # Group by a variable
#' tm_grouped <- tm %>%
#'   activate(rows) %>%
#'   group_by(group)
#'
#' # Multiple grouping variables
#' row_data2 <- data.frame(
#'   id = 1:5,
#'   condition = c("ctrl", "ctrl", "treat", "treat", "treat"),
#'   batch = c(1, 2, 1, 2, 1)
#' )
#' tm2 <- tidymatrix(mat, row_data2)
#' tm_grouped2 <- tm2 %>%
#'   activate(rows) %>%
#'   group_by(condition, batch)
group_by.tidymatrix <- function(.data, ..., .add = FALSE, .drop = TRUE) {
  if (.data$active == "matrix") {
    stop(
      "Cannot group_by when matrix is active.\n",
      "  Use activate(rows) or activate(columns) first.",
      call. = FALSE
    )
  }

  # Group the appropriate metadata
  if (.data$active == "rows") {
    .data$row_data <- dplyr::group_by(.data$row_data, ..., .add = .add, .drop = .drop)
  } else if (.data$active == "columns") {
    .data$col_data <- dplyr::group_by(.data$col_data, ..., .add = .add, .drop = .drop)
  }

  # Add grouped_tidymatrix class
  class(.data) <- c("grouped_tidymatrix", class(.data))

  .data
}

#' Remove grouping from a grouped tidymatrix
#'
#' @param x A grouped_tidymatrix object
#' @param ... Not used
#'
#' @return A tidymatrix object (ungrouped)
#' @export
ungroup.grouped_tidymatrix <- function(x, ...) {
  # Ungroup the metadata
  if (dplyr::is_grouped_df(x$row_data)) {
    x$row_data <- dplyr::ungroup(x$row_data)
  }
  if (dplyr::is_grouped_df(x$col_data)) {
    x$col_data <- dplyr::ungroup(x$col_data)
  }

  # Remove grouped_tidymatrix class
  class(x) <- setdiff(class(x), "grouped_tidymatrix")

  x
}

#' Check if object is a grouped tidymatrix
#'
#' @param x An object to test
#' @return TRUE if the object is a grouped_tidymatrix, FALSE otherwise
#' @export
is_grouped_tidymatrix <- function(x) {
  inherits(x, "grouped_tidymatrix")
}

#' Get grouping variables
#'
#' @param x A grouped_tidymatrix object
#' @return A list of grouping variable names
#' @export
groups.grouped_tidymatrix <- function(x) {
  if (x$active == "rows" && dplyr::is_grouped_df(x$row_data)) {
    dplyr::groups(x$row_data)
  } else if (x$active == "columns" && dplyr::is_grouped_df(x$col_data)) {
    dplyr::groups(x$col_data)
  } else {
    NULL
  }
}

#' Get grouping variable names
#'
#' @param x A grouped_tidymatrix object
#' @return A character vector of grouping variable names
#' @export
group_vars.grouped_tidymatrix <- function(x) {
  if (x$active == "rows" && dplyr::is_grouped_df(x$row_data)) {
    dplyr::group_vars(x$row_data)
  } else if (x$active == "columns" && dplyr::is_grouped_df(x$col_data)) {
    dplyr::group_vars(x$col_data)
  } else {
    character(0)
  }
}

#' @export
group_vars.tidymatrix <- function(x) {
  character(0)
}

#' Print a grouped tidymatrix
#'
#' @param x A grouped_tidymatrix object
#' @param ... Additional arguments (currently unused)
#' @export
print.grouped_tidymatrix <- function(x, ...) {
  cat("# A grouped tidymatrix: ", nrow(x$matrix), " x ", ncol(x$matrix), " matrix\n", sep = "")

  # Show grouping information
  grp_vars <- group_vars(x)
  if (length(grp_vars) > 0) {
    cat("# Groups: ", paste(grp_vars, collapse = ", "), "\n", sep = "")

    # Count number of groups
    if (x$active == "rows") {
      n_groups <- dplyr::n_groups(x$row_data)
    } else {
      n_groups <- dplyr::n_groups(x$col_data)
    }
    cat("# Number of groups: ", n_groups, "\n", sep = "")
  }

  cat("# Active: ", x$active, "\n", sep = "")
  cat("#\n")
  cat("# Row data: ", nrow(x$row_data), " rows x ", ncol(x$row_data), " columns\n", sep = "")
  cat("# Column data: ", nrow(x$col_data), " rows x ", ncol(x$col_data), " columns\n", sep = "")
  cat("#\n")

  if (x$active == "rows") {
    cat("# Active data (rows):\n")
    print(utils::head(x$row_data))
  } else if (x$active == "columns") {
    cat("# Active data (columns):\n")
    print(utils::head(x$col_data))
  } else {
    cat("# Matrix preview:\n")
    print(utils::head(x$matrix))
  }

  invisible(x)
}
