#' Apply a function across matrix dimensions with metadata access
#'
#' Applies a user-defined function to each row (when rows are active) or each
#' column (when columns are active), providing both the values and the opposite
#' dimension's metadata. This enables complex statistical modeling where you
#' need access to all annotations.
#'
#' @param .data A tidymatrix object with rows or columns active (not matrix)
#' @param fn A function with signature `function(values, metadata, ...)` where:
#'   - `values`: Numeric vector of row/column values
#'   - `metadata`: Complete col_data (rows active) or row_data (columns active)
#'   - `...`: Additional arguments from `compute_across()`
#'   Must return a named list or named vector
#' @param add_to_data Logical. If TRUE, adds results to row_data/col_data and
#'   returns modified tidymatrix. If FALSE (default), returns data.frame
#' @param prefix Character. Optional prefix for result column names when
#'   `add_to_data = TRUE`
#' @param return_tibble Logical. If TRUE (default), returns tibble. If FALSE,
#'   returns data.frame. Only applies when `add_to_data = FALSE`
#' @param ... Additional arguments passed to `fn`
#'
#' @return If `add_to_data = FALSE`: data.frame/tibble with one row per
#'   matrix row/column, containing identifiers and computed statistics.
#'   If `add_to_data = TRUE`: modified tidymatrix with results added to metadata.
#'
#' @export
#'
#' @examples
#' # T-test example
#' mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' col_data <- data.frame(
#'   sample = paste0("S", 1:10),
#'   condition = rep(c("Control", "Treatment"), each = 5)
#' )
#' row_data <- data.frame(gene = paste0("Gene", 1:10))
#' tm <- tidymatrix(mat, row_data, col_data)
#'
#' # Run t-test on each row
#' results <- tm |>
#'   activate(rows) |>
#'   compute_across(
#'     fn = function(vals, meta) {
#'       test <- t.test(vals ~ meta$condition)
#'       list(
#'         p.value = test$p.value,
#'         log2fc = log2(mean(vals[meta$condition == "Treatment"]) /
#'                       mean(vals[meta$condition == "Control"]))
#'       )
#'     }
#'   )
#'
#' # Linear model with multiple predictors
#' col_data2 <- data.frame(
#'   sample = paste0("S", 1:10),
#'   condition = rep(c("Control", "Treatment"), each = 5),
#'   batch = factor(rep(1:2, 5)),
#'   age = rnorm(10, 50, 10)
#' )
#' tm2 <- tidymatrix(mat, row_data, col_data2)
#'
#' lm_results <- tm2 |>
#'   activate(rows) |>
#'   compute_across(
#'     fn = function(vals, meta) {
#'       fit <- lm(vals ~ condition + batch + age, data = meta)
#'       summ <- summary(fit)
#'       coef_summ <- coef(summ)
#'
#'       list(
#'         condition_pval = coef_summ["conditionTreatment", "Pr(>|t|)"],
#'         condition_coef = coef_summ["conditionTreatment", "Estimate"],
#'         r.squared = summ$r.squared
#'       )
#'     }
#'   )
#'
#' # Add results to metadata
#' tm_with_stats <- tm |>
#'   activate(rows) |>
#'   compute_across(
#'     fn = function(vals, meta) {
#'       test <- t.test(vals ~ meta$condition)
#'       list(p.value = test$p.value)
#'     },
#'     add_to_data = TRUE,
#'     prefix = "ttest"
#'   )
compute_across <- function(.data,
                          fn,
                          add_to_data = FALSE,
                          prefix = NULL,
                          return_tibble = TRUE,
                          ...) {
  # Validation
  if (!is_tidymatrix(.data)) {
    stop("compute_across() can only be used on tidymatrix objects", call. = FALSE)
  }

  if (.data$active == "matrix") {
    stop(
      "Cannot use compute_across() when matrix is active.\n",
      "  Use activate(rows) or activate(columns) first.",
      call. = FALSE
    )
  }

  if (!is.function(fn)) {
    stop("fn must be a function", call. = FALSE)
  }

  # Extract components based on active dimension
  if (.data$active == "rows") {
    n_iter <- nrow(.data$matrix)
    metadata <- .data$col_data
    id_data <- .data$row_data
  } else {  # columns active
    n_iter <- ncol(.data$matrix)
    metadata <- .data$row_data
    id_data <- .data$col_data
  }

  # Apply function to each row/column
  results_list <- vector("list", n_iter)

  for (i in seq_len(n_iter)) {
    # Extract values
    values <- if (.data$active == "rows") {
      .data$matrix[i, ]
    } else {
      .data$matrix[, i]
    }

    # Apply user function with error handling
    result <- tryCatch(
      fn(values, metadata, ...),
      error = function(e) {
        warning(
          "Error in row/column ", i, ": ", e$message,
          call. = FALSE
        )
        list(error = as.character(e$message))
      }
    )

    # Ensure result is a list
    if (!is.list(result)) {
      result <- as.list(result)
    }

    results_list[[i]] <- result
  }

  # Combine results into data.frame
  results_df <- dplyr::bind_rows(results_list)

  # Apply prefix if specified
  if (!is.null(prefix) && prefix != "") {
    names(results_df) <- paste0(prefix, "_", names(results_df))
  }

  # Return or add to metadata
  if (add_to_data) {
    # Add results to appropriate metadata
    if (.data$active == "rows") {
      .data$row_data <- dplyr::bind_cols(.data$row_data, results_df)
    } else {
      .data$col_data <- dplyr::bind_cols(.data$col_data, results_df)
    }
    return(.data)
  } else {
    # Combine with identifiers and return
    if (return_tibble) {
      output <- dplyr::bind_cols(tibble::as_tibble(id_data), results_df)
    } else {
      output <- dplyr::bind_cols(id_data, results_df)
    }
    return(output)
  }
}

#' Convenience wrapper for t-tests
#'
#' Performs t-tests comparing two groups for each row (or column) of a
#' tidymatrix. Automatically detects groups and applies multiple testing
#' correction.
#'
#' @param .data A tidymatrix object with rows or columns active
#' @param group_col Character. Name of column in metadata containing group labels
#' @param control Character. Label for control group. If NULL, uses first unique value
#' @param treatment Character. Label for treatment group. If NULL, uses second unique value
#' @param log2 Logical. If TRUE (default), computes log2 fold change. If FALSE,
#'   computes raw fold change
#' @param adjust Character. Method for p-value adjustment. Default "fdr".
#'   See `?p.adjust` for options. Use "none" for no adjustment
#' @param ... Additional arguments passed to `t.test()`
#'
#' @return A data.frame/tibble with columns: identifiers, p.value, log2fc (or fc),
#'   and p.adj (if adjustment applied)
#'
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' col_data <- data.frame(
#'   sample = paste0("S", 1:10),
#'   condition = rep(c("Control", "Treatment"), each = 5)
#' )
#' row_data <- data.frame(gene = paste0("Gene", 1:10))
#' tm <- tidymatrix(mat, row_data, col_data)
#'
#' # Simple t-test
#' results <- tm |>
#'   activate(rows) |>
#'   compute_ttest(group_col = "condition")
#'
#' # With specific group labels
#' results <- tm |>
#'   activate(rows) |>
#'   compute_ttest(
#'     group_col = "condition",
#'     control = "Control",
#'     treatment = "Treatment"
#'   )
compute_ttest <- function(.data,
                      group_col,
                      control = NULL,
                      treatment = NULL,
                      log2 = TRUE,
                      adjust = "fdr",
                      add_to_data = FALSE,
                      prefix = NULL,
                      return_tibble = TRUE,
                      ...) {
  # Get metadata for group detection
  if (.data$active == "rows") {
    meta <- .data$col_data
  } else {
    meta <- .data$row_data
  }

  # Auto-detect groups if not specified
  if (is.null(control) || is.null(treatment)) {
    groups <- unique(meta[[group_col]])
    if (length(groups) != 2) {
      stop(
        "Expected 2 groups in ", group_col, ", found ", length(groups),
        call. = FALSE
      )
    }
    control <- groups[1]
    treatment <- groups[2]
  }

  # Define test function
  test_fn <- function(vals, metadata) {
    g1 <- vals[metadata[[group_col]] == control]
    g2 <- vals[metadata[[group_col]] == treatment]

    test <- t.test(g1, g2, ...)

    fc_value <- if (log2) {
      log2(mean(g2) / mean(g1))
    } else {
      mean(g2) / mean(g1)
    }

    list(
      p.value = test$p.value,
      fc = fc_value
    )
  }

  # Apply test
  results <- compute_across(.data, test_fn,
                           add_to_data = add_to_data,
                           prefix = prefix,
                           return_tibble = return_tibble)

  # If adding to data, we need to handle the renaming and p-value adjustment differently
  if (add_to_data) {
    # Modify the tidymatrix metadata
    if (.data$active == "rows") {
      metadata <- results$row_data
    } else {
      metadata <- results$col_data
    }

    # Rename fold change column
    fc_col <- if (!is.null(prefix)) paste0(prefix, "_fc") else "fc"
    if (log2) {
      names(metadata)[names(metadata) == fc_col] <-
        if (!is.null(prefix)) paste0(prefix, "_log2fc") else "log2fc"
    }

    # Adjust p-values if requested
    if (!is.null(adjust) && adjust != "none") {
      pval_col <- if (!is.null(prefix)) paste0(prefix, "_p.value") else "p.value"
      padj_col <- if (!is.null(prefix)) paste0(prefix, "_p.adj") else "p.adj"
      metadata[[padj_col]] <- p.adjust(metadata[[pval_col]], method = adjust)
    }

    # Update the metadata in the tidymatrix
    if (.data$active == "rows") {
      results$row_data <- metadata
    } else {
      results$col_data <- metadata
    }

    return(results)
  } else {
    # Rename fold change column
    if (log2) {
      names(results)[names(results) == "fc"] <- "log2fc"
    }

    # Adjust p-values if requested
    if (!is.null(adjust) && adjust != "none") {
      results$p.adj <- p.adjust(results$p.value, method = adjust)
    }

    return(results)
  }
}

#' Convenience wrapper for linear models
#'
#' Fits linear models for each row (or column) of a tidymatrix. Can return
#' statistics for a specific coefficient or overall model fit statistics.
#'
#' @param .data A tidymatrix object with rows or columns active
#' @param formula_rhs Right-hand side of formula (e.g., `~ condition + batch + age`)
#' @param coef Character. Name of coefficient to extract. If NULL, returns overall
#'   model statistics
#' @param adjust Character. Method for p-value adjustment. Default "fdr".
#'   Use "none" for no adjustment
#' @param ... Additional arguments passed to `lm()`
#'
#' @return A data.frame/tibble with model statistics. If `coef` specified:
#'   estimate, p.value, se, and p.adj. If `coef = NULL`: r.squared, f.statistic,
#'   p.value, and p.adj
#'
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' col_data <- data.frame(
#'   sample = paste0("S", 1:10),
#'   condition = factor(rep(c("Control", "Treatment"), each = 5)),
#'   batch = factor(rep(1:2, 5)),
#'   age = rnorm(10, 50, 10)
#' )
#' row_data <- data.frame(gene = paste0("Gene", 1:10))
#' tm <- tidymatrix(mat, row_data, col_data)
#'
#' # Extract specific coefficient
#' results <- tm |>
#'   activate(rows) |>
#'   compute_lm(
#'     formula_rhs = ~ condition + batch + age,
#'     coef = "conditionTreatment"
#'   )
#'
#' # Overall model statistics
#' results <- tm |>
#'   activate(rows) |>
#'   compute_lm(formula_rhs = ~ condition + batch + age)
compute_lm <- function(.data,
                   formula_rhs,
                   coef = NULL,
                   adjust = "fdr",
                   add_to_data = FALSE,
                   prefix = NULL,
                   return_tibble = TRUE,
                   ...) {
  # Define model function
  model_fn <- function(vals, metadata) {
    fit <- lm(
      as.formula(paste("vals", deparse(formula_rhs))),
      data = metadata,
      ...
    )
    summ <- summary(fit)

    if (!is.null(coef)) {
      # Extract specific coefficient
      coef_summ <- coef(summ)
      if (!coef %in% rownames(coef_summ)) {
        stop("Coefficient '", coef, "' not found in model", call. = FALSE)
      }

      list(
        estimate = coef(fit)[coef],
        p.value = coef_summ[coef, "Pr(>|t|)"],
        se = coef_summ[coef, "Std. Error"]
      )
    } else {
      # Overall model stats
      fstat <- summ$fstatistic
      list(
        r.squared = summ$r.squared,
        f.statistic = fstat[1],
        p.value = pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
      )
    }
  }

  # Apply model
  results <- compute_across(.data, model_fn,
                           add_to_data = add_to_data,
                           prefix = prefix,
                           return_tibble = return_tibble)

  # Handle p-value adjustment
  if (add_to_data) {
    # Get the metadata
    if (.data$active == "rows") {
      metadata <- results$row_data
    } else {
      metadata <- results$col_data
    }

    # Adjust p-values if present and requested
    pval_col <- if (!is.null(prefix)) paste0(prefix, "_p.value") else "p.value"
    if (!is.null(adjust) && adjust != "none" && pval_col %in% names(metadata)) {
      padj_col <- if (!is.null(prefix)) paste0(prefix, "_p.adj") else "p.adj"
      metadata[[padj_col]] <- p.adjust(metadata[[pval_col]], method = adjust)
    }

    # Update the metadata in the tidymatrix
    if (.data$active == "rows") {
      results$row_data <- metadata
    } else {
      results$col_data <- metadata
    }

    return(results)
  } else {
    # Adjust p-values if present and requested
    if (!is.null(adjust) && adjust != "none" && "p.value" %in% names(results)) {
      results$p.adj <- p.adjust(results$p.value, method = adjust)
    }

    return(results)
  }
}

#' Convenience wrapper for Wilcoxon rank-sum test
#'
#' Performs Wilcoxon rank-sum tests (Mann-Whitney U test) comparing two groups
#' for each row (or column) of a tidymatrix. This is a non-parametric alternative
#' to the t-test that doesn't assume normal distribution.
#'
#' @param .data A tidymatrix object with rows or columns active
#' @param group_col Character. Name of column in metadata containing group labels
#' @param control Character. Label for control group. If NULL, uses first unique value
#' @param treatment Character. Label for treatment group. If NULL, uses second unique value
#' @param log2 Logical. If TRUE (default), computes log2 fold change. If FALSE,
#'   computes raw fold change
#' @param adjust Character. Method for p-value adjustment. Default "fdr".
#'   See `?p.adjust` for options. Use "none" for no adjustment
#' @param ... Additional arguments passed to `wilcox.test()`
#'
#' @return A data.frame/tibble with columns: identifiers, p.value, log2fc (or fc),
#'   median_diff, and p.adj (if adjustment applied)
#'
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' col_data <- data.frame(
#'   sample = paste0("S", 1:10),
#'   condition = rep(c("Control", "Treatment"), each = 5)
#' )
#' row_data <- data.frame(gene = paste0("Gene", 1:10))
#' tm <- tidymatrix(mat, row_data, col_data)
#'
#' # Wilcoxon test
#' results <- tm |>
#'   activate(rows) |>
#'   compute_wilcox(group_col = "condition")
compute_wilcox <- function(.data,
                        group_col,
                        control = NULL,
                        treatment = NULL,
                        log2 = TRUE,
                        adjust = "fdr",
                        add_to_data = FALSE,
                        prefix = NULL,
                        return_tibble = TRUE,
                        ...) {
  # Get metadata for group detection
  if (.data$active == "rows") {
    meta <- .data$col_data
  } else {
    meta <- .data$row_data
  }

  # Auto-detect groups if not specified
  if (is.null(control) || is.null(treatment)) {
    groups <- unique(meta[[group_col]])
    if (length(groups) != 2) {
      stop(
        "Expected 2 groups in ", group_col, ", found ", length(groups),
        call. = FALSE
      )
    }
    control <- groups[1]
    treatment <- groups[2]
  }

  # Define test function
  test_fn <- function(vals, metadata) {
    g1 <- vals[metadata[[group_col]] == control]
    g2 <- vals[metadata[[group_col]] == treatment]

    test <- suppressWarnings(wilcox.test(g1, g2, ...))

    fc_value <- if (log2) {
      log2(mean(g2) / mean(g1))
    } else {
      mean(g2) / mean(g1)
    }

    list(
      p.value = test$p.value,
      fc = fc_value,
      median_diff = median(g2) - median(g1)
    )
  }

  # Apply test
  results <- compute_across(.data, test_fn,
                           add_to_data = add_to_data,
                           prefix = prefix,
                           return_tibble = return_tibble)

  # Handle renaming and p-value adjustment
  if (add_to_data) {
    # Get the metadata
    if (.data$active == "rows") {
      metadata <- results$row_data
    } else {
      metadata <- results$col_data
    }

    # Rename fold change column
    fc_col <- if (!is.null(prefix)) paste0(prefix, "_fc") else "fc"
    if (log2) {
      names(metadata)[names(metadata) == fc_col] <-
        if (!is.null(prefix)) paste0(prefix, "_log2fc") else "log2fc"
    }

    # Adjust p-values if requested
    if (!is.null(adjust) && adjust != "none") {
      pval_col <- if (!is.null(prefix)) paste0(prefix, "_p.value") else "p.value"
      padj_col <- if (!is.null(prefix)) paste0(prefix, "_p.adj") else "p.adj"
      metadata[[padj_col]] <- p.adjust(metadata[[pval_col]], method = adjust)
    }

    # Update the metadata in the tidymatrix
    if (.data$active == "rows") {
      results$row_data <- metadata
    } else {
      results$col_data <- metadata
    }

    return(results)
  } else {
    # Rename fold change column
    if (log2) {
      names(results)[names(results) == "fc"] <- "log2fc"
    }

    # Adjust p-values if requested
    if (!is.null(adjust) && adjust != "none") {
      results$p.adj <- p.adjust(results$p.value, method = adjust)
    }

    return(results)
  }
}

#' Convenience wrapper for ANOVA
#'
#' Performs one-way ANOVA for each row (or column) of a tidymatrix to test
#' for differences across multiple groups.
#'
#' @param .data A tidymatrix object with rows or columns active
#' @param group_col Character. Name of column in metadata containing group labels
#' @param adjust Character. Method for p-value adjustment. Default "fdr".
#'   Use "none" for no adjustment
#'
#' @return A data.frame/tibble with columns: identifiers, f.statistic, p.value,
#'   df_between, df_within, and p.adj (if adjustment applied)
#'
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(150), nrow = 10, ncol = 15)
#' col_data <- data.frame(
#'   sample = paste0("S", 1:15),
#'   condition = rep(c("A", "B", "C"), each = 5)
#' )
#' row_data <- data.frame(gene = paste0("Gene", 1:10))
#' tm <- tidymatrix(mat, row_data, col_data)
#'
#' # ANOVA
#' results <- tm |>
#'   activate(rows) |>
#'   compute_anova(group_col = "condition")
compute_anova <- function(.data,
                       group_col,
                       adjust = "fdr",
                       add_to_data = FALSE,
                       prefix = NULL,
                       return_tibble = TRUE) {
  # Define ANOVA function
  anova_fn <- function(vals, metadata) {
    fit <- aov(vals ~ metadata[[group_col]])
    summ <- summary(fit)[[1]]

    list(
      f.statistic = summ["metadata[[group_col]]", "F value"],
      p.value = summ["metadata[[group_col]]", "Pr(>F)"],
      df_between = summ["metadata[[group_col]]", "Df"],
      df_within = summ["Residuals", "Df"]
    )
  }

  # Apply ANOVA
  results <- compute_across(.data, anova_fn,
                           add_to_data = add_to_data,
                           prefix = prefix,
                           return_tibble = return_tibble)

  # Handle p-value adjustment
  if (add_to_data) {
    # Get the metadata
    if (.data$active == "rows") {
      metadata <- results$row_data
    } else {
      metadata <- results$col_data
    }

    # Adjust p-values if requested
    if (!is.null(adjust) && adjust != "none") {
      pval_col <- if (!is.null(prefix)) paste0(prefix, "_p.value") else "p.value"
      padj_col <- if (!is.null(prefix)) paste0(prefix, "_p.adj") else "p.adj"
      metadata[[padj_col]] <- p.adjust(metadata[[pval_col]], method = adjust)
    }

    # Update the metadata in the tidymatrix
    if (.data$active == "rows") {
      results$row_data <- metadata
    } else {
      results$col_data <- metadata
    }

    return(results)
  } else {
    # Adjust p-values if requested
    if (!is.null(adjust) && adjust != "none") {
      results$p.adj <- p.adjust(results$p.value, method = adjust)
    }

    return(results)
  }
}

#' Convenience wrapper for Kruskal-Wallis test
#'
#' Performs Kruskal-Wallis tests for each row (or column) of a tidymatrix.
#' This is a non-parametric alternative to one-way ANOVA.
#'
#' @param .data A tidymatrix object with rows or columns active
#' @param group_col Character. Name of column in metadata containing group labels
#' @param adjust Character. Method for p-value adjustment. Default "fdr".
#'   Use "none" for no adjustment
#'
#' @return A data.frame/tibble with columns: identifiers, statistic, p.value,
#'   df, and p.adj (if adjustment applied)
#'
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(150), nrow = 10, ncol = 15)
#' col_data <- data.frame(
#'   sample = paste0("S", 1:15),
#'   condition = rep(c("A", "B", "C"), each = 5)
#' )
#' row_data <- data.frame(gene = paste0("Gene", 1:10))
#' tm <- tidymatrix(mat, row_data, col_data)
#'
#' # Kruskal-Wallis test
#' results <- tm |>
#'   activate(rows) |>
#'   compute_kruskal(group_col = "condition")
compute_kruskal <- function(.data,
                         group_col,
                         adjust = "fdr",
                         add_to_data = FALSE,
                         prefix = NULL,
                         return_tibble = TRUE) {
  # Define Kruskal-Wallis function
  kruskal_fn <- function(vals, metadata) {
    test <- kruskal.test(vals ~ metadata[[group_col]])

    list(
      statistic = test$statistic,
      p.value = test$p.value,
      df = test$parameter
    )
  }

  # Apply test
  results <- compute_across(.data, kruskal_fn,
                           add_to_data = add_to_data,
                           prefix = prefix,
                           return_tibble = return_tibble)

  # Handle p-value adjustment
  if (add_to_data) {
    # Get the metadata
    if (.data$active == "rows") {
      metadata <- results$row_data
    } else {
      metadata <- results$col_data
    }

    # Adjust p-values if requested
    if (!is.null(adjust) && adjust != "none") {
      pval_col <- if (!is.null(prefix)) paste0(prefix, "_p.value") else "p.value"
      padj_col <- if (!is.null(prefix)) paste0(prefix, "_p.adj") else "p.adj"
      metadata[[padj_col]] <- p.adjust(metadata[[pval_col]], method = adjust)
    }

    # Update the metadata in the tidymatrix
    if (.data$active == "rows") {
      results$row_data <- metadata
    } else {
      results$col_data <- metadata
    }

    return(results)
  } else {
    # Adjust p-values if requested
    if (!is.null(adjust) && adjust != "none") {
      results$p.adj <- p.adjust(results$p.value, method = adjust)
    }

    return(results)
  }
}

#' Convenience wrapper for correlation tests
#'
#' Performs correlation tests between each row (or column) and a continuous
#' variable in the metadata.
#'
#' @param .data A tidymatrix object with rows or columns active
#' @param var Character. Name of continuous variable in metadata to correlate with
#' @param method Character. Correlation method: "pearson" (default), "spearman",
#'   or "kendall"
#' @param adjust Character. Method for p-value adjustment. Default "fdr".
#'   Use "none" for no adjustment
#'
#' @return A data.frame/tibble with columns: identifiers, correlation, p.value,
#'   and p.adj (if adjustment applied)
#'
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' col_data <- data.frame(
#'   sample = paste0("S", 1:10),
#'   age = rnorm(10, 50, 10)
#' )
#' row_data <- data.frame(gene = paste0("Gene", 1:10))
#' tm <- tidymatrix(mat, row_data, col_data)
#'
#' # Pearson correlation
#' results <- tm |>
#'   activate(rows) |>
#'   compute_correlation(var = "age", method = "pearson")
#'
#' # Spearman correlation
#' results <- tm |>
#'   activate(rows) |>
#'   compute_correlation(var = "age", method = "spearman")
compute_correlation <- function(.data,
                             var,
                             method = "pearson",
                             adjust = "fdr",
                             add_to_data = FALSE,
                             prefix = NULL,
                             return_tibble = TRUE) {
  # Define correlation function
  cor_fn <- function(vals, metadata) {
    test <- cor.test(vals, metadata[[var]], method = method)

    list(
      correlation = as.numeric(test$estimate),
      p.value = test$p.value
    )
  }

  # Apply correlation test
  results <- compute_across(.data, cor_fn,
                           add_to_data = add_to_data,
                           prefix = prefix,
                           return_tibble = return_tibble)

  # Handle p-value adjustment
  if (add_to_data) {
    # Get the metadata
    if (.data$active == "rows") {
      metadata <- results$row_data
    } else {
      metadata <- results$col_data
    }

    # Adjust p-values if requested
    if (!is.null(adjust) && adjust != "none") {
      pval_col <- if (!is.null(prefix)) paste0(prefix, "_p.value") else "p.value"
      padj_col <- if (!is.null(prefix)) paste0(prefix, "_p.adj") else "p.adj"
      metadata[[padj_col]] <- p.adjust(metadata[[pval_col]], method = adjust)
    }

    # Update the metadata in the tidymatrix
    if (.data$active == "rows") {
      results$row_data <- metadata
    } else {
      results$col_data <- metadata
    }

    return(results)
  } else {
    # Adjust p-values if requested
    if (!is.null(adjust) && adjust != "none") {
      results$p.adj <- p.adjust(results$p.value, method = adjust)
    }

    return(results)
  }
}

#' Convenience wrapper for simple linear regression
#'
#' Performs simple linear regression (one predictor) for each row (or column)
#' of a tidymatrix. For multiple predictors, use `compute_lm()` instead.
#'
#' @param .data A tidymatrix object with rows or columns active
#' @param predictor Character. Name of predictor variable in metadata
#' @param adjust Character. Method for p-value adjustment. Default "fdr".
#'   Use "none" for no adjustment
#'
#' @return A data.frame/tibble with columns: identifiers, slope, intercept,
#'   r.squared, p.value, and p.adj (if adjustment applied)
#'
#' @export
#'
#' @examples
#' mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' col_data <- data.frame(
#'   sample = paste0("S", 1:10),
#'   age = rnorm(10, 50, 10)
#' )
#' row_data <- data.frame(gene = paste0("Gene", 1:10))
#' tm <- tidymatrix(mat, row_data, col_data)
#'
#' # Simple linear regression
#' results <- tm |>
#'   activate(rows) |>
#'   compute_lm_simple(predictor = "age")
compute_lm_simple <- function(.data,
                           predictor,
                           adjust = "fdr",
                           add_to_data = FALSE,
                           prefix = NULL,
                           return_tibble = TRUE) {
  # Define regression function
  lm_fn <- function(vals, metadata) {
    fit <- lm(vals ~ metadata[[predictor]])
    summ <- summary(fit)
    coef_summ <- coef(summ)

    list(
      slope = coef(fit)[2],
      intercept = coef(fit)[1],
      r.squared = summ$r.squared,
      p.value = coef_summ[2, "Pr(>|t|)"],
      se = coef_summ[2, "Std. Error"]
    )
  }

  # Apply regression
  results <- compute_across(.data, lm_fn,
                           add_to_data = add_to_data,
                           prefix = prefix,
                           return_tibble = return_tibble)

  # Handle p-value adjustment
  if (add_to_data) {
    # Get the metadata
    if (.data$active == "rows") {
      metadata <- results$row_data
    } else {
      metadata <- results$col_data
    }

    # Adjust p-values if requested
    if (!is.null(adjust) && adjust != "none") {
      pval_col <- if (!is.null(prefix)) paste0(prefix, "_p.value") else "p.value"
      padj_col <- if (!is.null(prefix)) paste0(prefix, "_p.adj") else "p.adj"
      metadata[[padj_col]] <- p.adjust(metadata[[pval_col]], method = adjust)
    }

    # Update the metadata in the tidymatrix
    if (.data$active == "rows") {
      results$row_data <- metadata
    } else {
      results$col_data <- metadata
    }

    return(results)
  } else {
    # Adjust p-values if requested
    if (!is.null(adjust) && adjust != "none") {
      results$p.adj <- p.adjust(results$p.value, method = adjust)
    }

    return(results)
  }
}
