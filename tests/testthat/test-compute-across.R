test_that("compute_across works with simple function on rows", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4)
  col_data <- data.frame(sample = c("A", "B", "C"))
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_across(fn = function(vals, meta) {
      list(mean = mean(vals), sd = sd(vals))
    })

  expect_equal(nrow(result), 4)
  expect_true("mean" %in% names(result))
  expect_true("sd" %in% names(result))
  expect_equal(result$mean[1], mean(c(1, 5, 9)))
})

test_that("compute_across works with simple function on columns", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4)
  col_data <- data.frame(sample = c("A", "B", "C"))
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(columns) |>
    compute_across(fn = function(vals, meta) {
      list(mean = mean(vals), sd = sd(vals))
    })

  expect_equal(nrow(result), 3)
  expect_true("mean" %in% names(result))
  expect_true("sd" %in% names(result))
  expect_equal(result$mean[1], mean(c(1, 2, 3, 4)))
})

test_that("compute_across provides correct metadata to function", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4)
  col_data <- data.frame(sample = c("A", "B", "C"), type = c("x", "y", "z"))
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_across(fn = function(vals, meta) {
      list(
        n_samples = nrow(meta),
        has_type_col = "type" %in% names(meta)
      )
    })

  expect_equal(result$n_samples[1], 3)
  expect_true(result$has_type_col[1])
})

test_that("compute_across fails when matrix is active", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat) |> activate(matrix)

  expect_error(
    compute_across(tm, fn = function(vals, meta) list(mean = mean(vals))),
    "Cannot use compute_across\\(\\) when matrix is active"
  )
})

test_that("compute_across fails on non-tidymatrix", {
  expect_error(
    compute_across(data.frame(x = 1:3), fn = function(vals, meta) list(mean = mean(vals))),
    "can only be used on tidymatrix objects"
  )
})

test_that("compute_across with add_to_data = TRUE modifies tidymatrix", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4)
  col_data <- data.frame(sample = c("A", "B", "C"))
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_across(
      fn = function(vals, meta) list(mean = mean(vals)),
      add_to_data = TRUE
    )

  expect_s3_class(result, "tidymatrix")
  expect_true("mean" %in% names(result$row_data))
  expect_equal(result$row_data$mean[1], mean(c(1, 5, 9)))
})

test_that("compute_across with prefix adds prefixed columns", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4)
  col_data <- data.frame(sample = c("A", "B", "C"))
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_across(
      fn = function(vals, meta) list(mean = mean(vals)),
      add_to_data = TRUE,
      prefix = "stat"
    )

  expect_true("stat_mean" %in% names(result$row_data))
  expect_false("mean" %in% names(result$row_data))
})

test_that("compute_across with prefix in return mode", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4)
  col_data <- data.frame(sample = c("A", "B", "C"))
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_across(
      fn = function(vals, meta) list(mean = mean(vals)),
      prefix = "stat"
    )

  expect_true("stat_mean" %in% names(result))
  expect_false("mean" %in% names(result))
})

test_that("compute_across returns tibble by default", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4)
  col_data <- data.frame(sample = c("A", "B", "C"))
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_across(fn = function(vals, meta) list(mean = mean(vals)))

  expect_s3_class(result, "tbl_df")
})

test_that("compute_across returns data.frame when return_tibble = FALSE", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4)
  col_data <- data.frame(sample = c("A", "B", "C"))
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_across(
      fn = function(vals, meta) list(mean = mean(vals)),
      return_tibble = FALSE
    )

  expect_true(is.data.frame(result))
  expect_false(inherits(result, "tbl_df"))
})

test_that("compute_across handles errors gracefully", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4)
  col_data <- data.frame(sample = c("A", "B", "C"))
  tm <- tidymatrix(mat, row_data, col_data)

  expect_warning(
    result <- tm |>
      activate(rows) |>
      compute_across(fn = function(vals, meta) {
        if (vals[1] == 1) stop("Test error")
        list(mean = mean(vals))
      }),
    "Error in row/column 1"
  )

  expect_true("error" %in% names(result))
  expect_equal(result$error[1], "Test error")
})

test_that("compute_across passes additional arguments to fn", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4)
  col_data <- data.frame(sample = c("A", "B", "C"))
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_across(
      fn = function(vals, meta, multiplier) {
        list(scaled_mean = mean(vals) * multiplier)
      },
      multiplier = 10
    )

  expect_equal(result$scaled_mean[1], mean(c(1, 5, 9)) * 10)
})

test_that("compute_across works with t.test", {
  mat <- matrix(rnorm(60), nrow = 6, ncol = 10)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:10),
    condition = rep(c("Control", "Treatment"), each = 5)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_across(fn = function(vals, meta) {
      test <- t.test(vals ~ meta$condition)
      list(p.value = test$p.value)
    })

  expect_equal(nrow(result), 6)
  expect_true("p.value" %in% names(result))
  expect_true(all(result$p.value >= 0 & result$p.value <= 1))
})

test_that("compute_across works with lm", {
  mat <- matrix(rnorm(60), nrow = 6, ncol = 10)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:10),
    condition = factor(rep(c("Control", "Treatment"), each = 5)),
    batch = factor(rep(1:2, 5))
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_across(fn = function(vals, meta) {
      fit <- lm(vals ~ condition + batch, data = meta)
      summ <- summary(fit)
      list(r.squared = summ$r.squared)
    })

  expect_equal(nrow(result), 6)
  expect_true("r.squared" %in% names(result))
})

test_that("compute_ttest works with auto-detected groups", {
  mat <- matrix(rnorm(60), nrow = 6, ncol = 10)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:10),
    condition = rep(c("Control", "Treatment"), each = 5)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_ttest(group_col = "condition")

  expect_equal(nrow(result), 6)
  expect_true("p.value" %in% names(result))
  expect_true("log2fc" %in% names(result))
  expect_true("p.adj" %in% names(result))
})

test_that("compute_ttest works with specified groups", {
  mat <- matrix(rnorm(60), nrow = 6, ncol = 10)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:10),
    condition = rep(c("A", "B"), each = 5)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_ttest(group_col = "condition", control = "A", treatment = "B")

  expect_equal(nrow(result), 6)
  expect_true("p.value" %in% names(result))
  expect_true("log2fc" %in% names(result))
})

test_that("compute_ttest with log2 = FALSE returns raw fold change", {
  mat <- matrix(rnorm(60), nrow = 6, ncol = 10)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:10),
    condition = rep(c("Control", "Treatment"), each = 5)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_ttest(group_col = "condition", log2 = FALSE)

  expect_true("fc" %in% names(result))
  expect_false("log2fc" %in% names(result))
})

test_that("compute_ttest with adjust = 'none' omits p.adj", {
  mat <- matrix(rnorm(60), nrow = 6, ncol = 10)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:10),
    condition = rep(c("Control", "Treatment"), each = 5)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_ttest(group_col = "condition", adjust = "none")

  expect_false("p.adj" %in% names(result))
})

test_that("compute_ttest fails with non-binary groups", {
  mat <- matrix(rnorm(54), nrow = 6, ncol = 9)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:9),
    condition = rep(c("A", "B", "C"), each = 3)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  expect_error(
    tm |> activate(rows) |> compute_ttest(group_col = "condition"),
    "Expected 2 groups"
  )
})

test_that("compute_lm works with specific coefficient", {
  mat <- matrix(rnorm(60), nrow = 6, ncol = 10)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:10),
    condition = factor(rep(c("Control", "Treatment"), each = 5)),
    batch = factor(rep(1:2, 5))
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_lm(formula_rhs = ~ condition + batch, coef = "conditionTreatment")

  expect_equal(nrow(result), 6)
  expect_true("estimate" %in% names(result))
  expect_true("p.value" %in% names(result))
  expect_true("se" %in% names(result))
  expect_true("p.adj" %in% names(result))
})

test_that("compute_lm works with overall model statistics", {
  mat <- matrix(rnorm(60), nrow = 6, ncol = 10)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:10),
    condition = factor(rep(c("Control", "Treatment"), each = 5)),
    batch = factor(rep(1:2, 5))
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_lm(formula_rhs = ~ condition + batch)

  expect_equal(nrow(result), 6)
  expect_true("r.squared" %in% names(result))
  expect_true("f.statistic" %in% names(result))
  expect_true("p.value" %in% names(result))
})

test_that("compute_lm with adjust = 'none' omits p.adj", {
  mat <- matrix(rnorm(60), nrow = 6, ncol = 10)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:10),
    condition = factor(rep(c("Control", "Treatment"), each = 5))
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_lm(formula_rhs = ~ condition, coef = "conditionTreatment", adjust = "none")

  expect_false("p.adj" %in% names(result))
})

test_that("compute_across works with correlation test", {
  mat <- matrix(rnorm(60), nrow = 6, ncol = 10)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:10),
    phenotype_score = rnorm(10)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_across(fn = function(vals, meta) {
      ct <- cor.test(vals, meta$phenotype_score)
      list(
        correlation = ct$estimate,
        p.value = ct$p.value
      )
    })

  expect_equal(nrow(result), 6)
  expect_true("correlation" %in% names(result))
  expect_true("p.value" %in% names(result))
})

test_that("compute_across preserves row identifiers", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = c(10, 20, 30, 40), name = c("A", "B", "C", "D"))
  col_data <- data.frame(sample = c("X", "Y", "Z"))
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_across(fn = function(vals, meta) list(mean = mean(vals)))

  expect_equal(result$id, c(10, 20, 30, 40))
  expect_equal(result$name, c("A", "B", "C", "D"))
})

test_that("compute_across works after filter", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4, group = c("A", "A", "B", "B"))
  col_data <- data.frame(sample = c("X", "Y", "Z"))
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    filter(group == "A") |>
    compute_across(fn = function(vals, meta) list(mean = mean(vals)))

  expect_equal(nrow(result), 2)
  expect_equal(result$group, c("A", "A"))
})

test_that("compute_across on columns works correctly", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4, value = c(1, 2, 3, 4))
  col_data <- data.frame(sample = c("X", "Y", "Z"))
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(columns) |>
    compute_across(fn = function(vals, meta) {
      # meta should be row_data
      # vals should be column values
      list(
        n_rows = nrow(meta),
        col_sum = sum(vals)
      )
    })

  expect_equal(nrow(result), 3)
  expect_equal(result$n_rows, c(4, 4, 4))
  expect_equal(result$col_sum, c(10, 26, 42))  # sum of each column
})

# =============================================================================
# Tests for new convenience functions
# =============================================================================

test_that("compute_wilcox works with auto-detected groups", {
  mat <- matrix(rnorm(60), nrow = 6, ncol = 10)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:10),
    condition = rep(c("Control", "Treatment"), each = 5)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_wilcox(group_col = "condition")

  expect_equal(nrow(result), 6)
  expect_true("p.value" %in% names(result))
  expect_true("log2fc" %in% names(result))
  expect_true("median_diff" %in% names(result))
  expect_true("p.adj" %in% names(result))
})

test_that("compute_wilcox with log2 = FALSE returns raw fold change", {
  mat <- matrix(rnorm(60), nrow = 6, ncol = 10)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:10),
    condition = rep(c("Control", "Treatment"), each = 5)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_wilcox(group_col = "condition", log2 = FALSE)

  expect_true("fc" %in% names(result))
  expect_false("log2fc" %in% names(result))
})

test_that("compute_wilcox fails with non-binary groups", {
  mat <- matrix(rnorm(54), nrow = 6, ncol = 9)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:9),
    condition = rep(c("A", "B", "C"), each = 3)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  expect_error(
    tm |> activate(rows) |> compute_wilcox(group_col = "condition"),
    "Expected 2 groups"
  )
})

test_that("compute_anova works with multiple groups", {
  mat <- matrix(rnorm(90), nrow = 6, ncol = 15)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:15),
    condition = rep(c("A", "B", "C"), each = 5)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_anova(group_col = "condition")

  expect_equal(nrow(result), 6)
  expect_true("f.statistic" %in% names(result))
  expect_true("p.value" %in% names(result))
  expect_true("df_between" %in% names(result))
  expect_true("df_within" %in% names(result))
  expect_true("p.adj" %in% names(result))
  expect_equal(result$df_between[1], 2)  # 3 groups - 1
  expect_equal(result$df_within[1], 12)  # 15 samples - 3 groups
})

test_that("compute_anova with adjust = 'none' omits p.adj", {
  mat <- matrix(rnorm(90), nrow = 6, ncol = 15)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:15),
    condition = rep(c("A", "B", "C"), each = 5)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_anova(group_col = "condition", adjust = "none")

  expect_false("p.adj" %in% names(result))
})

test_that("compute_kruskal works with multiple groups", {
  mat <- matrix(rnorm(90), nrow = 6, ncol = 15)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:15),
    condition = rep(c("A", "B", "C"), each = 5)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_kruskal(group_col = "condition")

  expect_equal(nrow(result), 6)
  expect_true("statistic" %in% names(result))
  expect_true("p.value" %in% names(result))
  expect_true("df" %in% names(result))
  expect_true("p.adj" %in% names(result))
  expect_equal(as.numeric(result$df[1]), 2)  # 3 groups - 1
})

test_that("compute_kruskal with adjust = 'none' omits p.adj", {
  mat <- matrix(rnorm(90), nrow = 6, ncol = 15)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:15),
    condition = rep(c("A", "B", "C"), each = 5)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_kruskal(group_col = "condition", adjust = "none")

  expect_false("p.adj" %in% names(result))
})

test_that("compute_correlation works with Pearson", {
  mat <- matrix(rnorm(60), nrow = 6, ncol = 10)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:10),
    age = rnorm(10, 50, 10)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_correlation(var = "age", method = "pearson")

  expect_equal(nrow(result), 6)
  expect_true("correlation" %in% names(result))
  expect_true("p.value" %in% names(result))
  expect_true("p.adj" %in% names(result))
  expect_true(all(result$correlation >= -1 & result$correlation <= 1))
})

test_that("compute_correlation works with Spearman", {
  mat <- matrix(rnorm(60), nrow = 6, ncol = 10)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:10),
    age = rnorm(10, 50, 10)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_correlation(var = "age", method = "spearman")

  expect_equal(nrow(result), 6)
  expect_true("correlation" %in% names(result))
  expect_true("p.value" %in% names(result))
})

test_that("compute_correlation with adjust = 'none' omits p.adj", {
  mat <- matrix(rnorm(60), nrow = 6, ncol = 10)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:10),
    age = rnorm(10, 50, 10)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_correlation(var = "age", adjust = "none")

  expect_false("p.adj" %in% names(result))
})

test_that("compute_lm_simple works", {
  mat <- matrix(rnorm(60), nrow = 6, ncol = 10)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:10),
    age = rnorm(10, 50, 10)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_lm_simple(predictor = "age")

  expect_equal(nrow(result), 6)
  expect_true("slope" %in% names(result))
  expect_true("intercept" %in% names(result))
  expect_true("r.squared" %in% names(result))
  expect_true("p.value" %in% names(result))
  expect_true("se" %in% names(result))
  expect_true("p.adj" %in% names(result))
})

test_that("compute_lm_simple with adjust = 'none' omits p.adj", {
  mat <- matrix(rnorm(60), nrow = 6, ncol = 10)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:10),
    age = rnorm(10, 50, 10)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_lm_simple(predictor = "age", adjust = "none")

  expect_false("p.adj" %in% names(result))
})

# =============================================================================
# Tests for add_to_data parameter in convenience functions
# =============================================================================

test_that("compute_ttest with add_to_data works", {
  mat <- matrix(rnorm(60), nrow = 6, ncol = 10)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:10),
    condition = rep(c("Control", "Treatment"), each = 5)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_ttest(group_col = "condition", add_to_data = TRUE, prefix = "ttest")

  expect_s3_class(result, "tidymatrix")
  expect_true("ttest_p.value" %in% names(result$row_data))
  expect_true("ttest_log2fc" %in% names(result$row_data))
  expect_true("ttest_p.adj" %in% names(result$row_data))
})

test_that("compute_wilcox with add_to_data works", {
  mat <- matrix(rnorm(60), nrow = 6, ncol = 10)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:10),
    condition = rep(c("Control", "Treatment"), each = 5)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_wilcox(group_col = "condition", add_to_data = TRUE, prefix = "wilcox")

  expect_s3_class(result, "tidymatrix")
  expect_true("wilcox_p.value" %in% names(result$row_data))
  expect_true("wilcox_log2fc" %in% names(result$row_data))
  expect_true("wilcox_median_diff" %in% names(result$row_data))
})

test_that("compute_anova with add_to_data works", {
  mat <- matrix(rnorm(90), nrow = 6, ncol = 15)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:15),
    condition = rep(c("A", "B", "C"), each = 5)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_anova(group_col = "condition", add_to_data = TRUE, prefix = "anova")

  expect_s3_class(result, "tidymatrix")
  expect_true("anova_f.statistic" %in% names(result$row_data))
  expect_true("anova_p.value" %in% names(result$row_data))
  expect_true("anova_p.adj" %in% names(result$row_data))
})

test_that("compute_correlation with add_to_data works", {
  mat <- matrix(rnorm(60), nrow = 6, ncol = 10)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:10),
    age = rnorm(10, 50, 10)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_correlation(var = "age", add_to_data = TRUE, prefix = "cor")

  expect_s3_class(result, "tidymatrix")
  expect_true("cor_correlation" %in% names(result$row_data))
  expect_true("cor_p.value" %in% names(result$row_data))
  expect_true("cor_p.adj" %in% names(result$row_data))
})

test_that("compute_lm with add_to_data works", {
  mat <- matrix(rnorm(60), nrow = 6, ncol = 10)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:10),
    condition = factor(rep(c("Control", "Treatment"), each = 5)),
    batch = factor(rep(1:2, 5))
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_lm(formula_rhs = ~ condition + batch, coef = "conditionTreatment",
            add_to_data = TRUE, prefix = "lm")

  expect_s3_class(result, "tidymatrix")
  expect_true("lm_estimate" %in% names(result$row_data))
  expect_true("lm_p.value" %in% names(result$row_data))
  expect_true("lm_p.adj" %in% names(result$row_data))
})

test_that("Convenience functions can be chained with add_to_data", {
  mat <- matrix(rnorm(60), nrow = 6, ncol = 10)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample = paste0("S", 1:10),
    condition = rep(c("Control", "Treatment"), each = 5),
    age = rnorm(10, 50, 10)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  # Chain multiple tests
  result <- tm |>
    activate(rows) |>
    compute_ttest(group_col = "condition", add_to_data = TRUE, prefix = "ttest") |>
    compute_correlation(var = "age", add_to_data = TRUE, prefix = "cor")

  expect_s3_class(result, "tidymatrix")
  expect_true("ttest_p.value" %in% names(result$row_data))
  expect_true("cor_correlation" %in% names(result$row_data))

  # Can then filter based on results
  filtered <- result |>
    activate(rows) |>
    filter(ttest_p.adj < 0.5)

  expect_s3_class(filtered, "tidymatrix")
})
