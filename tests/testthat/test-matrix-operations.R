library(dplyr)

# Test transpose ----

test_that("transpose swaps rows and columns", {
  mat <- matrix(1:12, nrow = 3, ncol = 4)
  row_data <- data.frame(gene = paste0("G", 1:3))
  col_data <- data.frame(sample = paste0("S", 1:4))
  tm <- tidymatrix(mat, row_data, col_data)

  tm_t <- t(tm)

  expect_equal(dim(tm_t$matrix), c(4, 3))
  expect_equal(nrow(tm_t$row_data), 4)
  expect_equal(nrow(tm_t$col_data), 3)
  expect_true("sample" %in% names(tm_t$row_data))
  expect_true("gene" %in% names(tm_t$col_data))
})

test_that("transpose works regardless of active component", {
  mat <- matrix(1:12, nrow = 3, ncol = 4)
  tm <- tidymatrix(mat)

  # Works with matrix active
  tm1 <- tm |> activate(matrix) |> t()
  expect_equal(dim(tm1$matrix), c(4, 3))

  # Works with rows active
  tm2 <- tm |> activate(rows) |> t()
  expect_equal(dim(tm2$matrix), c(4, 3))

  # Works with columns active
  tm3 <- tm |> activate(columns) |> t()
  expect_equal(dim(tm3$matrix), c(4, 3))
})

test_that("transpose updates active state", {
  mat <- matrix(1:12, nrow = 3, ncol = 4)
  tm <- tidymatrix(mat)

  tm_rows <- tm |> activate(rows) |> t()
  expect_equal(tm_rows$active, "columns")

  tm_cols <- tm |> activate(columns) |> t()
  expect_equal(tm_cols$active, "rows")

  tm_mat <- tm |> activate(matrix) |> t()
  expect_equal(tm_mat$active, "matrix")
})

test_that("transpose removes stored analyses", {
  mat <- matrix(rnorm(20), nrow = 4, ncol = 5)
  tm <- tidymatrix(mat)

  tm <- tm |>
    activate(rows) |>
    compute_prcomp(name = "pca")

  expect_equal(list_analyses(tm), "pca")

  expect_warning(
    tm_t <- t(tm),
    "Removed 1 stored analysis"
  )

  expect_equal(length(list_analyses(tm_t)), 0)
})

# Test scale ----

test_that("scale works on rows", {
  set.seed(123)
  mat <- matrix(rnorm(20, mean = 10, sd = 5), nrow = 4, ncol = 5)
  tm <- tidymatrix(mat)

  tm_scaled <- tm |>
    activate(rows) |>
    scale()

  # Each row should have mean ~0 and sd ~1
  row_means <- rowMeans(tm_scaled$matrix)
  row_sds <- apply(tm_scaled$matrix, 1, sd)

  expect_true(all(abs(row_means) < 1e-10))
  expect_true(all(abs(row_sds - 1) < 1e-10))
})

test_that("scale works on columns", {
  set.seed(123)
  mat <- matrix(rnorm(20, mean = 10, sd = 5), nrow = 4, ncol = 5)
  tm <- tidymatrix(mat)

  tm_scaled <- tm |>
    activate(columns) |>
    scale()

  # Each column should have mean ~0 and sd ~1
  col_means <- colMeans(tm_scaled$matrix)
  col_sds <- apply(tm_scaled$matrix, 2, sd)

  expect_true(all(abs(col_means) < 1e-10))
  expect_true(all(abs(col_sds - 1) < 1e-10))
})

test_that("scale with center=FALSE works", {
  mat <- matrix(rnorm(20, mean = 10), nrow = 4, ncol = 5)
  tm <- tidymatrix(mat)

  # Should run without error
  tm_scaled <- tm |>
    activate(rows) |>
    scale(center = FALSE, scale = TRUE)

  # Matrix should still have same dimensions
  expect_equal(dim(tm_scaled$matrix), dim(mat))

  # Values should be different from original
  expect_false(identical(tm_scaled$matrix, mat))
})

test_that("scale errors when matrix is active", {
  mat <- matrix(rnorm(20), nrow = 4, ncol = 5)
  tm <- tidymatrix(mat)

  expect_error(
    tm |> activate(matrix) |> scale(),
    "Cannot scale when matrix is active"
  )
})

# Test center ----

test_that("center works on rows", {
  mat <- matrix(rnorm(20, mean = 10), nrow = 4, ncol = 5)
  tm <- tidymatrix(mat)

  tm_centered <- tm |>
    activate(rows) |>
    center()

  # Each row should have mean ~0
  row_means <- rowMeans(tm_centered$matrix)
  expect_true(all(abs(row_means) < 1e-10))
})

test_that("center works on columns", {
  mat <- matrix(rnorm(20, mean = 10), nrow = 4, ncol = 5)
  tm <- tidymatrix(mat)

  tm_centered <- tm |>
    activate(columns) |>
    center()

  # Each column should have mean ~0
  col_means <- colMeans(tm_centered$matrix)
  expect_true(all(abs(col_means) < 1e-10))
})

test_that("center errors when matrix is active", {
  mat <- matrix(rnorm(20), nrow = 4, ncol = 5)
  tm <- tidymatrix(mat)

  expect_error(
    tm |> activate(matrix) |> center(),
    "Cannot center when matrix is active"
  )
})

# Test transform ----

test_that("transform applies function to matrix", {
  mat <- matrix(1:12, nrow = 3, ncol = 4)
  tm <- tidymatrix(mat)

  tm_squared <- tm |>
    activate(matrix) |>
    transform(.^2)

  expect_equal(tm_squared$matrix, mat^2)
})

test_that("transform works with log", {
  mat <- matrix(1:12, nrow = 3, ncol = 4)
  tm <- tidymatrix(mat)

  tm_log <- tm |>
    activate(matrix) |>
    transform(log2(. + 1))

  expect_equal(tm_log$matrix, log2(mat + 1))
})

test_that("transform errors when matrix not active", {
  mat <- matrix(1:12, nrow = 3, ncol = 4)
  tm <- tidymatrix(mat)

  expect_error(
    tm |> activate(rows) |> transform(.^2),
    "Cannot transform when matrix is not active"
  )
})

# Test add_stats ----

test_that("add_stats adds row statistics", {
  mat <- matrix(rnorm(20), nrow = 4, ncol = 5)
  tm <- tidymatrix(mat)

  tm <- tm |>
    activate(rows) |>
    add_stats(mean, var, sd)

  expect_true("mean" %in% names(tm$row_data))
  expect_true("var" %in% names(tm$row_data))
  expect_true("sd" %in% names(tm$row_data))

  # Check values are correct
  expect_equal(tm$row_data$mean, rowMeans(mat))
  expect_equal(tm$row_data$var, apply(mat, 1, var))
})

test_that("add_stats adds column statistics", {
  mat <- matrix(rnorm(20), nrow = 4, ncol = 5)
  tm <- tidymatrix(mat)

  tm <- tm |>
    activate(columns) |>
    add_stats(median, min, max)

  expect_true("median" %in% names(tm$col_data))
  expect_true("min" %in% names(tm$col_data))
  expect_true("max" %in% names(tm$col_data))

  # Check values
  expect_equal(tm$col_data$median, apply(mat, 2, median))
  expect_equal(tm$col_data$min, apply(mat, 2, min))
  expect_equal(tm$col_data$max, apply(mat, 2, max))
})

test_that("add_stats with custom names", {
  mat <- matrix(rnorm(20), nrow = 4, ncol = 5)
  tm <- tidymatrix(mat)

  tm <- tm |>
    activate(rows) |>
    add_stats(mean, var, .names = c("row_mean", "row_var"))

  expect_true("row_mean" %in% names(tm$row_data))
  expect_true("row_var" %in% names(tm$row_data))
  expect_false("mean" %in% names(tm$row_data))
})

test_that("add_stats errors when matrix is active", {
  mat <- matrix(rnorm(20), nrow = 4, ncol = 5)
  tm <- tidymatrix(mat)

  expect_error(
    tm |> activate(matrix) |> add_stats(mean),
    "Cannot add_stats when matrix is active"
  )
})

test_that("add_stats works with filter", {
  mat <- matrix(rnorm(20), nrow = 4, ncol = 5)
  row_data <- data.frame(id = 1:4, group = c("A", "A", "B", "B"))
  tm <- tidymatrix(mat, row_data)

  tm <- tm |>
    activate(rows) |>
    add_stats(mean, var) |>
    filter(var > median(var))

  expect_true("mean" %in% names(tm$row_data))
  expect_true("var" %in% names(tm$row_data))
  expect_equal(nrow(tm$row_data), 2)
})

# Test log_transform ----

test_that("log_transform with base 2", {
  mat <- matrix(1:12, nrow = 3, ncol = 4)
  tm <- tidymatrix(mat)

  tm_log <- tm |>
    activate(matrix) |>
    log_transform(base = 2, offset = 1)

  expect_equal(tm_log$matrix, log2(mat + 1))
})

test_that("log_transform with base 10", {
  mat <- matrix(1:12, nrow = 3, ncol = 4)
  tm <- tidymatrix(mat)

  tm_log <- tm |>
    activate(matrix) |>
    log_transform(base = 10, offset = 0)

  expect_equal(tm_log$matrix, log10(mat))
})

test_that("log_transform with natural log", {
  mat <- matrix(1:12, nrow = 3, ncol = 4)
  tm <- tidymatrix(mat)

  tm_log <- tm |>
    activate(matrix) |>
    log_transform(base = "natural", offset = 1)

  expect_equal(tm_log$matrix, log(mat + 1))
})

test_that("log_transform errors when matrix not active", {
  mat <- matrix(1:12, nrow = 3, ncol = 4)
  tm <- tidymatrix(mat)

  expect_error(
    tm |> activate(rows) |> log_transform(),
    "Cannot log_transform when matrix is not active"
  )
})

# Test clip_values ----

test_that("clip_values sets floor and ceiling", {
  mat <- matrix(c(-5, -1, 0, 1, 5, 10), nrow = 2, ncol = 3)
  tm <- tidymatrix(mat)

  tm_clipped <- tm |>
    activate(matrix) |>
    clip_values(min = 0, max = 5)

  expect_equal(min(tm_clipped$matrix), 0)
  expect_equal(max(tm_clipped$matrix), 5)
  expect_true(all(tm_clipped$matrix >= 0))
  expect_true(all(tm_clipped$matrix <= 5))
})

test_that("clip_values with only min", {
  mat <- matrix(c(-5, -1, 0, 1, 5), nrow = 1, ncol = 5)
  tm <- tidymatrix(mat)

  tm_clipped <- tm |>
    activate(matrix) |>
    clip_values(min = 0)

  expect_equal(min(tm_clipped$matrix), 0)
  expect_equal(max(tm_clipped$matrix), 5)
})

test_that("clip_values with only max", {
  mat <- matrix(c(-5, -1, 0, 1, 5), nrow = 1, ncol = 5)
  tm <- tidymatrix(mat)

  tm_clipped <- tm |>
    activate(matrix) |>
    clip_values(max = 2)

  expect_equal(min(tm_clipped$matrix), -5)
  expect_equal(max(tm_clipped$matrix), 2)
})

test_that("clip_values errors when matrix not active", {
  mat <- matrix(1:12, nrow = 3, ncol = 4)
  tm <- tidymatrix(mat)

  expect_error(
    tm |> activate(rows) |> clip_values(min = 0),
    "Cannot clip_values when matrix is not active"
  )
})

# Test chaining operations ----

test_that("can chain multiple operations", {
  set.seed(123)
  mat <- matrix(abs(rnorm(20, mean = 100, sd = 50)), nrow = 4, ncol = 5)
  row_data <- data.frame(gene = paste0("G", 1:4))
  tm <- tidymatrix(mat, row_data)

  result <- tm |>
    activate(matrix) |>
    log_transform(base = 2, offset = 1) |>
    activate(rows) |>
    center() |>
    scale() |>
    add_stats(mean, sd)

  # Check log transform happened
  expect_true(all(result$matrix < 10))  # Log values should be smaller

  # Check centering and scaling
  row_means <- rowMeans(result$matrix)
  row_sds <- apply(result$matrix, 1, sd)
  expect_true(all(abs(row_means) < 1e-10))
  expect_true(all(abs(row_sds - 1) < 1e-10))

  # Check stats were added
  expect_true("mean" %in% names(result$row_data))
  expect_true("sd" %in% names(result$row_data))
})

test_that("operations work with grouping", {
  mat <- matrix(rnorm(20), nrow = 10, ncol = 2)
  row_data <- data.frame(
    id = 1:10,
    group = rep(c("A", "B"), each = 5)
  )
  tm <- tidymatrix(mat, row_data)

  result <- tm |>
    activate(rows) |>
    add_stats(mean, var) |>
    group_by(group) |>
    summarize(
      n = n(),
      avg_mean = mean(mean),
      avg_var = mean(var)
    )

  expect_equal(nrow(result$row_data), 2)
  expect_true("avg_mean" %in% names(result$row_data))
})

# Test transform_matrix ----

## Element-wise (matrix active) ----

test_that("transform_matrix applies base function element-wise", {
  mat <- matrix(1:12, nrow = 3, ncol = 4)
  tm <- tidymatrix(mat)

  result <- tm |> activate(matrix) |> transform_matrix(log)
  expect_equal(result$matrix, log(mat))
})

test_that("transform_matrix applies anonymous function element-wise", {
  mat <- matrix(1:12, nrow = 3, ncol = 4)
  tm <- tidymatrix(mat)

  result <- tm |> activate(matrix) |> transform_matrix(\(x) x^2 + 1)
  expect_equal(result$matrix, mat^2 + 1)
})

test_that("transform_matrix element-wise preserves dimensions", {
  mat <- matrix(1:12, nrow = 3, ncol = 4)
  tm <- tidymatrix(mat)

  result <- tm |> activate(matrix) |> transform_matrix(exp)
  expect_equal(dim(result$matrix), c(3, 4))
})

## Row-wise (rows active) ----

test_that("transform_matrix applies function to each row independently", {
  mat <- matrix(c(3, 1, 2, 6, 4, 5), nrow = 2, ncol = 3)
  tm <- tidymatrix(mat)

  result <- tm |> activate(rows) |> transform_matrix(rank)
  # Row 1: 3, 2, 4 -> ranks 2, 1, 3
  # Row 2: 1, 6, 5 -> ranks 1, 3, 2
  expect_equal(as.vector(result$matrix[1, ]), c(2, 1, 3))
  expect_equal(as.vector(result$matrix[2, ]), c(1, 3, 2))
})

test_that("transform_matrix row-wise normalization sums to 1", {
  mat <- matrix(c(2, 4, 6, 8, 10, 12), nrow = 2, ncol = 3)
  tm <- tidymatrix(mat)

  result <- tm |> activate(rows) |> transform_matrix(\(x) x / sum(x))
  expect_equal(rowSums(result$matrix), c(1, 1))
})

## Column-wise (columns active) ----

test_that("transform_matrix applies function to each column independently", {
  mat <- matrix(c(3, 1, 2, 6, 4, 5), nrow = 3, ncol = 2)
  tm <- tidymatrix(mat)

  result <- tm |> activate(columns) |> transform_matrix(rank)
  # Col 1: 3, 1, 2 -> ranks 3, 1, 2
  # Col 2: 6, 4, 5 -> ranks 3, 1, 2
  expect_equal(as.vector(result$matrix[, 1]), c(3, 1, 2))
  expect_equal(as.vector(result$matrix[, 2]), c(3, 1, 2))
})

test_that("transform_matrix column-wise min-max normalization", {
  mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
  tm <- tidymatrix(mat)

  result <- tm |>
    activate(columns) |>
    transform_matrix(\(x) (x - min(x)) / (max(x) - min(x)))

  # Each column should span [0, 1]
  expect_equal(min(result$matrix[, 1]), 0)
  expect_equal(max(result$matrix[, 1]), 1)
  expect_equal(min(result$matrix[, 2]), 0)
  expect_equal(max(result$matrix[, 2]), 1)
})

## Edge cases ----

test_that("transform_matrix works with single row matrix", {
  mat <- matrix(c(3, 1, 4, 2), nrow = 1, ncol = 4)
  tm <- tidymatrix(mat)

  result <- tm |> activate(rows) |> transform_matrix(rank)
  expect_equal(dim(result$matrix), c(1, 4))
  expect_equal(as.vector(result$matrix), c(2, 1, 4, 3))
})

test_that("transform_matrix works with single column matrix", {
  mat <- matrix(c(3, 1, 2), nrow = 3, ncol = 1)
  tm <- tidymatrix(mat)

  result <- tm |> activate(columns) |> transform_matrix(rank)
  expect_equal(dim(result$matrix), c(3, 1))
  expect_equal(as.vector(result$matrix), c(3, 1, 2))
})

test_that("transform_matrix preserves dimnames", {
  mat <- matrix(1:6, nrow = 2, ncol = 3,
                dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
  tm <- tidymatrix(mat)

  result <- tm |> activate(matrix) |> transform_matrix(log)
  expect_equal(rownames(result$matrix), c("r1", "r2"))
  expect_equal(colnames(result$matrix), c("c1", "c2", "c3"))
})

## Analysis invalidation ----

test_that("transform_matrix removes stored analyses", {
  mat <- matrix(rnorm(20), nrow = 4, ncol = 5)
  tm <- tidymatrix(mat)

  tm <- tm |>
    activate(rows) |>
    compute_prcomp(name = "pca")

  expect_equal(list_analyses(tm), "pca")

  expect_warning(
    result <- tm |> activate(matrix) |> transform_matrix(exp),
    "Removed 1 stored analysis"
  )
  expect_equal(length(list_analyses(result)), 0)
})

## Error cases ----

test_that("transform_matrix errors on non-tidymatrix input", {
  expect_error(
    transform_matrix(data.frame(x = 1:3), log),
    "can only be used on tidymatrix objects"
  )
})

test_that("transform_matrix errors when fn is not a function", {
  mat <- matrix(1:12, nrow = 3, ncol = 4)
  tm <- tidymatrix(mat)

  expect_error(
    tm |> activate(matrix) |> transform_matrix(42),
    "fn must be a function"
  )
})

test_that("transform_matrix errors when row fn changes length", {
  mat <- matrix(1:12, nrow = 3, ncol = 4)
  tm <- tidymatrix(mat)

  expect_error(
    tm |> activate(rows) |> transform_matrix(\(x) x[1:2]),
    "fn must return values with the same dimensions"
  )
})

test_that("transform_matrix errors when column fn changes length", {
  mat <- matrix(1:12, nrow = 3, ncol = 4)
  tm <- tidymatrix(mat)

  expect_error(
    tm |> activate(columns) |> transform_matrix(\(x) x[1:2]),
    "fn must return values with the same dimensions"
  )
})
