# Tests for to_long() function

# Basic functionality ----

test_that("flatten works with simple matrix", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(row_id = 1:4, row_group = c("A", "A", "B", "B"))
  col_data <- data.frame(col_id = 1:3, col_type = c("x", "y", "z"))
  tm <- tidymatrix(mat, row_data, col_data)

  result <- to_long(tm)

  expect_equal(nrow(result), 12)  # 4 * 3
  expect_equal(ncol(result), 5)   # 2 row cols + 2 col cols + 1 value col
  expect_true("value" %in% names(result))
  expect_equal(result$value, as.vector(mat))
})

test_that("flatten returns correct dimensions", {
  mat <- matrix(1:20, nrow = 5, ncol = 4)
  tm <- tidymatrix(mat)

  result <- to_long(tm)

  expect_equal(nrow(result), 20)
  expect_true("value" %in% names(result))
})

# Column name conflicts ----

test_that("flatten handles column name conflicts", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4, type = c("A", "A", "B", "B"))
  col_data <- data.frame(id = 1:3, type = c("x", "y", "z"))
  tm <- tidymatrix(mat, row_data, col_data)

  result <- to_long(tm)

  expect_true("row.id" %in% names(result))
  expect_true("row.type" %in% names(result))
  expect_true("col.id" %in% names(result))
  expect_true("col.type" %in% names(result))
  expect_false("id" %in% names(result))
  expect_false("type" %in% names(result))
})

test_that("flatten handles partial column name conflicts", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4, row_specific = c("A", "A", "B", "B"))
  col_data <- data.frame(id = 1:3, col_specific = c("x", "y", "z"))
  tm <- tidymatrix(mat, row_data, col_data)

  result <- to_long(tm)

  # When conflicts exist, ALL columns get prefixed
  expect_true("row.id" %in% names(result))
  expect_true("row.row_specific" %in% names(result))
  expect_true("col.id" %in% names(result))
  expect_true("col.col_specific" %in% names(result))
})

# No conflicts ----

test_that("flatten preserves column names when no conflicts", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(row_id = 1:4, row_group = c("A", "A", "B", "B"))
  col_data <- data.frame(col_id = 1:3, col_type = c("x", "y", "z"))
  tm <- tidymatrix(mat, row_data, col_data)

  result <- to_long(tm)

  expect_true("row_id" %in% names(result))
  expect_true("row_group" %in% names(result))
  expect_true("col_id" %in% names(result))
  expect_true("col_type" %in% names(result))
  expect_false("row.row_id" %in% names(result))
  expect_false("col.col_id" %in% names(result))
})

# Default metadata ----

test_that("flatten works with default metadata", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat)

  result <- to_long(tm)

  expect_equal(nrow(result), 12)
  expect_true(".row_id" %in% names(result))
  expect_true(".col_id" %in% names(result))
  expect_true("value" %in% names(result))
})

# return_tibble parameter ----

test_that("flatten returns tibble by default", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat)

  result <- to_long(tm)

  expect_s3_class(result, "tbl_df")
})

test_that("flatten returns data.frame when return_tibble = FALSE", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat)

  result <- to_long(tm, return_tibble = FALSE)

  expect_true(is.data.frame(result))
  expect_false(inherits(result, "tbl_df"))
})

# Correctness of row/col mapping ----

test_that("flatten correctly maps row and column metadata", {
  mat <- matrix(1:6, nrow = 2, ncol = 3)
  row_data <- data.frame(r = c("R1", "R2"))
  col_data <- data.frame(c = c("C1", "C2", "C3"))
  tm <- tidymatrix(mat, row_data, col_data)

  result <- to_long(tm)

  # Check first few rows manually
  # Column-major order: C1-R1, C1-R2, C2-R1, C2-R2, C3-R1, C3-R2
  expect_equal(result$r[1], "R1")
  expect_equal(result$c[1], "C1")
  expect_equal(result$value[1], 1)

  expect_equal(result$r[2], "R2")
  expect_equal(result$c[2], "C1")
  expect_equal(result$value[2], 2)

  expect_equal(result$r[3], "R1")
  expect_equal(result$c[3], "C2")
  expect_equal(result$value[3], 3)
})

test_that("flatten uses column-major order", {
  mat <- matrix(1:6, nrow = 2, ncol = 3)
  tm <- tidymatrix(mat)

  result <- to_long(tm)

  # Column-major order means: col1 values (1,2), then col2 values (3,4), then col3 values (5,6)
  expect_equal(result$value, c(1, 2, 3, 4, 5, 6))
})

# Active component doesn't matter ----

test_that("flatten works regardless of active component", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat)

  result1 <- tm |> activate(matrix) |> to_long()
  result2 <- tm |> activate(rows) |> to_long()
  result3 <- tm |> activate(columns) |> to_long()

  expect_equal(result1, result2)
  expect_equal(result2, result3)
})

# Empty matrix edge cases ----

test_that("flatten handles empty matrices with 0 rows", {
  mat <- matrix(numeric(0), nrow = 0, ncol = 3)
  tm <- tidymatrix(mat)

  result <- to_long(tm)

  expect_equal(nrow(result), 0)
  expect_true("value" %in% names(result))
  expect_true(".row_id" %in% names(result))
  expect_true(".col_id" %in% names(result))
})

test_that("flatten handles empty matrices with 0 columns", {
  mat <- matrix(numeric(0), nrow = 3, ncol = 0)
  tm <- tidymatrix(mat)

  result <- to_long(tm)

  expect_equal(nrow(result), 0)
  expect_true("value" %in% names(result))
})

# NA values ----

test_that("flatten preserves NA values", {
  mat <- matrix(c(1, NA, 3, 4), nrow = 2, ncol = 2)
  tm <- tidymatrix(mat)

  result <- to_long(tm)

  expect_equal(sum(is.na(result$value)), 1)
  expect_equal(result$value[2], NA_real_)
})

test_that("flatten handles matrix with all NAs", {
  mat <- matrix(NA_real_, nrow = 3, ncol = 2)
  tm <- tidymatrix(mat)

  result <- to_long(tm)

  expect_equal(nrow(result), 6)
  expect_true(all(is.na(result$value)))
})

# Integration with compute_ttest ----

test_that("flatten works in analysis workflow", {
  set.seed(42)
  mat <- matrix(rnorm(60), nrow = 6, ncol = 10)
  row_data <- data.frame(gene = paste0("Gene", 1:6))
  col_data <- data.frame(
    sample_id = paste0("S", 1:10),
    condition = rep(c("Control", "Treatment"), each = 5)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_ttest(group_col = "condition", add_to_data = TRUE) |>
    to_long()

  expect_equal(nrow(result), 60)  # 6 genes * 10 samples
  expect_true("gene" %in% names(result))
  expect_true("sample_id" %in% names(result))
  expect_true("condition" %in% names(result))
  expect_true("value" %in% names(result))
  # ttest results should be in every row (repeated)
  expect_true("p.value" %in% names(result))
})

# Non-tidymatrix input ----

test_that("flatten fails on non-tidymatrix", {
  expect_error(
    to_long(data.frame(x = 1:3)),
    "can only be used on tidymatrix objects"
  )
})

test_that("flatten fails on NULL", {
  expect_error(
    to_long(NULL),
    "can only be used on tidymatrix objects"
  )
})

# After filter ----

test_that("flatten works after filtering rows", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4, group = c("A", "A", "B", "B"))
  tm <- tidymatrix(mat, row_data)

  result <- tm |>
    activate(rows) |>
    filter(group == "A") |>
    to_long()

  expect_equal(nrow(result), 6)  # 2 rows * 3 cols
  expect_true(all(result$group == "A"))
})

test_that("flatten works after filtering columns", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  col_data <- data.frame(id = 1:3, type = c("x", "y", "x"))
  tm <- tidymatrix(mat, col_data = col_data)

  result <- tm |>
    activate(columns) |>
    filter(type == "x") |>
    to_long()

  expect_equal(nrow(result), 8)  # 4 rows * 2 cols
  expect_true(all(result$type == "x"))
})

# Single row/column ----

test_that("flatten works with single row matrix", {
  mat <- matrix(1:5, nrow = 1, ncol = 5)
  tm <- tidymatrix(mat)

  result <- to_long(tm)

  expect_equal(nrow(result), 5)
  expect_equal(result$value, 1:5)
})

test_that("flatten works with single column matrix", {
  mat <- matrix(1:5, nrow = 5, ncol = 1)
  tm <- tidymatrix(mat)

  result <- to_long(tm)

  expect_equal(nrow(result), 5)
  expect_equal(result$value, 1:5)
})

# Large metadata ----

test_that("flatten works with many metadata columns", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(
    id = 1:4,
    group = c("A", "A", "B", "B"),
    type = c("X", "Y", "X", "Y"),
    score = c(1.1, 2.2, 3.3, 4.4)
  )
  col_data <- data.frame(
    sample = 1:3,
    condition = c("Control", "Treatment", "Control"),
    batch = c(1, 1, 2)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- to_long(tm)

  expect_equal(nrow(result), 12)
  expect_equal(ncol(result), 8)  # 4 row cols + 3 col cols + 1 value col
  expect_true(all(c("id", "group", "type", "score", "sample", "condition", "batch", "value") %in% names(result)))
})

# Row names preservation ----

test_that("flatten resets row names to default", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat)

  result <- to_long(tm, return_tibble = FALSE)

  # Data.frames in R always have row names - check they're default (1, 2, 3, ...)
  expect_equal(rownames(result), as.character(1:12))
})
