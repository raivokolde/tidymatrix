test_that("tidymatrix constructor works with all arguments", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4, group = c("A", "A", "B", "B"))
  col_data <- data.frame(id = 1:3, type = c("x", "y", "z"))

  tm <- tidymatrix(mat, row_data, col_data)

  expect_s3_class(tm, "tidymatrix")
  expect_identical(tm$matrix, mat)
  expect_identical(tm$row_data, row_data)
  expect_identical(tm$col_data, col_data)
  expect_equal(tm$active, "matrix")
})

test_that("tidymatrix creates default row_data when NULL", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat)

  expect_s3_class(tm, "tidymatrix")
  expect_equal(nrow(tm$row_data), 4)
  expect_equal(ncol(tm$row_data), 1)
  expect_equal(tm$row_data$.row_id, 1:4)
})

test_that("tidymatrix creates default col_data when NULL", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat)

  expect_s3_class(tm, "tidymatrix")
  expect_equal(nrow(tm$col_data), 3)
  expect_equal(ncol(tm$col_data), 1)
  expect_equal(tm$col_data$.col_id, 1:3)
})

test_that("tidymatrix validates matrix input", {
  expect_error(
    tidymatrix(data.frame(a = 1:3)),
    "`matrix` must be a matrix object"
  )

  expect_error(
    tidymatrix(c(1, 2, 3)),
    "`matrix` must be a matrix object"
  )
})

test_that("tidymatrix validates row_data dimensions", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:3)  # Wrong number of rows

  expect_error(
    tidymatrix(mat, row_data),
    "`row_data` must have the same number of rows as `matrix`"
  )
})

test_that("tidymatrix validates col_data dimensions", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  col_data <- data.frame(id = 1:4)  # Wrong number of rows

  expect_error(
    tidymatrix(mat, col_data = col_data),
    "`col_data` must have the same number of rows as `matrix` has columns"
  )
})

test_that("is_tidymatrix correctly identifies tidymatrix objects", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat)

  expect_true(is_tidymatrix(tm))
  expect_false(is_tidymatrix(mat))
  expect_false(is_tidymatrix(data.frame(a = 1:3)))
  expect_false(is_tidymatrix(list()))
})

test_that("validate_tidymatrix accepts valid objects", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat)

  expect_invisible(validate_tidymatrix(tm))
})

test_that("validate_tidymatrix rejects invalid objects", {
  expect_error(
    validate_tidymatrix(list()),
    "Object is not a tidymatrix"
  )
})

test_that("validate_tidymatrix detects mismatched dimensions", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat)

  # Manually corrupt the object
  tm$row_data <- tm$row_data[1:2, , drop = FALSE]

  expect_error(
    validate_tidymatrix(tm),
    "Number of rows in row_data does not match matrix rows"
  )
})

test_that("print.tidymatrix works without error", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4)
  col_data <- data.frame(id = 1:3)
  tm <- tidymatrix(mat, row_data, col_data)

  expect_output(print(tm), "A tidymatrix")
  expect_output(print(tm), "4 x 3 matrix")
  expect_output(print(tm), "Active: matrix")
})

test_that("print.tidymatrix shows active component", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4, group = c("A", "A", "B", "B"))
  tm <- tidymatrix(mat, row_data)

  tm$active <- "rows"
  expect_output(print(tm), "Active: rows")
  expect_output(print(tm), "Active data \\(rows\\)")

  tm$active <- "columns"
  expect_output(print(tm), "Active: columns")
  expect_output(print(tm), "Active data \\(columns\\)")
})
