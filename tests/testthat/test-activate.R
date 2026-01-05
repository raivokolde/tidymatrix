test_that("activate switches to rows", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat)

  tm_rows <- activate(tm, rows)

  expect_equal(tm_rows$active, "rows")
  expect_s3_class(tm_rows, "tidymatrix")
})

test_that("activate switches to columns", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat)

  tm_cols <- activate(tm, columns)

  expect_equal(tm_cols$active, "columns")
  expect_s3_class(tm_cols, "tidymatrix")
})

test_that("activate switches to matrix", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat)
  tm$active <- "rows"

  tm_mat <- activate(tm, matrix)

  expect_equal(tm_mat$active, "matrix")
  expect_s3_class(tm_mat, "tidymatrix")
})

test_that("activate works with pipe", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat)

  tm_rows <- tm |> activate(rows)

  expect_equal(tm_rows$active, "rows")
})

test_that("activate rejects invalid activation targets", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat)

  expect_error(
    activate(tm, invalid),
    "`what` must be one of 'rows', 'columns', or 'matrix'"
  )
})

test_that("activate only works on tidymatrix objects", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)

  expect_error(
    activate(mat, rows),
    "activate\\(\\) can only be used on tidymatrix objects"
  )

  expect_error(
    activate(data.frame(a = 1:3), rows),
    "activate\\(\\) can only be used on tidymatrix objects"
  )
})

test_that("activate preserves all data", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4, group = c("A", "A", "B", "B"))
  col_data <- data.frame(id = 1:3, type = c("x", "y", "z"))
  tm <- tidymatrix(mat, row_data, col_data)

  tm_rows <- activate(tm, rows)

  expect_identical(tm_rows$matrix, mat)
  expect_identical(tm_rows$row_data, row_data)
  expect_identical(tm_rows$col_data, col_data)
})

test_that("active returns the active component", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat)

  expect_equal(active(tm), "matrix")

  tm <- activate(tm, rows)
  expect_equal(active(tm), "rows")

  tm <- activate(tm, columns)
  expect_equal(active(tm), "columns")
})

test_that("active only works on tidymatrix objects", {
  expect_error(
    active(matrix(1:12, nrow = 4)),
    "active\\(\\) can only be used on tidymatrix objects"
  )
})
