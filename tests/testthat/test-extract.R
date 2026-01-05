test_that("pull_active works with matrix", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat)

  result <- tm |> activate(matrix) |> pull_active()

  expect_true(is.matrix(result))
  expect_equal(result, mat)
})

test_that("pull_active works with rows", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4, group = c("A", "A", "B", "B"))
  tm <- tidymatrix(mat, row_data)

  result <- tm |> activate(rows) |> pull_active()

  expect_true(is.data.frame(result))
  expect_equal(result, row_data)
})

test_that("pull_active works with columns", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  col_data <- data.frame(id = 1:3, type = c("x", "y", "z"))
  tm <- tidymatrix(mat, col_data = col_data)

  result <- tm |> activate(columns) |> pull_active()

  expect_true(is.data.frame(result))
  expect_equal(result, col_data)
})

test_that("pull_active returns modified data after mutations", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4)
  tm <- tidymatrix(mat, row_data)

  result <- tm |>
    activate(rows) |>
    mutate(new_col = id * 2) |>
    pull_active()

  expect_true("new_col" %in% names(result))
  expect_equal(result$new_col, c(2, 4, 6, 8))
})

test_that("pull_active fails on non-tidymatrix", {
  expect_error(
    pull_active(data.frame(x = 1:3)),
    "can only be used on tidymatrix objects"
  )
})

test_that("as_tibble works with rows active", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4, group = c("A", "A", "B", "B"))
  tm <- tidymatrix(mat, row_data)

  result <- tm |> activate(rows) |> as_tibble()

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 2)
})

test_that("as_tibble works with columns active", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  col_data <- data.frame(id = 1:3, type = c("x", "y", "z"))
  tm <- tidymatrix(mat, col_data = col_data)

  result <- tm |> activate(columns) |> as_tibble()

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 2)
})

test_that("as_tibble fails when matrix is active", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat) |> activate(matrix)

  expect_error(
    as_tibble(tm),
    "Cannot convert to tibble when matrix is active"
  )
})

test_that("as_tibble fails on non-tidymatrix", {
  expect_error(
    as_tibble.tidymatrix(data.frame(x = 1:3)),
    "must be a tidymatrix object"
  )
})

test_that("as.data.frame works with rows active", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4, group = c("A", "A", "B", "B"))
  tm <- tidymatrix(mat, row_data)

  result <- tm |> activate(rows) |> as.data.frame()

  expect_true(is.data.frame(result))
  expect_false(inherits(result, "tbl_df"))  # Should be plain data.frame
  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 2)
})

test_that("as.data.frame works with columns active", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  col_data <- data.frame(id = 1:3, type = c("x", "y", "z"))
  tm <- tidymatrix(mat, col_data = col_data)

  result <- tm |> activate(columns) |> as.data.frame()

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 2)
})

test_that("as.data.frame fails when matrix is active", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat) |> activate(matrix)

  expect_error(
    as.data.frame(tm),
    "Cannot convert to data.frame when matrix is active"
  )
})

test_that("extraction works in a PCA workflow", {
  mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
  row_data <- data.frame(id = 1:10, group = rep(c("A", "B"), each = 5))
  tm <- tidymatrix(mat, row_data)

  # Test pull_active
  result1 <- tm |>
    activate(rows) |>
    compute_prcomp(n_components = 2, center = TRUE, scale. = FALSE) |>
    pull_active()

  expect_true(is.data.frame(result1))
  expect_true("row_pca_PC1" %in% names(result1))
  expect_true("row_pca_PC2" %in% names(result1))

  # Test as_tibble
  result2 <- tm |>
    activate(rows) |>
    compute_prcomp(n_components = 2, center = TRUE, scale. = FALSE) |>
    as_tibble()

  expect_s3_class(result2, "tbl_df")
  expect_true("row_pca_PC1" %in% names(result2))
  expect_true("row_pca_PC2" %in% names(result2))
})

test_that("pull_active preserves filtered data", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4, group = c("A", "A", "B", "B"))
  tm <- tidymatrix(mat, row_data)

  result <- tm |>
    activate(rows) |>
    filter(group == "A") |>
    pull_active()

  expect_equal(nrow(result), 2)
  expect_equal(result$group, c("A", "A"))
})
