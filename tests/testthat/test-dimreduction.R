# Tests for dimensionality reduction methods

# Basic MDS tests (no external dependencies)
test_that("compute_mds works on rows", {
  mat <- matrix(rnorm(100), nrow = 20, ncol = 5)
  row_data <- data.frame(id = 1:20)
  col_data <- data.frame(sample = 1:5)
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_mds(k = 2)

  # Check that columns were added
  expect_true("row_mds_1" %in% names(result$row_data))
  expect_true("row_mds_2" %in% names(result$row_data))

  # Check dimensions
  expect_equal(nrow(result$row_data), 20)
  expect_equal(ncol(result$row_data), 3)  # id + mds_1 + mds_2

  # Check that analysis was stored
  expect_true("row_mds" %in% list_analyses(result))
})

test_that("compute_mds works on columns", {
  mat <- matrix(rnorm(100), nrow = 20, ncol = 5)
  row_data <- data.frame(id = 1:20)
  col_data <- data.frame(sample = 1:5)
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(columns) |>
    compute_mds(k = 2)

  # Check that columns were added to col_data
  expect_true("column_mds_1" %in% names(result$col_data))
  expect_true("column_mds_2" %in% names(result$col_data))

  expect_equal(nrow(result$col_data), 5)
})

test_that("compute_mds k parameter controls dimensions", {
  mat <- matrix(rnorm(100), nrow = 20, ncol = 5)
  row_data <- data.frame(id = 1:20)
  tm <- tidymatrix(mat, row_data)

  result <- tm |>
    activate(rows) |>
    compute_mds(k = 3)

  expect_true("row_mds_1" %in% names(result$row_data))
  expect_true("row_mds_2" %in% names(result$row_data))
  expect_true("row_mds_3" %in% names(result$row_data))
})

test_that("compute_mds is deterministic", {
  mat <- matrix(rnorm(100), nrow = 20, ncol = 5)
  row_data <- data.frame(id = 1:20)
  tm <- tidymatrix(mat, row_data)

  result1 <- tm |> activate(rows) |> compute_mds(k = 2, store = FALSE)
  result2 <- tm |> activate(rows) |> compute_mds(k = 2, store = FALSE)

  expect_equal(result1$row_data$row_mds_1, result2$row_data$row_mds_1)
  expect_equal(result1$row_data$row_mds_2, result2$row_data$row_mds_2)
})

test_that("compute_mds store=FALSE doesn't store analysis", {
  mat <- matrix(rnorm(100), nrow = 20, ncol = 5)
  row_data <- data.frame(id = 1:20)
  tm <- tidymatrix(mat, row_data)

  result <- tm |>
    activate(rows) |>
    compute_mds(k = 2, store = FALSE)

  expect_equal(length(list_analyses(result)), 0)
  expect_true("row_mds_1" %in% names(result$row_data))
})

test_that("compute_mds errors when matrix is active", {
  mat <- matrix(rnorm(100), nrow = 20, ncol = 5)
  row_data <- data.frame(id = 1:20)
  tm <- tidymatrix(mat, row_data)

  expect_error(
    compute_mds(tm, k = 2),
    "Cannot compute MDS when matrix is active"
  )
})

test_that("compute_mds errors on non-tidymatrix input", {
  expect_error(
    compute_mds(matrix(1:10, 2, 5)),
    "must be a tidymatrix object"
  )
})

test_that("compute_mds with custom name", {
  mat <- matrix(rnorm(100), nrow = 20, ncol = 5)
  row_data <- data.frame(id = 1:20)
  tm <- tidymatrix(mat, row_data)

  result <- tm |>
    activate(rows) |>
    compute_mds(k = 2, name = "custom_mds")

  expect_true("custom_mds_1" %in% names(result$row_data))
  expect_true("custom_mds_2" %in% names(result$row_data))
  expect_true("custom_mds" %in% list_analyses(result))
})

# t-SNE tests (conditional on Rtsne package)
test_that("compute_tsne works on rows", {
  skip_if_not_installed("Rtsne")

  mat <- matrix(rnorm(250), nrow = 50, ncol = 5)
  row_data <- data.frame(id = 1:50)
  col_data <- data.frame(sample = 1:5)
  tm <- tidymatrix(mat, row_data, col_data)

  set.seed(42)
  result <- tm |>
    activate(rows) |>
    compute_tsne(dims = 2, perplexity = 10)

  # Check that columns were added
  expect_true("row_tsne_1" %in% names(result$row_data))
  expect_true("row_tsne_2" %in% names(result$row_data))

  # Check dimensions
  expect_equal(nrow(result$row_data), 50)
  expect_equal(ncol(result$row_data), 3)  # id + tsne_1 + tsne_2

  # Check that analysis was stored
  expect_true("row_tsne" %in% list_analyses(result))
})

test_that("compute_tsne works on columns", {
  skip_if_not_installed("Rtsne")

  mat <- matrix(rnorm(500), nrow = 100, ncol = 10)
  row_data <- data.frame(id = 1:100)
  col_data <- data.frame(sample = 1:10)
  tm <- tidymatrix(mat, row_data, col_data)

  set.seed(42)
  result <- tm |>
    activate(columns) |>
    compute_tsne(dims = 2, perplexity = 3, check_duplicates = FALSE)

  expect_true("column_tsne_1" %in% names(result$col_data))
  expect_true("column_tsne_2" %in% names(result$col_data))
})

test_that("compute_tsne dims parameter controls dimensions", {
  skip_if_not_installed("Rtsne")

  mat <- matrix(rnorm(250), nrow = 50, ncol = 5)
  row_data <- data.frame(id = 1:50)
  tm <- tidymatrix(mat, row_data)

  set.seed(42)
  result <- tm |>
    activate(rows) |>
    compute_tsne(dims = 3, perplexity = 10)

  expect_true("row_tsne_1" %in% names(result$row_data))
  expect_true("row_tsne_2" %in% names(result$row_data))
  expect_true("row_tsne_3" %in% names(result$row_data))
})

test_that("compute_tsne with same seed gives same results", {
  skip_if_not_installed("Rtsne")

  mat <- matrix(rnorm(250), nrow = 50, ncol = 5)
  row_data <- data.frame(id = 1:50)
  tm <- tidymatrix(mat, row_data)

  set.seed(42)
  result1 <- tm |> activate(rows) |> compute_tsne(dims = 2, perplexity = 10, store = FALSE)

  set.seed(42)
  result2 <- tm |> activate(rows) |> compute_tsne(dims = 2, perplexity = 10, store = FALSE)

  expect_equal(result1$row_data$row_tsne_1, result2$row_data$row_tsne_1, tolerance = 1e-10)
  expect_equal(result1$row_data$row_tsne_2, result2$row_data$row_tsne_2, tolerance = 1e-10)
})

test_that("compute_tsne store=FALSE doesn't store analysis", {
  skip_if_not_installed("Rtsne")

  mat <- matrix(rnorm(250), nrow = 50, ncol = 5)
  row_data <- data.frame(id = 1:50)
  tm <- tidymatrix(mat, row_data)

  set.seed(42)
  result <- tm |>
    activate(rows) |>
    compute_tsne(dims = 2, perplexity = 10, store = FALSE)

  expect_equal(length(list_analyses(result)), 0)
  expect_true("row_tsne_1" %in% names(result$row_data))
})

test_that("compute_tsne errors when matrix is active", {
  skip_if_not_installed("Rtsne")

  mat <- matrix(rnorm(250), nrow = 50, ncol = 5)
  row_data <- data.frame(id = 1:50)
  tm <- tidymatrix(mat, row_data)

  expect_error(
    compute_tsne(tm, dims = 2),
    "Cannot compute t-SNE when matrix is active"
  )
})

# UMAP tests (conditional on umap package)
test_that("compute_umap works on rows", {
  skip_if_not_installed("umap")

  mat <- matrix(rnorm(250), nrow = 50, ncol = 5)
  row_data <- data.frame(id = 1:50)
  col_data <- data.frame(sample = 1:5)
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(rows) |>
    compute_umap(n_components = 2, random_state = 42)

  # Check that columns were added
  expect_true("row_umap_1" %in% names(result$row_data))
  expect_true("row_umap_2" %in% names(result$row_data))

  # Check dimensions
  expect_equal(nrow(result$row_data), 50)
  expect_equal(ncol(result$row_data), 3)  # id + umap_1 + umap_2

  # Check that analysis was stored
  expect_true("row_umap" %in% list_analyses(result))
})

test_that("compute_umap works on columns", {
  skip_if_not_installed("umap")

  mat <- matrix(rnorm(1000), nrow = 100, ncol = 20)
  row_data <- data.frame(id = 1:100)
  col_data <- data.frame(sample = 1:20)
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm |>
    activate(columns) |>
    compute_umap(n_components = 2, n_neighbors = 10, random_state = 42)

  expect_true("column_umap_1" %in% names(result$col_data))
  expect_true("column_umap_2" %in% names(result$col_data))
})

test_that("compute_umap n_components parameter controls dimensions", {
  skip_if_not_installed("umap")

  mat <- matrix(rnorm(250), nrow = 50, ncol = 5)
  row_data <- data.frame(id = 1:50)
  tm <- tidymatrix(mat, row_data)

  result <- tm |>
    activate(rows) |>
    compute_umap(n_components = 3, random_state = 42)

  expect_true("row_umap_1" %in% names(result$row_data))
  expect_true("row_umap_2" %in% names(result$row_data))
  expect_true("row_umap_3" %in% names(result$row_data))
})

test_that("compute_umap with random_state gives reproducible results", {
  skip_if_not_installed("umap")

  mat <- matrix(rnorm(250), nrow = 50, ncol = 5)
  row_data <- data.frame(id = 1:50)
  tm <- tidymatrix(mat, row_data)

  result1 <- tm |> activate(rows) |> compute_umap(n_components = 2, random_state = 42, store = FALSE)
  result2 <- tm |> activate(rows) |> compute_umap(n_components = 2, random_state = 42, store = FALSE)

  expect_equal(result1$row_data$row_umap_1, result2$row_data$row_umap_1, tolerance = 1e-6)
  expect_equal(result1$row_data$row_umap_2, result2$row_data$row_umap_2, tolerance = 1e-6)
})

test_that("compute_umap store=FALSE doesn't store analysis", {
  skip_if_not_installed("umap")

  mat <- matrix(rnorm(250), nrow = 50, ncol = 5)
  row_data <- data.frame(id = 1:50)
  tm <- tidymatrix(mat, row_data)

  result <- tm |>
    activate(rows) |>
    compute_umap(n_components = 2, random_state = 42, store = FALSE)

  expect_equal(length(list_analyses(result)), 0)
  expect_true("row_umap_1" %in% names(result$row_data))
})

test_that("compute_umap errors when matrix is active", {
  skip_if_not_installed("umap")

  mat <- matrix(rnorm(250), nrow = 50, ncol = 5)
  row_data <- data.frame(id = 1:50)
  tm <- tidymatrix(mat, row_data)

  expect_error(
    compute_umap(tm, n_components = 2),
    "Cannot compute UMAP when matrix is active"
  )
})

test_that("compute_umap with custom parameters", {
  skip_if_not_installed("umap")

  mat <- matrix(rnorm(250), nrow = 50, ncol = 5)
  row_data <- data.frame(id = 1:50)
  tm <- tidymatrix(mat, row_data)

  result <- tm |>
    activate(rows) |>
    compute_umap(n_components = 2, n_neighbors = 10, min_dist = 0.05,
                 random_state = 42)

  expect_true("row_umap_1" %in% names(result$row_data))
  expect_true("row_umap_2" %in% names(result$row_data))
})

# Integration tests
test_that("can chain multiple dimreduction methods", {
  mat <- matrix(rnorm(250), nrow = 50, ncol = 5)
  row_data <- data.frame(id = 1:50)
  tm <- tidymatrix(mat, row_data)

  result <- tm |>
    activate(rows) |>
    compute_mds(k = 2, name = "mds1") |>
    compute_mds(k = 2, name = "mds2", dist_method = "manhattan")

  expect_true("mds1_1" %in% names(result$row_data))
  expect_true("mds2_1" %in% names(result$row_data))
  expect_true("mds1" %in% list_analyses(result))
  expect_true("mds2" %in% list_analyses(result))
})

# Test that coordinates are actually different from random
test_that("MDS produces non-random coordinates", {
  mat <- matrix(rnorm(100), nrow = 20, ncol = 5)
  row_data <- data.frame(id = 1:20)
  tm <- tidymatrix(mat, row_data)

  result <- tm |> activate(rows) |> compute_mds(k = 2)

  # MDS coordinates should have some structure (not all the same)
  expect_true(sd(result$row_data$row_mds_1) > 0)
  expect_true(sd(result$row_data$row_mds_2) > 0)
})
