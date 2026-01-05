library(dplyr)

# Skip all tests if pheatmap is not installed
skip_if_not_installed("pheatmap")

# Test basic pheatmap ----

test_that("plot_pheatmap works with basic inputs", {
  mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
  row_data <- data.frame(
    gene = paste0("Gene_", 1:10),
    type = rep(c("A", "B"), each = 5)
  )
  col_data <- data.frame(
    sample = paste0("Sample_", 1:10),
    condition = rep(c("Control", "Treatment"), 5)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  # Should create pheatmap without error
  p <- plot_pheatmap(tm, row_names = "gene", col_names = "sample")

  expect_s3_class(p, "pheatmap")
})

test_that("plot_pheatmap works without names", {
  mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
  tm <- tidymatrix(mat)

  # Should work with NULL names
  p <- plot_pheatmap(tm)

  expect_s3_class(p, "pheatmap")
})

test_that("plot_pheatmap with row annotations only", {
  mat <- matrix(rnorm(60), nrow = 6, ncol = 10)
  row_data <- data.frame(
    gene = paste0("G", 1:6),
    group = rep(c("A", "B", "C"), each = 2)
  )
  tm <- tidymatrix(mat, row_data)

  p <- plot_pheatmap(tm, row_names = "gene")

  expect_s3_class(p, "pheatmap")
})

test_that("plot_pheatmap with column annotations only", {
  mat <- matrix(rnorm(60), nrow = 10, ncol = 6)
  col_data <- data.frame(
    sample = paste0("S", 1:6),
    treatment = rep(c("Ctrl", "Drug"), 3)
  )
  tm <- tidymatrix(mat, col_data = col_data)

  p <- plot_pheatmap(tm, col_names = "sample")

  expect_s3_class(p, "pheatmap")
})

test_that("plot_pheatmap excludes name columns from annotations", {
  mat <- matrix(rnorm(40), nrow = 4, ncol = 10)
  row_data <- data.frame(
    gene_id = paste0("Gene_", 1:4),
    type = c("A", "A", "B", "B"),
    score = rnorm(4)
  )
  col_data <- data.frame(
    sample_id = paste0("Sample_", 1:10),
    group = rep(c("X", "Y"), 5)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  # Should run without error
  p <- plot_pheatmap(tm, row_names = "gene_id", col_names = "sample_id")

  expect_s3_class(p, "pheatmap")
})

test_that("plot_pheatmap with custom annotation columns", {
  mat <- matrix(rnorm(40), nrow = 4, ncol = 10)
  row_data <- data.frame(
    gene_id = paste0("Gene_", 1:4),
    type = c("A", "A", "B", "B"),
    pathway = c("P1", "P2", "P1", "P2"),
    score = rnorm(4)
  )
  tm <- tidymatrix(mat, row_data)

  # Only include specific columns
  p <- plot_pheatmap(tm,
    row_names = "gene_id",
    row_annotation = c("type", "pathway")
  )

  expect_s3_class(p, "pheatmap")
})

test_that("plot_pheatmap with no annotations", {
  mat <- matrix(rnorm(40), nrow = 4, ncol = 10)
  row_data <- data.frame(gene = paste0("G", 1:4), type = c("A", "A", "B", "B"))
  tm <- tidymatrix(mat, row_data)

  p <- plot_pheatmap(tm,
    row_names = "gene",
    row_annotation = FALSE,
    col_annotation = FALSE
  )

  expect_s3_class(p, "pheatmap")
})

test_that("plot_pheatmap with no clustering", {
  mat <- matrix(rnorm(40), nrow = 4, ncol = 10)
  tm <- tidymatrix(mat)

  p <- plot_pheatmap(tm,
    row_cluster = FALSE,
    col_cluster = FALSE
  )

  expect_s3_class(p, "pheatmap")
})

# Test with stored clustering ----

test_that("plot_pheatmap uses stored hclust for rows", {
  set.seed(123)
  mat <- matrix(rnorm(40), nrow = 4, ncol = 10)
  row_data <- data.frame(gene = paste0("Gene_", 1:4))
  tm <- tidymatrix(mat, row_data)

  tm <- tm |>
    activate(rows) |>
    compute_hclust(name = "row_clusters", method = "complete")

  p <- plot_pheatmap(tm,
    row_names = "gene",
    row_cluster = "row_clusters",
    col_cluster = FALSE
  )

  expect_s3_class(p, "pheatmap")
})

test_that("plot_pheatmap uses stored hclust for columns", {
  set.seed(123)
  mat <- matrix(rnorm(40), nrow = 10, ncol = 4)
  col_data <- data.frame(sample = paste0("Sample_", 1:4))
  tm <- tidymatrix(mat, col_data = col_data)

  tm <- tm |>
    activate(columns) |>
    compute_hclust(name = "col_clusters", method = "ward.D2")

  p <- plot_pheatmap(tm,
    col_names = "sample",
    row_cluster = FALSE,
    col_cluster = "col_clusters"
  )

  expect_s3_class(p, "pheatmap")
})

test_that("plot_pheatmap uses stored hclust for both dimensions", {
  set.seed(123)
  mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
  row_data <- data.frame(gene = paste0("G", 1:10))
  col_data <- data.frame(sample = paste0("S", 1:10))
  tm <- tidymatrix(mat, row_data, col_data)

  tm <- tm |>
    activate(rows) |>
    compute_hclust(name = "genes") |>
    activate(columns) |>
    compute_hclust(name = "samples")

  p <- plot_pheatmap(tm,
    row_names = "gene",
    col_names = "sample",
    row_cluster = "genes",
    col_cluster = "samples"
  )

  expect_s3_class(p, "pheatmap")
})

# Test error handling ----

test_that("plot_pheatmap errors with invalid row_names", {
  mat <- matrix(rnorm(40), nrow = 4, ncol = 10)
  row_data <- data.frame(gene = paste0("Gene_", 1:4))
  tm <- tidymatrix(mat, row_data)

  expect_error(
    plot_pheatmap(tm, row_names = "invalid_column"),
    "row_names 'invalid_column' not found"
  )
})

test_that("plot_pheatmap errors with invalid col_names", {
  mat <- matrix(rnorm(40), nrow = 4, ncol = 10)
  col_data <- data.frame(sample = paste0("S", 1:10))
  tm <- tidymatrix(mat, col_data = col_data)

  expect_error(
    plot_pheatmap(tm, col_names = "nonexistent"),
    "col_names 'nonexistent' not found"
  )
})

test_that("plot_pheatmap errors with invalid cluster name", {
  mat <- matrix(rnorm(40), nrow = 4, ncol = 10)
  tm <- tidymatrix(mat)

  expect_error(
    plot_pheatmap(tm, row_cluster = "nonexistent"),
    "Row cluster 'nonexistent' not found"
  )
})

test_that("plot_pheatmap errors when cluster is not hclust", {
  mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
  tm <- tidymatrix(mat)

  # Store a PCA (not hclust)
  tm <- tm |>
    activate(rows) |>
    compute_prcomp(name = "pca")

  expect_error(
    plot_pheatmap(tm, row_cluster = "pca"),
    "is not an hclust object"
  )
})

# Test integration with other functions ----

test_that("plot_pheatmap works in pipeline", {
  mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
  row_data <- data.frame(
    gene = paste0("Gene_", 1:10),
    type = rep(c("A", "B"), each = 5)
  )
  col_data <- data.frame(
    sample = paste0("Sample_", 1:10),
    condition = rep(c("Ctrl", "Treat"), 5)
  )
  tm <- tidymatrix(mat, row_data, col_data)

  p <- tm |>
    activate(rows) |>
    scale() |>
    compute_hclust(k = 2, name = "genes") |>
    activate(columns) |>
    compute_hclust(k = 2, name = "samples") |>
    plot_pheatmap(
      row_names = "gene",
      col_names = "sample",
      row_cluster = "genes",
      col_cluster = "samples"
    )

  expect_s3_class(p, "pheatmap")
})
