library(dplyr)

# Test PCA ----

test_that("compute_prcomp adds PC columns to metadata", {
  set.seed(123)
  mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
  row_data <- data.frame(id = 1:10)
  tm <- tidymatrix(mat, row_data)

  # PCA on rows
  tm <- tm |>
    activate(rows) |>
    compute_prcomp(n_components = 3, center = TRUE)

  expect_true("row_pca_PC1" %in% names(tm$row_data))
  expect_true("row_pca_PC2" %in% names(tm$row_data))
  expect_true("row_pca_PC3" %in% names(tm$row_data))
  expect_equal(nrow(tm$row_data), 10)
})

test_that("compute_prcomp stores full prcomp object", {
  set.seed(123)
  mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
  tm <- tidymatrix(mat)

  tm <- tm |>
    activate(columns) |>
    compute_prcomp(name = "pca", center = TRUE, scale. = TRUE)

  expect_equal(list_analyses(tm), "pca")

  pca_obj <- get_analysis(tm, "pca")
  expect_s3_class(pca_obj, "prcomp")
  expect_true("sdev" %in% names(pca_obj))
  expect_true("rotation" %in% names(pca_obj))
})

test_that("compute_prcomp with custom name", {
  mat <- matrix(rnorm(50), nrow = 10, ncol = 5)
  tm <- tidymatrix(mat)

  tm <- tm |>
    activate(rows) |>
    compute_prcomp(name = "gene_pca", n_components = 2)

  expect_true("gene_pca_PC1" %in% names(tm$row_data))
  expect_true("gene_pca_PC2" %in% names(tm$row_data))
  expect_false("gene_pca_PC3" %in% names(tm$row_data))

  expect_equal(list_analyses(tm), "gene_pca")
})

test_that("compute_prcomp errors when matrix is active", {
  mat <- matrix(rnorm(50), nrow = 10, ncol = 5)
  tm <- tidymatrix(mat)

  expect_error(
    compute_prcomp(tm, center = TRUE),
    "Cannot compute PCA when matrix is active"
  )
})

# Test hierarchical clustering ----

test_that("compute_hclust adds cluster column", {
  set.seed(123)
  mat <- matrix(rnorm(50), nrow = 10, ncol = 5)
  tm <- tidymatrix(mat)

  tm <- tm |>
    activate(rows) |>
    compute_hclust(k = 3)

  expect_true("row_hclust_cluster" %in% names(tm$row_data))
  expect_s3_class(tm$row_data$row_hclust_cluster, "factor")
  expect_equal(nlevels(tm$row_data$row_hclust_cluster), 3)
})

test_that("compute_hclust stores hclust object", {
  mat <- matrix(rnorm(50), nrow = 10, ncol = 5)
  tm <- tidymatrix(mat)

  tm <- tm |>
    activate(columns) |>
    compute_hclust(k = 2, name = "sample_hc", method = "ward.D2")

  expect_equal(list_analyses(tm), "sample_hc")

  hc_obj <- get_analysis(tm, "sample_hc")
  expect_s3_class(hc_obj, "hclust")
  expect_equal(hc_obj$method, "ward.D2")
})

test_that("compute_hclust with custom name", {
  mat <- matrix(rnorm(50), nrow = 10, ncol = 5)
  tm <- tidymatrix(mat)

  tm <- tm |>
    activate(rows) |>
    compute_hclust(k = 3, name = "gene_k3") |>
    compute_hclust(k = 5, name = "gene_k5")

  expect_true("gene_k3_cluster" %in% names(tm$row_data))
  expect_true("gene_k5_cluster" %in% names(tm$row_data))

  expect_equal(nlevels(tm$row_data$gene_k3_cluster), 3)
  expect_equal(nlevels(tm$row_data$gene_k5_cluster), 5)
})

test_that("compute_hclust without k doesn't add cluster column", {
  mat <- matrix(rnorm(50), nrow = 10, ncol = 5)
  tm <- tidymatrix(mat)

  tm <- tm |>
    activate(rows) |>
    compute_hclust(name = "hc")

  expect_false("hc_cluster" %in% names(tm$row_data))
  expect_equal(list_analyses(tm), "hc")
})

# Test k-means clustering ----

test_that("compute_kmeans adds cluster column", {
  set.seed(123)
  mat <- matrix(rnorm(50), nrow = 10, ncol = 5)
  tm <- tidymatrix(mat)

  tm <- tm |>
    activate(rows) |>
    compute_kmeans(centers = 3, nstart = 10)

  expect_true("row_kmeans_cluster" %in% names(tm$row_data))
  expect_s3_class(tm$row_data$row_kmeans_cluster, "factor")
  expect_equal(nlevels(tm$row_data$row_kmeans_cluster), 3)
})

test_that("compute_kmeans stores kmeans object", {
  set.seed(123)
  mat <- matrix(rnorm(50), nrow = 10, ncol = 5)
  tm <- tidymatrix(mat)

  tm <- tm |>
    activate(columns) |>
    compute_kmeans(centers = 2, name = "km", nstart = 25)

  expect_equal(list_analyses(tm), "km")

  km_obj <- get_analysis(tm, "km")
  expect_true("cluster" %in% names(km_obj))
  expect_true("centers" %in% names(km_obj))
  expect_equal(nrow(km_obj$centers), 2)
})

# Test analysis management ----

test_that("list_analyses returns empty for new tidymatrix", {
  mat <- matrix(rnorm(50), nrow = 10, ncol = 5)
  tm <- tidymatrix(mat)

  expect_equal(list_analyses(tm), character(0))
})

test_that("list_analyses returns all analysis names", {
  mat <- matrix(rnorm(50), nrow = 10, ncol = 5)
  tm <- tidymatrix(mat)

  tm <- tm |>
    activate(rows) |>
    compute_prcomp(name = "pca1") |>
    compute_hclust(k = 3, name = "hc1") |>
    activate(columns) |>
    compute_prcomp(name = "pca2")

  analyses <- list_analyses(tm)
  expect_equal(length(analyses), 3)
  expect_true("pca1" %in% analyses)
  expect_true("hc1" %in% analyses)
  expect_true("pca2" %in% analyses)
})

test_that("get_analysis retrieves stored objects", {
  mat <- matrix(rnorm(50), nrow = 10, ncol = 5)
  tm <- tidymatrix(mat)

  tm <- tm |>
    activate(rows) |>
    compute_prcomp(name = "pca", center = TRUE)

  pca_obj <- get_analysis(tm, "pca")
  expect_s3_class(pca_obj, "prcomp")
})

test_that("get_analysis errors on missing analysis", {
  mat <- matrix(rnorm(50), nrow = 10, ncol = 5)
  tm <- tidymatrix(mat)

  expect_error(
    get_analysis(tm, "nonexistent"),
    "No analysis named 'nonexistent' found"
  )
})

test_that("remove_analysis removes single analysis", {
  mat <- matrix(rnorm(50), nrow = 10, ncol = 5)
  tm <- tidymatrix(mat)

  tm <- tm |>
    activate(rows) |>
    compute_prcomp(name = "pca1") |>
    compute_hclust(k = 3, name = "hc1")

  expect_equal(length(list_analyses(tm)), 2)

  tm <- remove_analysis(tm, "pca1")

  expect_equal(list_analyses(tm), "hc1")
  expect_error(get_analysis(tm, "pca1"), "No analysis named")
})

test_that("remove_analysis with NULL removes all", {
  mat <- matrix(rnorm(50), nrow = 10, ncol = 5)
  tm <- tidymatrix(mat)

  tm <- tm |>
    activate(rows) |>
    compute_prcomp(name = "pca1") |>
    compute_hclust(k = 3, name = "hc1")

  tm <- remove_analysis(tm, NULL)

  expect_equal(length(list_analyses(tm)), 0)
})

test_that("check_analyses shows validity", {
  mat <- matrix(rnorm(50), nrow = 10, ncol = 5)
  tm <- tidymatrix(mat)

  tm <- tm |>
    activate(rows) |>
    compute_prcomp(name = "pca")

  expect_output(check_analyses(tm), "VALID")
})

# Test analysis invalidation ----

test_that("filter removes stored analyses", {
  mat <- matrix(rnorm(50), nrow = 10, ncol = 5)
  row_data <- data.frame(id = 1:10, group = rep(c("A", "B"), each = 5))
  tm <- tidymatrix(mat, row_data)

  tm <- tm |>
    activate(rows) |>
    compute_prcomp(name = "pca")

  expect_equal(list_analyses(tm), "pca")

  expect_warning(
    tm <- tm |> activate(rows) |> filter(group == "A"),
    "Removed 1 stored analysis"
  )

  expect_equal(length(list_analyses(tm)), 0)
  # But PC columns should still be there (using name "pca" so columns are pca_PC1, etc.)
  expect_true("pca_PC1" %in% names(tm$row_data))
})

test_that("slice removes stored analyses", {
  mat <- matrix(rnorm(50), nrow = 10, ncol = 5)
  tm <- tidymatrix(mat)

  tm <- tm |>
    activate(rows) |>
    compute_hclust(k = 3, name = "hc")

  expect_equal(list_analyses(tm), "hc")

  expect_warning(
    tm <- tm |> activate(rows) |> slice(1:5),
    "Removed 1 stored analysis"
  )

  expect_equal(length(list_analyses(tm)), 0)
})

test_that("mutate doesn't remove stored analyses", {
  mat <- matrix(rnorm(50), nrow = 10, ncol = 5)
  tm <- tidymatrix(mat)

  tm <- tm |>
    activate(rows) |>
    compute_prcomp(name = "pca")

  tm <- tm |>
    activate(rows) |>
    mutate(new_col = row_number())

  expect_equal(list_analyses(tm), "pca")
})
