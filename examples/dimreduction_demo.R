# Dimensionality Reduction Demo for tidymatrix
# This script demonstrates t-SNE, UMAP, and MDS for dimensionality reduction

library(tidymatrix)
library(dplyr)
library(ggplot2)

# Create example gene expression data with structure
set.seed(123)
n_genes <- 200
n_samples <- 50

# Create structured data with 3 groups
group1_samples <- 15
group2_samples <- 20
group3_samples <- 15

expr_matrix <- cbind(
  matrix(rnorm(n_genes * group1_samples, mean = 10, sd = 2), nrow = n_genes),
  matrix(rnorm(n_genes * group2_samples, mean = 12, sd = 2), nrow = n_genes),
  matrix(rnorm(n_genes * group3_samples, mean = 8, sd = 2), nrow = n_genes)
)

# Add some genes that differ between groups
for (i in 1:20) {
  expr_matrix[i, 1:group1_samples] <- expr_matrix[i, 1:group1_samples] + 5
}
for (i in 21:40) {
  expr_matrix[i, (group1_samples + 1):(group1_samples + group2_samples)] <-
    expr_matrix[i, (group1_samples + 1):(group1_samples + group2_samples)] + 5
}

# Row metadata (genes)
gene_data <- data.frame(
  gene_id = paste0("Gene_", 1:n_genes),
  category = sample(c("Housekeeping", "Regulated", "Marker"), n_genes, replace = TRUE)
)

# Column metadata (samples)
sample_data <- data.frame(
  sample_id = paste0("Sample_", 1:n_samples),
  group = factor(c(
    rep("Group1", group1_samples),
    rep("Group2", group2_samples),
    rep("Group3", group3_samples)
  )),
  batch = factor(sample(c("BatchA", "BatchB"), n_samples, replace = TRUE))
)

# Create tidymatrix
tm <- tidymatrix(expr_matrix, gene_data, sample_data)

cat("Original tidymatrix:\n")
print(tm)

# =============================================================================
# Example 1: MDS - Classical Multidimensional Scaling
# =============================================================================

cat("\n=== Example 1: MDS ===\n")

tm_mds <- tm |>
  activate(columns) |>
  compute_mds(k = 2, name = "sample_mds")

cat("MDS coordinates added to sample metadata:\n")
print(head(tm_mds$col_data))

# Visualize MDS
p1 <- ggplot(tm_mds$col_data, aes(x = sample_mds_1, y = sample_mds_2, color = group)) +
  geom_point(size = 3, alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "MDS: Sample Similarity",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2",
    color = "Group"
  )

print(p1)

# =============================================================================
# Example 2: t-SNE - t-distributed Stochastic Neighbor Embedding
# =============================================================================

cat("\n=== Example 2: t-SNE ===\n")

set.seed(42)  # For reproducibility
tm_tsne <- tm |>
  activate(columns) |>
  compute_tsne(
    dims = 2,
    name = "sample_tsne",
    perplexity = 15,  # Typical: between 5 and 50
    max_iter = 1000,
    verbose = FALSE
  )

cat("t-SNE coordinates added to sample metadata:\n")
print(head(tm_tsne$col_data))

# Visualize t-SNE
p2 <- ggplot(tm_tsne$col_data, aes(x = sample_tsne_1, y = sample_tsne_2, color = group)) +
  geom_point(size = 3, alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "t-SNE: Sample Clusters",
    x = "t-SNE Dimension 1",
    y = "t-SNE Dimension 2",
    color = "Group"
  )

print(p2)

# =============================================================================
# Example 3: UMAP - Uniform Manifold Approximation and Projection
# =============================================================================

cat("\n=== Example 3: UMAP ===\n")

tm_umap <- tm |>
  activate(columns) |>
  compute_umap(
    n_components = 2,
    name = "sample_umap",
    n_neighbors = 15,     # Size of local neighborhood
    min_dist = 0.1,       # Minimum distance between points
    random_state = 42     # For reproducibility
  )

cat("UMAP coordinates added to sample metadata:\n")
print(head(tm_umap$col_data))

# Visualize UMAP
p3 <- ggplot(tm_umap$col_data, aes(x = sample_umap_1, y = sample_umap_2, color = group)) +
  geom_point(size = 3, alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "UMAP: Sample Embedding",
    x = "UMAP Dimension 1",
    y = "UMAP Dimension 2",
    color = "Group"
  )

print(p3)

# =============================================================================
# Example 4: Compare all three methods
# =============================================================================

cat("\n=== Example 4: Compare all methods ===\n")

# Combine all results
tm_all <- tm |>
  activate(columns) |>
  compute_mds(k = 2, name = "mds") |>
  compute_tsne(dims = 2, name = "tsne", perplexity = 15, verbose = FALSE) |>
  compute_umap(n_components = 2, name = "umap", random_state = 42)

cat("All methods applied:\n")
print(names(tm_all$col_data))

# Create combined plot
library(tidyr)

plot_data <- tm_all$col_data |>
  pivot_longer(
    cols = c(mds_1, mds_2, tsne_1, tsne_2, umap_1, umap_2),
    names_to = c("method", "dimension"),
    names_pattern = "(.*)_(\\d+)",
    values_to = "value"
  ) |>
  pivot_wider(
    names_from = dimension,
    values_from = value,
    names_prefix = "dim"
  )

p4 <- ggplot(plot_data, aes(x = dim1, y = dim2, color = group)) +
  geom_point(size = 2, alpha = 0.7) +
  facet_wrap(~method, scales = "free") +
  theme_minimal() +
  labs(
    title = "Comparison of Dimensionality Reduction Methods",
    x = "Dimension 1",
    y = "Dimension 2",
    color = "Group"
  )

print(p4)

# =============================================================================
# Example 5: Gene-level dimensionality reduction
# =============================================================================

cat("\n=== Example 5: Gene-level analysis ===\n")

# Apply PCA and t-SNE on genes (rows)
set.seed(42)
tm_genes <- tm |>
  activate(rows) |>
  compute_prcomp(n_components = 2, center = TRUE, scale. = TRUE) |>
  compute_tsne(dims = 2, perplexity = 30, verbose = FALSE)

cat("Genes with dimensionality reduction:\n")
print(head(tm_genes$row_data))

# Visualize genes by category
p5 <- ggplot(tm_genes$row_data, aes(x = row_tsne_1, y = row_tsne_2, color = category)) +
  geom_point(size = 2, alpha = 0.6) +
  theme_minimal() +
  labs(
    title = "t-SNE: Gene Similarity",
    x = "t-SNE Dimension 1",
    y = "t-SNE Dimension 2",
    color = "Category"
  )

print(p5)

# =============================================================================
# Example 6: Parameter tuning for t-SNE
# =============================================================================

cat("\n=== Example 6: t-SNE parameter effects ===\n")

set.seed(42)
tm_tsne_low_perp <- tm |>
  activate(columns) |>
  compute_tsne(dims = 2, name = "tsne_low", perplexity = 5, verbose = FALSE)

set.seed(42)
tm_tsne_high_perp <- tm |>
  activate(columns) |>
  compute_tsne(dims = 2, name = "tsne_high", perplexity = 30, verbose = FALSE)

# Compare
perp_data <- bind_rows(
  tm_tsne_low_perp$col_data |>
    mutate(perplexity = "Low (5)") |>
    rename(dim1 = tsne_low_1, dim2 = tsne_low_2),
  tm_tsne_high_perp$col_data |>
    mutate(perplexity = "High (30)") |>
    rename(dim1 = tsne_high_1, dim2 = tsne_high_2)
)

p6 <- ggplot(perp_data, aes(x = dim1, y = dim2, color = group)) +
  geom_point(size = 2, alpha = 0.7) +
  facet_wrap(~perplexity, scales = "free") +
  theme_minimal() +
  labs(
    title = "t-SNE: Effect of Perplexity Parameter",
    subtitle = "Lower perplexity emphasizes local structure, higher emphasizes global",
    x = "t-SNE Dimension 1",
    y = "t-SNE Dimension 2",
    color = "Group"
  )

print(p6)

# =============================================================================
# Example 7: Retrieving stored analysis objects
# =============================================================================

cat("\n=== Example 7: Accessing stored analysis objects ===\n")

# Get the stored UMAP object
umap_obj <- get_analysis(tm_umap, "sample_umap")
cat("UMAP object structure:\n")
print(str(umap_obj))

# Get the stored t-SNE object
tsne_obj <- get_analysis(tm_tsne, "sample_tsne")
cat("\nt-SNE object - iterations run:", tsne_obj$itercosts, "\n")

# Get MDS object (contains distance matrix and result)
mds_obj <- get_analysis(tm_mds, "sample_mds")
cat("\nMDS object components:", names(mds_obj), "\n")

# =============================================================================
# Example 8: Chaining with dplyr operations
# =============================================================================

cat("\n=== Example 8: Integration with dplyr ===\n")

# Filter samples, then apply dimensionality reduction
tm_filtered <- tm |>
  activate(columns) |>
  filter(group != "Group3") |>
  compute_umap(n_components = 2, random_state = 42)

cat("Filtered to groups 1 and 2, then UMAP:\n")
cat("Samples:", nrow(tm_filtered$col_data), "\n")

# =============================================================================
# Summary
# =============================================================================

cat("\n=== Method Comparison Summary ===\n")
cat("
MDS (Classical Multidimensional Scaling):
  - Deterministic (always same result)
  - Fast
  - Linear method
  - Good for preserving overall distances
  - No external dependencies (uses base R)

t-SNE (t-distributed Stochastic Neighbor Embedding):
  - Stochastic (use set.seed() for reproducibility)
  - Can be slow for large datasets
  - Non-linear
  - Excellent for revealing local structure/clusters
  - Good for visualization
  - Perplexity parameter: 5-50 typical

UMAP (Uniform Manifold Approximation and Projection):
  - Generally faster than t-SNE
  - Better preserves global structure
  - Non-linear
  - Good balance of local and global structure
  - More deterministic than t-SNE (with random_state)
  - n_neighbors parameter: controls local vs global emphasis
")

cat("\n=== All examples completed successfully! ===\n")
