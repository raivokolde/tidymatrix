# Example: Using tidymatrix with ggplot2
# This demonstrates how to extract PCA results and pipe them directly to ggplot

library(tidymatrix)
library(dplyr)
library(ggplot2)

# Create example gene expression data
set.seed(123)
n_genes <- 100
n_samples <- 20

# Simulate expression matrix with some structure
expression <- matrix(rnorm(n_genes * n_samples), nrow = n_genes, ncol = n_samples)
expression[1:30, 1:10] <- expression[1:30, 1:10] + 2  # Group 1 genes high in condition A
expression[31:60, 11:20] <- expression[31:60, 11:20] + 2  # Group 2 genes high in condition B

# Create metadata
gene_data <- data.frame(
  gene_id = paste0("Gene_", 1:n_genes),
  group = sample(c("GroupA", "GroupB", "GroupC"), n_genes, replace = TRUE)
)

sample_data <- data.frame(
  sample_id = paste0("Sample_", 1:n_samples),
  condition = rep(c("Control", "Treatment"), each = 10),
  batch = rep(1:2, 10)
)

# Create tidymatrix
expr_tm <- tidymatrix(expression, gene_data, sample_data)

# =============================================================================
# Example 1: PCA plot using pull_active()
# =============================================================================

# pull_active() works on any active component
expr_tm |>
  activate(columns) |>
  compute_prcomp(center = TRUE, scale. = TRUE) |>
  pull_active() |>
  ggplot(aes(x = column_pca_PC1, y = column_pca_PC2, color = condition, shape = factor(batch))) +
    geom_point(size = 3) +
    labs(title = "PCA of Samples",
         subtitle = "Using pull_active() to extract column metadata") +
    theme_minimal()

# =============================================================================
# Example 2: PCA plot using as_tibble()
# =============================================================================

# as_tibble() only works on metadata (rows/columns), not on matrix
expr_tm |>
  activate(columns) |>
  compute_prcomp(center = TRUE, scale. = TRUE) |>
  as_tibble() |>
  ggplot(aes(x = column_pca_PC1, y = column_pca_PC2, color = condition)) +
    geom_point(size = 3) +
    geom_text(aes(label = sample_id), hjust = -0.2, size = 3) +
    labs(title = "PCA of Samples with Labels",
         subtitle = "Using as_tibble() for cleaner tibble output") +
    theme_minimal()

# =============================================================================
# Example 3: Complex workflow with filtering and plotting
# =============================================================================

# Filter high-variance genes, compute PCA, and plot
expr_tm |>
  activate(rows) |>
  mutate(variance = apply(expression, 1, var)) |>
  filter(variance > quantile(variance, 0.75)) |>  # Top 25% most variable genes
  compute_prcomp(n_components = 3, center = TRUE, scale. = FALSE) |>
  as_tibble() |>
  ggplot(aes(x = row_pca_PC1, y = row_pca_PC2, color = group)) +
    geom_point(alpha = 0.6) +
    labs(title = "PCA of High-Variance Genes",
         subtitle = "Filtered to top 25% most variable genes") +
    theme_minimal()

# =============================================================================
# Example 4: Using with facets
# =============================================================================

expr_tm |>
  activate(columns) |>
  compute_prcomp(center = TRUE, scale. = TRUE) |>
  as_tibble() |>
  ggplot(aes(x = column_pca_PC1, y = column_pca_PC2, color = condition)) +
    geom_point(size = 3) +
    facet_wrap(~ batch) +
    labs(title = "PCA by Batch",
         subtitle = "Faceted plot showing batch effects") +
    theme_minimal()

# =============================================================================
# Example 5: pull_active() can extract matrix too
# =============================================================================

# Extract the matrix for correlation heatmap
cor_matrix <- expr_tm |>
  activate(matrix) |>
  pull_active() |>
  cor()

# Or extract filtered/transformed matrix
high_var_matrix <- expr_tm |>
  activate(rows) |>
  mutate(variance = apply(expression, 1, var)) |>
  filter(variance > quantile(variance, 0.9)) |>
  activate(matrix) |>
  pull_active()

print(paste("Original matrix dimensions:", paste(dim(expression), collapse = " x ")))
print(paste("High variance matrix dimensions:", paste(dim(high_var_matrix), collapse = " x ")))

# =============================================================================
# Example 6: Combining multiple analyses
# =============================================================================

# Add clustering and plot clusters in PCA space
expr_tm |>
  activate(rows) |>
  compute_prcomp(n_components = 2, center = TRUE, scale. = TRUE) |>
  compute_kmeans(centers = 3, name = "gene_cluster") |>
  as_tibble() |>
  ggplot(aes(x = row_pca_PC1, y = row_pca_PC2, color = factor(gene_cluster_cluster))) +
    geom_point(alpha = 0.6) +
    labs(title = "Gene Clusters in PCA Space",
         color = "Cluster") +
    theme_minimal()

# =============================================================================
# Example 7: Using as.data.frame() for base R plotting
# =============================================================================

pca_data <- expr_tm |>
  activate(columns) |>
  compute_prcomp(center = TRUE, scale. = TRUE) |>
  as.data.frame()  # Plain data.frame for base R

# Base R plotting
plot(pca_data$column_pca_PC1, pca_data$column_pca_PC2,
     col = as.factor(pca_data$condition),
     pch = 19,
     xlab = "PC1", ylab = "PC2",
     main = "PCA using base R")
legend("topright", legend = levels(as.factor(pca_data$condition)),
       col = 1:2, pch = 19)

cat("\n=== Examples complete ===\n")
