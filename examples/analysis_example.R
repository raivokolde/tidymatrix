# Analysis Integration Example for tidymatrix
# This example demonstrates how to perform PCA and clustering on tidymatrix objects

library(tidymatrix)
library(dplyr)

# Create example gene expression data
set.seed(42)
n_genes <- 100
n_samples <- 20

# Simulate expression matrix
expression <- matrix(rnorm(n_genes * n_samples), nrow = n_genes, ncol = n_samples)

# Add some structure: genes in groups, samples in conditions
expression[1:30, 1:10] <- expression[1:30, 1:10] + 2  # Group 1 genes high in condition A
expression[31:60, 11:20] <- expression[31:60, 11:20] + 2  # Group 2 genes high in condition B

# Create metadata
gene_data <- data.frame(
  gene_id = paste0("Gene_", 1:n_genes),
  chromosome = sample(paste0("chr", 1:5), n_genes, replace = TRUE)
)

sample_data <- data.frame(
  sample_id = paste0("Sample_", 1:n_samples),
  condition = rep(c("Control", "Treatment"), each = 10),
  batch = rep(1:2, 10)
)

# Create tidymatrix
expr_tm <- tidymatrix(expression, gene_data, sample_data)

# =============================================================================
# Example 1: PCA on samples
# =============================================================================

cat("\\n=== Example 1: PCA on samples ===\\n")

expr_tm <- expr_tm |>
  activate(columns) |>
  compute_prcomp(name = "sample_pca", center = TRUE, scale. = TRUE)

# Check what columns were added
cat("Columns added to sample metadata:\\n")
print(names(expr_tm$col_data))

# Get the full PCA object
pca_obj <- get_analysis(expr_tm, "sample_pca")
cat("\\nVariance explained by first 3 PCs:\\n")
var_explained <- pca_obj$sdev[1:3]^2 / sum(pca_obj$sdev^2) * 100
print(round(var_explained, 2))

# Use PC scores in downstream analysis
cat("\\nSamples with high PC1 scores:\\n")
high_pc1 <- expr_tm |>
  activate(columns) |>
  filter(sample_pca_PC1 > median(sample_pca_PC1)) |>
  pull(sample_id)
print(high_pc1)

# =============================================================================
# Example 2: Clustering genes
# =============================================================================

cat("\\n=== Example 2: Hierarchical clustering of genes ===\\n")

expr_tm <- expr_tm |>
  activate(rows) |>
  compute_hclust(k = 3, name = "gene_clusters", method = "ward.D2")

# Check cluster assignments
cat("Genes per cluster:\\n")
cluster_counts <- expr_tm |>
  activate(rows) |>
  count(gene_clusters_cluster)
print(cluster_counts$row_data)

# Get the dendrogram
hc_obj <- get_analysis(expr_tm, "gene_clusters")
cat("\\nDendrogram available for plotting\\n")
cat("Height:", round(max(hc_obj$height), 2), "\\n")

# =============================================================================
# Example 3: Multiple analyses with different parameters
# =============================================================================

cat("\\n=== Example 3: Multiple clusterings ===\\n")

expr_tm <- expr_tm |>
  activate(rows) |>
  compute_hclust(k = 5, name = "gene_k5") |>
  compute_kmeans(centers = 4, name = "gene_km4", nstart = 25)

# List all stored analyses
cat("Stored analyses:\\n")
print(list_analyses(expr_tm))

# Compare different clustering results
cat("\\nClustering results comparison:\\n")
comparison <- expr_tm |>
  activate(rows) |>
  count(gene_clusters_cluster, gene_k5_cluster)
print(head(comparison$row_data, 10))

# =============================================================================
# Example 4: Analysis invalidation on data modification
# =============================================================================

cat("\\n=== Example 4: Analysis invalidation ===\\n")

cat("Analyses before filtering:\\n")
print(list_analyses(expr_tm))

cat("\\nNumber of rows before filter:", nrow(expr_tm$matrix), "\\n")

# Filter to only genes with high variance
expr_tm_filtered <- expr_tm |>
  activate(rows) |>
  mutate(gene_var = apply(expr_tm$matrix, 1, var)) |>
  filter(gene_var > median(gene_var))

cat("Number of rows after filter:", nrow(expr_tm_filtered$matrix), "\\n")

cat("\\nAnalyses after filtering:\\n")
print(list_analyses(expr_tm_filtered))

cat("\\nNote: Stored analysis objects were removed, but metadata columns are preserved\\n")
cat("Cluster columns still present:",
    "gene_clusters_cluster" %in% names(expr_tm_filtered$row_data), "\\n")

# =============================================================================
# Example 5: Complete analysis workflow
# =============================================================================

cat("\\n=== Example 5: Complete analysis workflow ===\\n")

# Start fresh
workflow_tm <- tidymatrix(expression, gene_data, sample_data) |>
  # PCA on samples
  activate(columns) |>
  compute_prcomp(name = "samples", center = TRUE, scale. = TRUE) |>
  # Cluster samples
  compute_hclust(k = 2, name = "sample_groups") |>
  # Cluster genes
  activate(rows) |>
  compute_hclust(k = 5, name = "gene_modules", method = "ward.D2") |>
  # Add variance as a feature
  mutate(variance = apply(expression, 1, var))

# Now we can do complex queries
cat("\\nGenes in module 1 with high variance:\\n")
result <- workflow_tm |>
  activate(rows) |>
  filter(gene_modules_cluster == 1, variance > quantile(variance, 0.75))

cat("Found", nrow(result$matrix), "genes\\n")

# Check which samples are in each group
cat("\\nSample grouping:\\n")
sample_groups <- workflow_tm |>
  activate(columns) |>
  count(sample_groups_cluster, condition)
print(sample_groups$col_data)

cat("\\n=== Analysis integration examples complete ===\\n")
