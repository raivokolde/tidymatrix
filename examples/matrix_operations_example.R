# Matrix Operations Example for tidymatrix
# Demonstrates scaling, centering, transforming, and other matrix operations

library(tidymatrix)
library(dplyr)

# Create example gene expression data
set.seed(42)
n_genes <- 50
n_samples <- 12

# Simulate expression matrix with structure (all positive values)
expression <- matrix(abs(rnorm(n_genes * n_samples, mean = 100, sd = 50)),
                     nrow = n_genes, ncol = n_samples)

# Add some genes with high expression
expression[1:10, ] <- expression[1:10, ] + 200

# Create metadata
gene_data <- data.frame(
  gene_id = paste0("Gene_", 1:n_genes),
  type = c(rep("housekeeping", 10), rep("regulated", 40))
)

sample_data <- data.frame(
  sample_id = paste0("Sample_", 1:n_samples),
  condition = rep(c("Control", "Treatment"), each = 6),
  batch = rep(1:3, each = 4)
)

# Create tidymatrix
expr_tm <- tidymatrix(expression, gene_data, sample_data)

cat("\\n=== Original Data ===\\n")
cat("Matrix range:", range(expr_tm$matrix), "\\n")
cat("Matrix dimensions:", dim(expr_tm$matrix), "\\n")

# =============================================================================
# Example 1: Log transformation
# =============================================================================

cat("\\n=== Example 1: Log Transformation ===\\n")

expr_log <- expr_tm |>
  activate(matrix) |>
  log_transform(base = 2, offset = 1)

cat("After log2(x + 1):\\n")
cat("  Range:", range(expr_log$matrix), "\\n")
cat("  Mean:", mean(expr_log$matrix), "\\n")

# =============================================================================
# Example 2: Row scaling (z-score per gene)
# =============================================================================

cat("\\n=== Example 2: Row Scaling (Z-score per gene) ===\\n")

expr_scaled <- expr_tm |>
  activate(matrix) |>
  log_transform(base = 2, offset = 1) |>
  activate(rows) |>
  scale()

# Check that each gene has mean=0, sd=1
row_means <- rowMeans(expr_scaled$matrix)
row_sds <- apply(expr_scaled$matrix, 1, sd)

cat("After row scaling:\\n")
cat("  Row means (should be ~0):", range(row_means), "\\n")
cat("  Row SDs (should be ~1):", range(row_sds), "\\n")

# =============================================================================
# Example 3: Column centering
# =============================================================================

cat("\\n=== Example 3: Column Centering (center each sample) ===\\n")

expr_centered <- expr_tm |>
  activate(columns) |>
  center()

col_means <- colMeans(expr_centered$matrix)
cat("Column means after centering:", range(col_means), "\\n")

# =============================================================================
# Example 4: Adding statistics and filtering
# =============================================================================

cat("\\n=== Example 4: Add Statistics and Filter ===\\n")

expr_filtered <- expr_tm |>
  activate(rows) |>
  add_stats(mean, var, sd) |>
  filter(var > quantile(var, 0.75))  # Keep top 25% most variable genes

cat("Original genes:", nrow(expr_tm$matrix), "\\n")
cat("After filtering for high variance:", nrow(expr_filtered$matrix), "\\n")
cat("Variance range:", range(expr_filtered$row_data$var), "\\n")

# =============================================================================
# Example 5: Transpose
# =============================================================================

cat("\\n=== Example 5: Transpose (genes x samples -> samples x genes) ===\\n")

expr_transposed <- expr_tm |>
  transpose()

cat("Original dimensions (genes x samples):", dim(expr_tm$matrix), "\\n")
cat("After transpose (samples x genes):", dim(expr_transposed$matrix), "\\n")
cat("Row metadata swapped:", "sample_id" %in% names(expr_transposed$row_data), "\\n")
cat("Column metadata swapped:", "gene_id" %in% names(expr_transposed$col_data), "\\n")

# =============================================================================
# Example 6: Custom transformations
# =============================================================================

cat("\\n=== Example 6: Custom Transformations ===\\n")

# Clip outliers
expr_clipped <- expr_tm |>
  activate(matrix) |>
  clip_values(min = 0, max = 300)

cat("Original range:", range(expr_tm$matrix), "\\n")
cat("After clipping to [0, 300]:", range(expr_clipped$matrix), "\\n")

# Custom transformation with transform()
expr_sqrt <- expr_tm |>
  activate(matrix) |>
  transform(sqrt(abs(.)))

cat("\\nAfter sqrt(abs(x)):", range(expr_sqrt$matrix), "\\n")

# =============================================================================
# Example 7: Complex preprocessing pipeline
# =============================================================================

cat("\\n=== Example 7: Complete Preprocessing Pipeline ===\\n")

expr_processed <- expr_tm |>
  # Filter low expression genes
  activate(rows) |>
  add_stats(mean) |>
  filter(mean > median(mean)) |>

  # Log transform
  activate(matrix) |>
  log_transform(base = 2, offset = 1) |>

  # Center and scale rows
  activate(rows) |>
  center() |>
  scale() |>

  # Add more statistics
  add_stats(var, sd, .names = c("variance", "std_dev")) |>

  # PCA on samples
  activate(columns) |>
  compute_prcomp(name = "samples", center = TRUE) |>

  # Cluster genes
  activate(rows) |>
  compute_hclust(k = 3, name = "gene_clusters", method = "ward.D2")

cat("Processing steps completed:\\n")
cat("  Final dimensions:", dim(expr_processed$matrix), "\\n")
cat("  Genes kept:", nrow(expr_processed$matrix), "\\n")
cat("  Row metadata columns:", paste(names(expr_processed$row_data), collapse = ", "), "\\n")
cat("  Column metadata columns:", paste(names(expr_processed$col_data), collapse = ", "), "\\n")
cat("  Stored analyses:", paste(list_analyses(expr_processed), collapse = ", "), "\\n")

# =============================================================================
# Example 8: Sample-wise normalization (library size)
# =============================================================================

cat("\\n=== Example 8: Library Size Normalization ===\\n")

# Calculate library sizes (column sums)
expr_norm <- expr_tm |>
  activate(columns) |>
  add_stats(sum, .names = "library_size")

cat("Library sizes (column sums):\\n")
print(expr_norm$col_data$library_size)

# Normalize by dividing each column by its sum and multiplying by mean
mean_lib_size <- mean(expr_norm$col_data$library_size)

# Can't do this directly with current functions, but shows the workflow
# For now, demonstrate the statistics are available
cat("\\nMean library size:", mean_lib_size, "\\n")
cat("These stats can be used for custom normalization\\n")

# =============================================================================
# Example 9: Batch effect visualization prep
# =============================================================================

cat("\\n=== Example 9: Prepare Data for Batch Effect Visualization ===\\n")

batch_analysis <- expr_tm |>
  activate(matrix) |>
  log_transform(base = 2, offset = 1) |>
  activate(columns) |>
  compute_prcomp(name = "pca_batch", center = TRUE, scale. = TRUE)

# Check if PC1 correlates with batch
cat("PC scores by batch:\\n")
pc_by_batch <- batch_analysis$col_data |>
  group_by(batch) |>
  summarise(
    mean_PC1 = mean(pca_batch_PC1),
    mean_PC2 = mean(pca_batch_PC2)
  )
print(pc_by_batch)

cat("\\n=== Matrix Operations Examples Complete ===\\n")
