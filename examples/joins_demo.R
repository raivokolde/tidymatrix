# Join Operations Demo for tidymatrix
# This script demonstrates the use of all 6 join types with tidymatrix

library(tidymatrix)
library(dplyr)

# Create example gene expression data
set.seed(123)
expr_matrix <- matrix(rnorm(50), nrow = 10, ncol = 5)

# Row metadata (genes)
gene_data <- data.frame(
  gene_id = paste0("Gene_", 1:10),
  pathway = sample(c("Metabolism", "Signaling"), 10, replace = TRUE)
)

# Column metadata (samples)
sample_data <- data.frame(
  sample_id = paste0("Sample_", 1:5),
  condition = rep(c("Control", "Treatment"), c(2, 3))
)

# Create tidymatrix
tm <- tidymatrix(expr_matrix, gene_data, sample_data)

print("Original tidymatrix:")
print(tm)

# =============================================================================
# Example 1: left_join - Add gene annotations (keep all genes)
# =============================================================================

cat("\n=== Example 1: left_join ===\n")

# External gene annotations (partial matches)
gene_annotations <- data.frame(
  gene_id = paste0("Gene_", c(1:8, 15:17)),  # Some genes match, some are new
  chromosome = sample(paste0("chr", 1:22), 11, replace = TRUE),
  description = paste("Gene function", c(1:8, 15:17))
)

tm_left <- tm |>
  activate(rows) |>
  left_join(gene_annotations, by = "gene_id")

cat("After left_join:\n")
cat("- Rows:", nrow(tm_left$row_data), "(all original genes kept)\n")
cat("- Genes 9-10 have NA for new columns\n")
print(head(tm_left$row_data))

# =============================================================================
# Example 2: inner_join - Keep only annotated genes
# =============================================================================

cat("\n=== Example 2: inner_join ===\n")

tm_inner <- tm |>
  activate(rows) |>
  inner_join(gene_annotations, by = "gene_id")

cat("After inner_join:\n")
cat("- Rows:", nrow(tm_inner$row_data), "(only genes 1-8, with matches)\n")
cat("- Matrix dimensions:", nrow(tm_inner$matrix), "x", ncol(tm_inner$matrix), "\n")

# =============================================================================
# Example 3: full_join - Combine all genes
# =============================================================================

cat("\n=== Example 3: full_join ===\n")

tm_full <- tm |>
  activate(rows) |>
  full_join(gene_annotations, by = "gene_id")

cat("After full_join:\n")
cat("- Rows:", nrow(tm_full$row_data), "(all genes from both sources)\n")
cat("- Matrix dimensions:", nrow(tm_full$matrix), "x", ncol(tm_full$matrix), "\n")
cat("- New genes (15-17) have NA matrix values\n")

# Check for NA rows
cat("Gene_15 matrix values:", tm_full$matrix[tm_full$row_data$gene_id == "Gene_15", ], "\n")

# =============================================================================
# Example 4: right_join - Keep only annotated genes
# =============================================================================

cat("\n=== Example 4: right_join ===\n")

tm_right <- tm |>
  activate(rows) |>
  right_join(gene_annotations, by = "gene_id")

cat("After right_join:\n")
cat("- Rows:", nrow(tm_right$row_data), "(all annotated genes)\n")
cat("- Genes 9-10 removed, Genes 15-17 added with NA\n")

# =============================================================================
# Example 5: semi_join - Filter to annotated genes (no new columns)
# =============================================================================

cat("\n=== Example 5: semi_join ===\n")

tm_semi <- tm |>
  activate(rows) |>
  semi_join(gene_annotations, by = "gene_id")

cat("After semi_join:\n")
cat("- Rows:", nrow(tm_semi$row_data), "(only genes with matches)\n")
cat("- Columns in row_data:", ncol(tm_semi$row_data), "(no new annotation columns)\n")
cat("- Matrix dimensions:", nrow(tm_semi$matrix), "x", ncol(tm_semi$matrix), "\n")

# =============================================================================
# Example 6: anti_join - Filter to non-annotated genes
# =============================================================================

cat("\n=== Example 6: anti_join ===\n")

tm_anti <- tm |>
  activate(rows) |>
  anti_join(gene_annotations, by = "gene_id")

cat("After anti_join:\n")
cat("- Rows:", nrow(tm_anti$row_data), "(only genes without matches)\n")
cat("- These are genes 9 and 10\n")
print(tm_anti$row_data)

# =============================================================================
# Example 7: Column joins - Add sample metadata
# =============================================================================

cat("\n=== Example 7: Column join ===\n")

# External sample metadata
sample_metadata <- data.frame(
  sample_id = paste0("Sample_", c(1:3, 6:8)),
  batch = c("A", "A", "B", "C", "C", "C"),
  date = as.Date("2024-01-01") + 0:5
)

tm_col <- tm |>
  activate(columns) |>
  left_join(sample_metadata, by = "sample_id")

cat("After left_join on columns:\n")
cat("- Columns:", ncol(tm_col$matrix), "(all original samples kept)\n")
cat("- Samples 4-5 have NA for batch and date\n")
print(tm_col$col_data)

# =============================================================================
# Example 8: Chaining joins with dplyr operations
# =============================================================================

cat("\n=== Example 8: Chaining operations ===\n")

# Add annotations, filter to specific pathway, then analyze
result <- tm |>
  activate(rows) |>
  left_join(gene_annotations, by = "gene_id") |>
  filter(pathway == "Metabolism") |>
  select(gene_id, pathway, chromosome, description)

cat("Filtered to Metabolism pathway genes:\n")
print(result)

cat("\n=== All examples completed successfully! ===\n")
