# Pheatmap Integration Example for tidymatrix
# Demonstrates how to create heatmaps with annotations and clustering

library(tidymatrix)
library(dplyr)
library(pheatmap)

# Create example gene expression data
set.seed(42)
n_genes <- 30
n_samples <- 20

# Simulate expression matrix with structure
expression <- matrix(abs(rnorm(n_genes * n_samples, mean = 100, sd = 50)),
  nrow = n_genes, ncol = n_samples
)

# Add some structure: different gene groups
expression[1:10, 1:10] <- expression[1:10, 1:10] * 2 # High in first condition
expression[11:20, 11:20] <- expression[11:20, 11:20] * 2 # High in second condition

# Create metadata
gene_data <- data.frame(
  gene_id = paste0("Gene_", 1:n_genes),
  gene_type = c(
    rep("TypeA_responder", 10),
    rep("TypeB_responder", 10),
    rep("Housekeeping", 10)
  ),
  chromosome = sample(paste0("chr", 1:5), n_genes, replace = TRUE)
)

sample_data <- data.frame(
  sample_id = paste0("Sample_", 1:n_samples),
  condition = rep(c("Control", "Treatment"), each = 10),
  batch = rep(1:4, each = 5),
  patient = rep(paste0("Patient_", 1:10), 2)
)

# Create tidymatrix
expr_tm <- tidymatrix(expression, gene_data, sample_data)

cat("\n=== Example 1: Basic Heatmap with Annotations ===\n")

# Basic heatmap with automatic clustering
plot_pheatmap(expr_tm,
  row_names = "gene_id",
  col_names = "sample_id"
)

cat("Created heatmap with:\n")
cat("  - Row names from gene_id\n")
cat("  - Column names from sample_id\n")
cat("  - All metadata columns as annotations\n")
cat("  - Automatic clustering\n")

cat("\n=== Example 2: Preprocessed Data with Custom Annotations ===\n")

# Preprocess: log transform and scale
expr_processed <- expr_tm |>
  activate(matrix) |>
  log_transform(base = 2, offset = 1) |>
  activate(rows) |>
  scale()

# Heatmap with specific annotation columns
plot_pheatmap(expr_processed,
  row_names = "gene_id",
  col_names = "sample_id",
  row_annotation = c("gene_type"), # Only show gene type
  col_annotation = c("condition", "batch"), # Show condition and batch
  show_rownames = FALSE, # Don't show row names (too many)
  show_colnames = TRUE
)

cat("Created heatmap with:\n")
cat("  - Log-transformed and scaled data\n")
cat("  - Custom row annotations (gene_type only)\n")
cat("  - Custom column annotations (condition and batch)\n")
cat("  - Row names hidden\n")

cat("\n=== Example 3: Using Stored Clustering Results ===\n")

# Perform clustering and store results
expr_clustered <- expr_tm |>
  activate(matrix) |>
  log_transform(base = 2, offset = 1) |>
  activate(rows) |>
  compute_hclust(k = 3, name = "gene_clusters", method = "ward.D2") |>
  activate(columns) |>
  compute_hclust(k = 2, name = "sample_groups", method = "complete")

# Use the stored clustering
plot_pheatmap(expr_clustered,
  row_names = "gene_id",
  col_names = "sample_id",
  row_cluster = "gene_clusters",
  col_cluster = "sample_groups",
  show_rownames = FALSE
)

cat("Created heatmap with:\n")
cat("  - Stored hierarchical clustering for rows (ward.D2)\n")
cat("  - Stored hierarchical clustering for columns (complete)\n")
cat("  - Cluster assignments shown in annotations\n")

cat("\n=== Example 4: Complete Analysis Pipeline ===\n")

# Full preprocessing and visualization pipeline
result <- expr_tm |>
  # Add statistics
  activate(rows) |>
  add_stats(mean, var) |>

  # Filter for variable genes
  filter(var > quantile(var, 0.5)) |>

  # Log transform and scale
  activate(matrix) |>
  log_transform(base = 2, offset = 1) |>
  activate(rows) |>
  center() |>
  scale() |>

  # Cluster
  compute_hclust(k = 2, name = "gene_modules", method = "ward.D2") |>
  activate(columns) |>
  compute_hclust(k = 2, name = "sample_types", method = "average")

cat("Processed data:\n")
cat("  - Started with", nrow(expr_tm$matrix), "genes\n")
cat("  - Filtered to", nrow(result$matrix), "most variable genes\n")
cat("  - Log-transformed and z-scaled by gene\n")
cat("  - Clustered into 2 gene modules and 2 sample types\n\n")

# Create heatmap
plot_pheatmap(result,
  row_names = "gene_id",
  col_names = "sample_id",
  row_cluster = "gene_modules",
  col_cluster = "sample_types",
  row_annotation = c("gene_type", "gene_modules_cluster"),
  col_annotation = c("condition", "sample_types_cluster"),
  show_rownames = FALSE,
  color = colorRampPalette(c("blue", "white", "red"))(50),
  main = "Gene Expression Heatmap"
)

cat("Final heatmap shows:\n")
cat("  - Most variable genes only\n")
cat("  - Gene modules and sample types from clustering\n")
cat("  - Original annotations (gene type, condition)\n")
cat("  - Custom color scheme\n")

cat("\n=== Example 5: No Clustering ===\n")

# Heatmap without clustering (preserve original order)
plot_pheatmap(expr_tm,
  row_names = "gene_id",
  col_names = "sample_id",
  row_cluster = FALSE,
  col_cluster = FALSE,
  show_rownames = FALSE,
  main = "Expression (No Clustering)"
)

cat("Created heatmap without clustering\n")
cat("  - Rows and columns in original order\n")
cat("  - Useful for ordered data or time series\n")

cat("\n=== Example 6: Focus on Specific Samples ===\n")

# Filter and visualize specific subset
treatment_only <- expr_tm |>
  activate(columns) |>
  filter(condition == "Treatment") |>
  activate(matrix) |>
  log_transform(base = 2, offset = 1) |>
  activate(rows) |>
  scale()

plot_pheatmap(treatment_only,
  row_names = "gene_id",
  col_names = "patient",
  col_annotation = "batch",
  show_rownames = FALSE,
  main = "Treatment Samples Only"
)

cat("Created heatmap for treatment samples only:\n")
cat("  -", ncol(treatment_only$matrix), "treatment samples\n")
cat("  - Labeled by patient\n")
cat("  - Colored by batch\n")

cat("\n=== Example 7: Export-Ready Heatmap ===\n")

# Create publication-ready heatmap
final_plot <- expr_tm |>
  activate(rows) |>
  add_stats(var) |>
  filter(var > quantile(var, 0.75)) |> # Top 25% variable
  activate(matrix) |>
  log_transform(base = 2, offset = 1) |>
  activate(rows) |>
  scale() |>
  compute_hclust(k = 3, name = "clusters", method = "ward.D2") |>
  plot_pheatmap(
    row_names = "gene_id",
    col_names = "sample_id",
    row_cluster = "clusters",
    row_annotation = c("gene_type", "clusters_cluster"),
    col_annotation = c("condition", "batch"),
    show_rownames = FALSE,
    show_colnames = FALSE,
    fontsize = 10,
    fontsize_row = 8,
    fontsize_col = 8,
    color = colorRampPalette(c("navy", "white", "firebrick3"))(100),
    main = "Gene Expression Patterns",
    border_color = NA
  )

cat("Publication-ready heatmap created with:\n")
cat("  - Top 25% most variable genes\n")
cat("  - Z-scaled expression values\n")
cat("  - Ward.D2 hierarchical clustering\n")
cat("  - Clean formatting and custom colors\n")

cat("\n=== Pheatmap Integration Examples Complete ===\n")
cat("\nNote: All plots are created but not saved to file.\n")
cat("Use pdf() or png() to save plots, e.g.:\n")
cat("  pdf('heatmap.pdf', width=10, height=8)\n")
cat("  plot_pheatmap(...)\n")
cat("  dev.off()\n")
