# compute_across Examples
# This file demonstrates the use of compute_across() for row-wise statistical analysis

library(tidymatrix)
library(dplyr)

# Create example gene expression data
set.seed(123)
expr_matrix <- matrix(rnorm(600, mean = 10, sd = 2), nrow = 60, ncol = 10)

# Row metadata (genes)
gene_data <- data.frame(
  gene_id = paste0("Gene_", 1:60),
  pathway = sample(c("Metabolism", "Signaling", "Transport"), 60, replace = TRUE),
  chr = sample(paste0("chr", 1:22), 60, replace = TRUE)
)

# Column metadata (samples)
sample_data <- data.frame(
  sample_id = paste0("Sample_", 1:10),
  condition = rep(c("Control", "Treatment"), each = 5),
  batch = factor(rep(c("Batch1", "Batch2"), 5)),
  age = rnorm(10, 50, 10),
  stringsAsFactors = FALSE
)

# Create tidymatrix
tm <- tidymatrix(expr_matrix, gene_data, sample_data)

# =============================================================================
# Example 1: Simple t-test using test_ttest() convenience function
# =============================================================================

cat("\n=== Example 1: Simple t-test ===\n")

ttest_results <- tm |>
  activate(rows) |>
  test_ttest(group_col = "condition")

print(head(ttest_results))

# Filter for significant genes (FDR < 0.05)
sig_genes <- ttest_results |>
  filter(p.adj < 0.05)

cat("\nNumber of significant genes:", nrow(sig_genes), "\n")

# =============================================================================
# Example 2: Custom t-test with compute_across()
# =============================================================================

cat("\n=== Example 2: Custom t-test with additional statistics ===\n")

custom_ttest <- tm |>
  activate(rows) |>
  compute_across(
    fn = function(vals, meta) {
      ctrl <- vals[meta$condition == "Control"]
      trt <- vals[meta$condition == "Treatment"]

      test <- t.test(ctrl, trt)

      list(
        mean_ctrl = mean(ctrl),
        mean_trt = mean(trt),
        log2fc = log2(mean(trt) / mean(ctrl)),
        p.value = test$p.value,
        ci_lower = test$conf.int[1],
        ci_upper = test$conf.int[2]
      )
    }
  )

print(head(custom_ttest))

# =============================================================================
# Example 3: Linear model with multiple predictors using test_lm()
# =============================================================================

cat("\n=== Example 3: Linear model with multiple predictors ===\n")

lm_results <- tm |>
  activate(rows) |>
  test_lm(
    formula_rhs = ~ condition + batch + age,
    coef = "conditionTreatment"
  )

print(head(lm_results))

# Filter for genes with significant treatment effect
sig_lm <- lm_results |>
  filter(p.adj < 0.05)

cat("\nGenes with significant treatment effect:", nrow(sig_lm), "\n")

# =============================================================================
# Example 4: Overall model statistics
# =============================================================================

cat("\n=== Example 4: Overall model statistics ===\n")

model_fit <- tm |>
  activate(rows) |>
  test_lm(formula_rhs = ~ condition + batch + age)

print(head(model_fit))

# =============================================================================
# Example 5: Correlation with a phenotype
# =============================================================================

cat("\n=== Example 5: Correlation with phenotype ===\n")

cor_results <- tm |>
  activate(rows) |>
  compute_across(
    fn = function(vals, meta) {
      cor_test <- cor.test(vals, meta$age, method = "spearman")
      list(
        correlation = cor_test$estimate,
        p.value = cor_test$p.value
      )
    }
  ) |>
  mutate(p.adj = p.adjust(p.value, method = "fdr"))

print(head(cor_results))

# Genes with strong correlation (|rho| > 0.7, FDR < 0.05)
strong_cor <- cor_results |>
  filter(abs(correlation) > 0.7, p.adj < 0.05)

cat("\nGenes with strong age correlation:", nrow(strong_cor), "\n")

# =============================================================================
# Example 6: Add results to metadata
# =============================================================================

cat("\n=== Example 6: Add results to tidymatrix metadata ===\n")

tm_enriched <- tm |>
  activate(rows) |>
  test_ttest(group_col = "condition") |>
  # Add back to tidymatrix as new columns
  {
    tm |>
      activate(rows) |>
      compute_across(
        fn = function(vals, meta) {
          test <- t.test(vals ~ meta$condition)
          list(ttest_pval = test$p.value)
        },
        add_to_data = TRUE,
        prefix = "stat"
      )
  }

# Now we can filter the tidymatrix directly
sig_tm <- tm_enriched |>
  activate(rows) |>
  mutate(stat_p.adj = p.adjust(stat_ttest_pval, method = "fdr")) |>
  filter(stat_p.adj < 0.05)

cat("\nRows in filtered tidymatrix:", nrow(sig_tm$matrix), "\n")

# =============================================================================
# Example 7: GLM - Logistic regression
# =============================================================================

cat("\n=== Example 7: Logistic regression ===\n")

# Create binary outcome
sample_data_binary <- sample_data |>
  mutate(disease = ifelse(condition == "Treatment", 1, 0))

tm_binary <- tidymatrix(expr_matrix, gene_data, sample_data_binary)

glm_results <- tm_binary |>
  activate(rows) |>
  compute_across(
    fn = function(vals, meta) {
      fit <- glm(meta$disease ~ vals, family = binomial())
      summ <- summary(fit)
      coef_tbl <- coef(summ)

      list(
        odds_ratio = exp(coef(fit)["vals"]),
        p.value = coef_tbl["vals", "Pr(>|z|)"],
        aic = fit$aic
      )
    }
  ) |>
  mutate(p.adj = p.adjust(p.value, method = "fdr"))

print(head(glm_results))

# =============================================================================
# Example 8: Variance analysis
# =============================================================================

cat("\n=== Example 8: Variance and coefficient of variation ===\n")

variance_stats <- tm |>
  activate(rows) |>
  compute_across(
    fn = function(vals, meta) {
      ctrl_vals <- vals[meta$condition == "Control"]
      trt_vals <- vals[meta$condition == "Treatment"]

      list(
        var_ctrl = var(ctrl_vals),
        var_trt = var(trt_vals),
        cv_ctrl = sd(ctrl_vals) / mean(ctrl_vals),
        cv_trt = sd(trt_vals) / mean(trt_vals),
        var_ratio = var(trt_vals) / var(ctrl_vals)
      )
    }
  )

print(head(variance_stats))

# Genes with increased variability in treatment
high_var_genes <- variance_stats |>
  filter(var_ratio > 2)

cat("\nGenes with >2x variance increase:", nrow(high_var_genes), "\n")

# =============================================================================
# Example 9: Working on columns instead of rows
# =============================================================================

cat("\n=== Example 9: Analysis on columns ===\n")

# Compute statistics for each sample (column)
sample_stats <- tm |>
  activate(columns) |>
  compute_across(
    fn = function(vals, meta) {
      # vals = expression values for this sample across all genes
      # meta = gene_data (row metadata)
      list(
        n_genes = length(vals),
        mean_expr = mean(vals),
        median_expr = median(vals),
        sd_expr = sd(vals),
        n_high_expr = sum(vals > quantile(vals, 0.9))
      )
    }
  )

print(sample_stats)

# =============================================================================
# Example 10: Error handling
# =============================================================================

cat("\n=== Example 10: Error handling ===\n")

# Create a function that might fail for some rows
risky_analysis <- tm |>
  activate(rows) |>
  compute_across(
    fn = function(vals, meta) {
      # This will fail if all values are the same (no variance)
      if (var(vals) == 0) {
        stop("No variance in values")
      }

      test <- t.test(vals ~ meta$condition)
      list(p.value = test$p.value)
    }
  )

# Check for errors
if ("error" %in% names(risky_analysis)) {
  error_rows <- risky_analysis |>
    filter(!is.na(error))
  cat("\nRows with errors:", nrow(error_rows), "\n")
}

# =============================================================================
# Example 11: Combining with dplyr verbs
# =============================================================================

cat("\n=== Example 11: Pipeline with dplyr verbs ===\n")

pathway_analysis <- tm |>
  activate(rows) |>
  # First filter to specific pathway
  filter(pathway == "Metabolism") |>
  # Run t-test
  test_ttest(group_col = "condition") |>
  # Filter for significance
  filter(p.adj < 0.05) |>
  # Sort by effect size
  arrange(desc(abs(log2fc))) |>
  # Select relevant columns
  select(gene_id, pathway, log2fc, p.value, p.adj)

print(pathway_analysis)

# =============================================================================
# Example 12: Visualization pipeline
# =============================================================================

cat("\n=== Example 12: Visualization pipeline ===\n")

# Compute PCA and add statistics
library(ggplot2)

pca_with_stats <- tm |>
  activate(rows) |>
  compute_prcomp(n_components = 2, center = TRUE, scale. = TRUE) |>
  test_ttest(group_col = "condition") |>
  # Now we can plot with both PCA scores and p-values
  mutate(
    significance = ifelse(p.adj < 0.05, "Significant", "Not significant")
  )

# Extract for plotting
plot_data <- pca_with_stats |>
  select(gene_id, pathway, row_pca_PC1, row_pca_PC2, log2fc, p.adj, significance)

# Create plot
p <- ggplot(plot_data, aes(x = row_pca_PC1, y = row_pca_PC2)) +
  geom_point(aes(color = significance, size = abs(log2fc))) +
  theme_minimal() +
  labs(
    title = "PCA of genes colored by significance",
    x = "PC1",
    y = "PC2"
  )

print(p)

cat("\n=== All examples completed successfully! ===\n")
