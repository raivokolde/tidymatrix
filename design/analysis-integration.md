# Integrating Analytical Methods with tidymatrix

## The Challenge

Common matrix analyses (clustering, PCA, dimensionality reduction) produce rich outputs that relate to both the matrix data and metadata. We need to decide how to structure these results within the tidymatrix framework.

## Common Analyses and Their Outputs

### 1. Hierarchical Clustering
**Input:** Matrix (e.g., genes × samples)
**Output:**
- Dendrogram/tree object (for rows, columns, or both)
- Cluster assignments (categorical: which cluster each row/column belongs to)
- Distance matrix
- Optimal row/column ordering for visualization

**Typical use case:**
```r
# Cluster samples by gene expression
hclust_result <- hclust(dist(t(expression_matrix)))
cutree(hclust_result, k = 3)  # Get 3 clusters
```

### 2. PCA (Principal Component Analysis)
**Input:** Matrix (typically samples × features)
**Output:**
- PC scores (samples in PC space - new matrix)
- PC loadings (feature contributions to PCs)
- Variance explained per PC
- Rotation matrix

**Typical use case:**
```r
pca_result <- prcomp(t(expression_matrix))
# pca_result$x = PC scores (samples × PCs)
# pca_result$rotation = loadings (genes × PCs)
# pca_result$sdev = variance explained
```

### 3. t-SNE / UMAP
**Input:** Matrix
**Output:**
- Low-dimensional coordinates (typically 2D or 3D)
- Per-point statistics (e.g., perplexity, local density)

**Typical use case:**
```r
tsne_result <- Rtsne(t(expression_matrix))
# tsne_result$Y = 2D coordinates for samples
```

### 4. K-means Clustering
**Input:** Matrix
**Output:**
- Cluster assignments
- Cluster centers
- Within-cluster sum of squares

## Design Options

### Option 1: Attach Results to Metadata (Lightweight)

**Approach:** Add analysis results as new columns in row_data or col_data.

```r
# Add PCA scores to sample metadata
tm <- expr_tm %>%
  activate(columns) %>%
  add_pca(n_components = 3)  # Adds PC1, PC2, PC3 to col_data

# Result:
# col_data now has: sample_id, condition, batch, PC1, PC2, PC3

# Add cluster assignments
tm <- tm %>%
  activate(rows) %>%
  add_hclust(k = 5)  # Adds cluster column to row_data

# Chain analyses
tm <- expr_tm %>%
  activate(columns) %>%
  add_pca(n_components = 2) %>%
  add_umap(n_components = 2) %>%
  add_hclust(k = 3)
```

**Pros:**
- Simple and clean - results live where they logically belong
- Easy to use in downstream operations (filter, group_by, etc.)
- No new classes needed
- Integrates naturally with existing tidymatrix verbs

**Cons:**
- Loses rich analysis objects (dendrograms, loadings, etc.)
- Can't easily access variance explained, dendrograms, etc.
- Hard to apply same analysis multiple times with different parameters
- No way to store analysis metadata (parameters used, etc.)

**Good for:** Quick exploratory analyses, simple cluster assignments

---

### Option 2: Analysis Results as Attributes (Medium)

**Approach:** Store full analysis objects as attributes of the tidymatrix, optionally add key results to metadata.

```r
# Perform PCA and store full results
tm <- expr_tm %>%
  activate(columns) %>%
  do_pca(n_components = 5, name = "sample_pca")

# Attributes stored:
# attr(tm, "analyses")$sample_pca = full prcomp object

# Auto-add scores to col_data:
# col_data now has: ..., PC1, PC2, PC3, PC4, PC5

# Access full results
pca_obj <- get_analysis(tm, "sample_pca")
plot(pca_obj$sdev)  # Variance explained

# Access loadings
loadings <- get_loadings(tm, "sample_pca")  # Returns gene × PC matrix

# Multiple analyses with different parameters
tm <- tm %>%
  activate(rows) %>%
  do_hclust(k = 3, name = "gene_clusters_k3") %>%
  do_hclust(k = 5, name = "gene_clusters_k5")

# List all analyses
list_analyses(tm)
# [1] "sample_pca" "gene_clusters_k3" "gene_clusters_k5"
```

**Pros:**
- Keeps full analysis objects for detailed exploration
- Can store multiple analyses with different parameters
- Convenient shortcuts via metadata columns
- Analysis metadata preserved (parameters, method, date)

**Cons:**
- More complex implementation
- Need naming scheme to avoid conflicts
- tidymatrix object can get "heavy" with many analyses
- Copying/filtering tidymatrix also copies all stored analyses

**Good for:** Comprehensive analyses, reproducibility, detailed exploration

---

### Option 3: Separate Analysis Objects (Heavyweight)

**Approach:** Return dedicated analysis result objects that link back to tidymatrix.

```r
# PCA returns a tidymatrix_pca object
pca_result <- tm %>%
  activate(columns) %>%
  tidy_pca(n_components = 5)

class(pca_result)
# [1] "tidymatrix_pca" "tidymatrix" "list"

# Access components
pca_result$scores          # PC scores as tidymatrix
pca_result$loadings        # Loadings as data.frame
pca_result$variance_explained
pca_result$original_data   # Link back to original tm

# Plot methods
plot(pca_result, type = "variance")
plot(pca_result, type = "biplot", color_by = "condition")

# Clustering returns tidymatrix_hclust
hc_result <- tm %>%
  activate(rows) %>%
  tidy_hclust(k = 3)

hc_result$tidymatrix       # tidymatrix with cluster column added
hc_result$dendrogram       # dendrogram object
hc_result$distances        # distance matrix

plot(hc_result, type = "dendrogram")
plot(hc_result, type = "heatmap")  # Heatmap with dendrogram
```

**Pros:**
- Clean separation of concerns
- Rich, specialized objects for each analysis type
- Easy to implement analysis-specific methods
- Can have specialized plotting for each analysis
- tidymatrix stays lightweight

**Cons:**
- More classes to maintain
- Less "tidy" - breaks the pipe-friendly pattern somewhat
- Results are separate from the original data

**Good for:** Publication-quality analyses, complex visualizations

---

### Option 4: Hybrid Approach (Recommended)

**Approach:** Combine the best of Options 1 and 2.

**Simple functions** add results to metadata only:
```r
# Add key results to metadata (lightweight)
tm <- tm %>%
  activate(columns) %>%
  add_pca_scores(n = 2)  # Just adds PC1, PC2 to col_data

tm <- tm %>%
  activate(rows) %>%
  add_clusters(method = "hclust", k = 3)  # Just adds cluster column
```

**Comprehensive functions** store full results as attributes AND add to metadata:
```r
# Perform full analysis, store results, add key columns
tm <- tm %>%
  activate(columns) %>%
  compute_pca(n_components = 5, name = "main_pca")

# - Adds PC1-PC5 to col_data
# - Stores full prcomp object as attribute

# Get full analysis object
pca_obj <- get_analysis(tm, "main_pca")
# Returns standard prcomp object - use it however you want!

# Standard prcomp methods work
summary(pca_obj)
plot(pca_obj$sdev)              # Scree plot
biplot(pca_obj)                 # Biplot
pca_obj$rotation                # Loadings
pca_obj$sdev^2 / sum(pca_obj$sdev^2)  # Variance explained

# List all stored analyses
list_analyses(tm)
# [1] "main_pca"

# Remove analysis if no longer needed
tm <- remove_analysis(tm, "main_pca")

# Or remove all
tm <- remove_all_analyses(tm)
```

**Pros:**
- Flexibility: choose simple or comprehensive based on needs
- Lightweight option for exploration
- Full option for reproducibility and detailed analysis
- Can always extract full objects when needed
- Extracted results are themselves tidymatrix objects

**Cons:**
- Two APIs to learn (add_* vs compute_*)
- Need clear naming conventions

---

## Integration with Visualization

Regardless of storage approach, we need good visualization:

### Heatmaps with Clustering
```r
# pheatmap-style with ggplot2
tm %>%
  activate(rows) %>%
  compute_hclust(k = 5, name = "gene_clusters") %>%
  activate(columns) %>%
  compute_hclust(k = 3, name = "sample_clusters") %>%
  plot_heatmap(
    row_cluster = "gene_clusters",
    col_cluster = "sample_clusters",
    color_rows_by = "pathway",
    color_cols_by = "condition",
    show_row_dend = TRUE,
    show_col_dend = TRUE
  )
```

### PCA Plots
```r
tm %>%
  activate(columns) %>%
  compute_pca(name = "pca") %>%
  plot_pca(
    analysis = "pca",
    dims = c(1, 2),
    color_by = "condition",
    shape_by = "batch",
    label_by = "sample_id"
  )
```

### Dimensionality Reduction
```r
tm %>%
  activate(columns) %>%
  add_umap(n_components = 2) %>%
  plot_scatter(
    x = "UMAP1",
    y = "UMAP2",
    color_by = "condition",
    size_by = "qc_score"
  )
```

## Recommendations

### Phase 1: Start Simple (Option 1 + helpers)
1. Implement `add_*` functions that just add columns to metadata
   - `add_pca_scores()` - adds PC coordinates
   - `add_umap()` - adds UMAP coordinates
   - `add_clusters()` - adds cluster assignments
   - `add_hclust()` - hierarchical clustering
   - `add_kmeans()` - k-means clustering

2. These are wrappers around base R / standard packages
3. Results are just new columns - easy to use with existing verbs

### Phase 2: Rich Results (Option 2 or 4)
4. Implement `compute_*` functions that store full results
   - Store as attributes with clear naming
   - Auto-add key results to metadata
   - Implement getters: `get_analysis()`, `get_loadings()`, etc.

5. Extractors return tidymatrix objects when possible
   - `get_loadings()` returns tidymatrix (features × PCs)
   - `get_scores()` returns tidymatrix (samples × PCs)

### Phase 3: Visualization
6. Implement plotting functions
   - `plot_heatmap()` with optional dendrograms
   - `plot_pca()` for PCA-specific plots
   - `plot_scatter()` for general 2D plots
   - Integration with ggplot2

## Key Design Principles

1. **Metadata-first**: Results that are per-row or per-column should live in metadata

2. **Attributes for rich objects**: Full analysis objects stored as attributes with clear names

3. **tidymatrix all the way down**: Extracted results should be tidymatrix objects when they have matrix structure

4. **Composability**: Should work with pipes and existing tidymatrix verbs

5. **Discoverability**: Easy to list, access, and remove stored analyses

## Example Workflow: Gene Expression Analysis

```r
library(tidymatrix)
library(dplyr)

# Start with expression data
expr_tm <- tidymatrix(
  expression_matrix,  # genes × samples
  gene_data,         # gene annotations
  sample_data        # sample metadata
)

# Comprehensive analysis pipeline
expr_tm <- expr_tm %>%
  # QC filtering
  activate(columns) %>%
  filter(qc_pass == TRUE) %>%

  # Gene filtering
  activate(rows) %>%
  filter(mean_expression > 1) %>%

  # PCA on samples
  activate(columns) %>%
  compute_pca(n_components = 10, name = "sample_pca") %>%

  # Cluster genes
  activate(rows) %>%
  compute_hclust(k = 5, method = "ward.D2", name = "gene_clusters") %>%

  # UMAP for visualization
  activate(columns) %>%
  add_umap(n_neighbors = 15, n_components = 2)

# Now we can:

# 1. Use PC scores for grouping
expr_tm %>%
  activate(columns) %>%
  mutate(PC1_high = PC1 > median(PC1)) %>%
  group_by(PC1_high) %>%
  summarize(n = n())

# 2. Examine gene clusters
expr_tm %>%
  activate(rows) %>%
  count(cluster, pathway)

# 3. Plot PCA
plot_pca(expr_tm, "sample_pca", color_by = "condition")

# 4. Plot heatmap with dendrograms
plot_heatmap(expr_tm,
             row_cluster = "gene_clusters",
             color_cols_by = "condition")

# 5. Get loadings to see what drives PC1
loadings <- get_loadings(expr_tm, "sample_pca")
loadings %>%
  activate(rows) %>%
  arrange(desc(abs(PC1))) %>%
  slice_head(n = 20)  # Top 20 genes driving PC1
```

## Analysis Invalidation Problem

**The Challenge:** What happens when data is modified after analysis?

```r
# Compute PCA on 1000 genes
tm <- expr_tm %>%
  activate(rows) %>%
  compute_pca(name = "gene_pca")  # Stores full PCA object

# Later: filter to top 500 genes
tm <- tm %>%
  activate(rows) %>%
  filter(variance > threshold)  # Now only 500 genes

# Problem: Stored PCA object is based on 1000 genes!
pca_obj <- get_analysis(tm, "gene_pca")  # Returns outdated analysis
```

This could lead to:
- Misleading results
- Reproducibility issues
- Confusion about what data was analyzed

### Solution 1: Auto-Remove on Modification (Simple & Safe)

**Approach:** Remove all stored analyses whenever data is modified.

```r
tm <- expr_tm %>%
  activate(columns) %>%
  compute_pca(name = "sample_pca")  # Stores analysis

list_analyses(tm)
# [1] "sample_pca"

# Any modification removes stored analyses
tm <- tm %>%
  activate(rows) %>%
  filter(mean_expr > 1)

list_analyses(tm)
# character(0)  # Analyses removed!

# Warning message:
# Removed 1 stored analysis due to data modification: sample_pca
# Metadata columns (PC1, PC2, ...) are preserved.
```

**What gets removed vs. kept:**
- ✗ Full analysis objects (prcomp, hclust, etc.) - **REMOVED**
- ✓ Metadata columns (PC1, PC2, cluster) - **KEPT**
- Rationale: Columns are just data, objects represent relationships

**Operations that trigger removal:**
- `filter()` on active component
- `slice()` on active component
- `arrange()` - maybe keep? (just reordering)
- `mutate()` - keep (not changing matrix)
- `select()` on metadata - keep (not changing matrix)
- Any matrix transformation (scaling, normalization)

**Pros:**
- Simple to implement
- Always safe - no stale analyses
- Clear mental model

**Cons:**
- Potentially too aggressive
- Might remove analyses even when safe
- Need to re-run analysis if needed later

---

### Solution 2: Smart Invalidation (Complex)

**Approach:** Track which analyses are affected by which operations.

```r
# Each analysis tracks what it depends on
attr(tm, "analyses")$sample_pca$depends_on <- list(
  active = "columns",
  row_ids = rownames(tm$matrix),
  col_ids = rownames(tm$col_data),
  matrix_hash = digest(tm$matrix)
)

# Filter rows - doesn't affect column-based PCA
tm <- tm %>%
  activate(rows) %>%
  filter(mean_expr > 1)

list_analyses(tm)
# [1] "sample_pca"  # Still present! Rows don't affect column PCA

# But filter columns - now it's affected
tm <- tm %>%
  activate(columns) %>%
  filter(batch == 1)

list_analyses(tm)
# character(0)  # Removed because columns changed
```

**Invalidation rules:**
- Row-based analysis (gene clustering): invalidated by row operations
- Column-based analysis (sample PCA): invalidated by column operations
- Matrix-based analysis: invalidated by any change

**Pros:**
- More intelligent - keeps analyses when safe
- Better for complex workflows

**Cons:**
- Complex to implement correctly
- Hard to reason about edge cases
- What about matrix transformations that preserve structure?

---

### Solution 3: Explicit Lifecycle with Warnings (Recommended)

**Approach:** Track analysis validity, warn on access, let user decide.

```r
# Compute analysis - marked as "valid"
tm <- expr_tm %>%
  activate(columns) %>%
  compute_pca(name = "sample_pca")

attr(tm, "analyses")$sample_pca$valid <- TRUE
attr(tm, "analyses")$sample_pca$computed_on <- Sys.time()
attr(tm, "analyses")$sample_pca$n_rows <- nrow(tm$matrix)
attr(tm, "analyses")$sample_pca$n_cols <- ncol(tm$matrix)

# Modify data - mark as "stale" but keep
tm <- tm %>%
  activate(columns) %>%
  filter(batch == 1)

attr(tm, "analyses")$sample_pca$valid <- FALSE
attr(tm, "analyses")$sample_pca$invalidated_on <- Sys.time()
attr(tm, "analyses")$sample_pca$invalidated_by <- "filter(columns)"

# Try to access stale analysis - warning
pca_obj <- get_analysis(tm, "sample_pca")
# Warning: Analysis 'sample_pca' was computed on different data:
#   - Original: 1000 rows × 50 columns
#   - Current:  1000 rows × 30 columns
#   - Invalidated by: filter(columns) on 2025-01-02
#
# Use recompute_analysis(tm, "sample_pca") to update.

# User can explicitly remove or recompute
tm <- remove_analysis(tm, "sample_pca")
tm <- recompute_analysis(tm, "sample_pca")  # Re-runs with same parameters
```

**Additional features:**
```r
# Check validity
check_analyses(tm)
# Analysis 'sample_pca': STALE (computed on 50 columns, now 30)
# Analysis 'gene_clusters': VALID

# Remove all stale analyses
tm <- remove_stale_analyses(tm)

# Or remove all analyses
tm <- remove_all_analyses(tm)
```

**Pros:**
- User stays in control
- Clear warnings prevent silent errors
- Can recompute if desired
- Preserves analysis history

**Cons:**
- More complex user experience
- Users might ignore warnings
- Need good documentation

---

### Solution 4: Snapshot Approach (Alternative)

**Approach:** Store snapshot of data state with each analysis.

```r
# Analysis stores minimal snapshot
attr(tm, "analyses")$sample_pca$snapshot <- list(
  row_names = rownames(tm$matrix),
  col_names = rownames(tm$col_data),
  matrix_dim = dim(tm$matrix)
)

# When accessing, check if current state matches snapshot
pca_obj <- get_analysis(tm, "sample_pca")
# Automatically checks if dimensions/names match
# Warns if mismatch, shows what changed
```

---

### Solution 5: Keep Columns, Remove Objects (Hybrid - Simple)

**Approach:** Metadata columns are permanent, analysis objects are temporary.

```r
tm <- expr_tm %>%
  activate(columns) %>%
  compute_pca(name = "sample_pca")

# This creates:
# 1. PC1, PC2, ... columns in col_data (PERMANENT)
# 2. Full prcomp object as attribute (TEMPORARY)

# Filter data
tm <- tm %>%
  activate(rows) %>%
  filter(mean_expr > 1)

# Result:
# - PC1, PC2 columns still exist (they're just data)
# - prcomp object removed (would be misleading)
# - User sees: "Removed stored analysis 'sample_pca' (metadata columns preserved)"

# If you need the full object again:
tm <- recompute_pca(tm, name = "sample_pca",
                    using_columns = c("PC1", "PC2", ...))
# Or re-run from scratch
```

**Philosophy:**
- Metadata columns = data (keep them)
- Analysis objects = relationships in data (remove when data changes)

**Pros:**
- Clear separation
- Safe default
- Metadata columns remain useful even if object is gone

**Cons:**
- Metadata columns might be misleading without context
- Can't access variance explained, loadings, etc.

---

## Recommended Approach: Solution 3 + 5 Combined

**Default behavior** (Solution 5):
- Remove analysis objects on data modification
- Keep metadata columns (they're just data)
- Clear warning about what was removed

**Advanced features** (Solution 3):
- `preserve_analyses = TRUE` option to keep objects but mark as stale
- `get_analysis()` warns if stale
- `recompute_analysis()` to update
- `check_analyses()` to see status

```r
# Default: removes objects, keeps columns
tm <- expr_tm %>%
  compute_pca(name = "pca") %>%
  filter(mean_expr > 1)
# Warning: Removed stored analysis 'pca' (columns PC1, PC2, ... preserved)

# Preserve objects (advanced)
tm <- expr_tm %>%
  compute_pca(name = "pca") %>%
  filter(mean_expr > 1, preserve_analyses = TRUE)
# Warning: Analysis 'pca' may be stale (data modified)

# Check and recompute
check_analyses(tm)
tm <- recompute_analysis(tm, "pca")
```

---

## Implementation Guidelines

### Operations that invalidate analyses:

**Always invalidate:**
- `filter()` on active component
- `slice*()` on active component
- Matrix transformations (if we add them)
- `summarize()` (completely different data)

**Never invalidate:**
- `mutate()` on metadata (just adding columns)
- `select()` on metadata (just choosing columns)
- `rename()` on metadata
- `relocate()` on metadata

**Maybe invalidate (user choice):**
- `arrange()` - reorders but doesn't remove
  - Row clustering might care about order
  - PCA probably doesn't care
  - Default: keep objects, mark as "reordered"

### User control:

```r
# Globally set policy
options(tidymatrix.analysis_policy = "remove")  # default
options(tidymatrix.analysis_policy = "warn")    # keep but warn
options(tidymatrix.analysis_policy = "keep")    # keep silently (dangerous!)

# Per-operation override
tm %>% filter(..., preserve_analyses = TRUE)
```

## Questions for Discussion

1. **Which solution appeals to you most?**
   - Solution 1 (always remove)?
   - Solution 3 (keep but warn)?
   - Solution 5 (remove objects, keep columns)?
   - Combined approach?

2. **Should `arrange()` invalidate analyses?**
   - It changes order but not content
   - Might matter for some analyses, not others

3. **Metadata columns after filtering:**
   - Keep PC1, PC2 even though underlying PCA object is gone?
   - Or remove them too to avoid confusion?

4. **Recompute functionality:**
   - Should we auto-recompute if user tries to access stale analysis?
   - Or just warn and let them explicitly recompute?
