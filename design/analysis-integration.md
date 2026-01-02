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
# - Can access with get_analysis(tm, "main_pca")

# Get stored analyses
analyses <- get_analyses(tm)
names(analyses)
# [1] "main_pca"

pca_obj <- analyses$main_pca
plot(pca_obj$sdev)  # Variance explained

# Extract loadings with metadata
loadings_tm <- get_loadings(tm, "main_pca")  # Returns tidymatrix!
# Matrix: genes × PCs
# row_data: original gene metadata
# col_data: PC number, variance explained per PC

# Remove analysis if no longer needed
tm <- remove_analysis(tm, "main_pca")
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

## Questions for Discussion

1. **Which option appeals to you most?** (1, 2, 3, or 4)

2. **Storage priority:**
   - Keep it simple (just metadata columns)?
   - Store full analysis objects?

3. **Multiple analyses:**
   - Do you often need to compare multiple clusterings/PCAs?
   - If yes, how do you currently handle this?

4. **Visualization priority:**
   - Is heatmap + dendrogram the main need?
   - Or also PCA plots, scatter plots, etc.?

5. **Analysis types:**
   - Which analyses are most critical for your work?
   - PCA, hierarchical clustering, k-means, UMAP, t-SNE?
   - Any others (NMF, ICA, etc.)?

6. **Real-world workflow:**
   - Can you describe a typical analysis pipeline?
   - This will help us design the right API
