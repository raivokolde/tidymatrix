# tidymatrix 0.1.0

## Major Features

### Analysis Integration
* Added comprehensive support for analytical methods with automatic metadata integration
* New functions for PCA analysis:
  - `compute_prcomp()`: Perform PCA and store full results
  - `add_pca_scores()`: Lightweight PCA that only adds scores to metadata
* New functions for clustering:
  - `compute_hclust()`: Hierarchical clustering with dendrogram storage
  - `compute_kmeans()`: K-means clustering
  - `add_clusters()`: Lightweight clustering without object storage
* Analysis results are added as columns to metadata with clear naming:
  - PCA: `{name}_PC1`, `{name}_PC2`, etc.
  - Clustering: `{name}_cluster`

### Analysis Management
* `get_analysis()`: Retrieve stored analysis objects (prcomp, hclust, kmeans)
* `list_analyses()`: List all stored analyses
* `remove_analysis()`: Remove specific or all stored analyses
* `check_analyses()`: Check validity of stored analyses

### Analysis Invalidation
* Stored analysis objects are automatically removed when data is modified via `filter()` or `slice()` operations
* Metadata columns (PC scores, cluster assignments) are preserved even when analysis objects are removed
* Clear warnings indicate which analyses were removed during data modification

## Core Features (from initial development)

### Data Structure
* `tidymatrix()`: Create tidymatrix objects combining matrix data with row and column metadata
* `activate()`: Switch context between rows, columns, or matrix
* Automatic validation of matrix-metadata alignment

### dplyr Integration
* `filter()`: Filter rows or columns with corresponding matrix subsetting
* `select()`: Select metadata columns
* `mutate()`: Add or modify metadata columns
* `arrange()`: Reorder rows or columns
* `slice()`, `slice_head()`, `slice_tail()`, `slice_sample()`: Subset by position
* `rename()`: Rename metadata columns
* `pull()`: Extract metadata columns as vectors
* `relocate()`: Reorder metadata columns

### Grouping and Aggregation
* `group_by()`: Group by metadata variables (creates grouped_tidymatrix)
* `summarize()`: Aggregate grouped data with matrix aggregation
  - Default `mean()` for numeric matrices
  - Required `.matrix_fn` parameter for non-numeric matrices
  - Type-aware error messages with helpful suggestions
* `count()`: Count observations by group with matrix aggregation
* `tally()`: Count within existing groups
* `ungroup()`: Remove grouping

## Design Principles

1. **Tidy principle**: Same data type in, same data type out
2. **Explicit naming**: Clear, prefixed column names prevent conflicts
3. **Smart defaults**: Sensible defaults with explicit requirements where needed
4. **Helpful errors**: Type-aware error messages guide users
5. **Analysis objects**: Store full R objects (prcomp, hclust) for later use
6. **Metadata persistence**: Metadata columns persist even when analysis objects are invalidated

## Examples

See `examples/analysis_example.R` for a comprehensive demonstration of:
- PCA on samples with variance explained
- Hierarchical clustering of genes
- Multiple analyses with different parameters
- Analysis invalidation on data modification
- Lightweight vs. comprehensive analysis workflows
- Complex multi-step analysis pipelines

## Tests

* 256 tests covering all functionality
* Comprehensive test coverage for:
  - Core tidymatrix operations
  - All dplyr verbs
  - Grouping and summarization
  - Analysis integration
  - Analysis invalidation
