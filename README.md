# tidymatrix

Tidyverse-style operations on matrices with row and column metadata.

## Overview

`tidymatrix` provides a unified data structure for working with matrices and their associated metadata. It's designed for scenarios where you have:

- A matrix of values (e.g., gene expression, survey responses, experimental measurements)
- Row metadata (e.g., gene annotations, demographic information)
- Column metadata (e.g., sample information, question details)

Instead of juggling three separate objects, `tidymatrix` combines them into a single object that supports tidyverse-style manipulation.

## Inspiration

This package is inspired by `tidygraph`'s approach to handling network data. Just as `tidygraph` allows you to activate and manipulate nodes or edges, `tidymatrix` lets you activate and work with rows, columns, or the matrix itself.

## Installation

```r
# Install from source (when available on CRAN, use install.packages)
devtools::install_local("path/to/tidymatrix")
```

## Core Concepts

### The tidymatrix object

A `tidymatrix` combines three components:
- **matrix**: Your numeric data
- **row_data**: A data.frame with one row per matrix row
- **col_data**: A data.frame with one row per matrix column

### Activation

Use `activate()` to switch between working on different components:
- `activate(rows)`: Work with row metadata
- `activate(columns)`: Work with column metadata
- `activate(matrix)`: Work with the matrix itself (future functionality)

### dplyr verbs

Once activated, you can use familiar dplyr verbs:
- `filter()`: Subset rows or columns based on metadata
- `select()`: Choose which metadata columns to keep
- `mutate()`: Add or modify metadata columns
- `arrange()`: Reorder rows or columns (and the matrix accordingly)

## Quick Example

```r
library(tidymatrix)
library(dplyr)

# Create sample survey data
responses <- matrix(sample(1:5, 40, replace = TRUE), nrow = 10, ncol = 4)

people <- data.frame(
  id = 1:10,
  age = sample(20:60, 10),
  group = rep(c("A", "B"), each = 5)
)

questions <- data.frame(
  id = 1:4,
  category = c("demo", "demo", "satisfaction", "satisfaction"),
  required = c(TRUE, TRUE, FALSE, FALSE)
)

# Create tidymatrix
tm <- tidymatrix(responses, people, questions)

# Filter to group A, satisfaction questions only
tm_filtered <- tm %>%
  activate(rows) %>%
  filter(group == "A") %>%
  activate(columns) %>%
  filter(category == "satisfaction")

print(tm_filtered)
```

## Use Cases

### 1. Survey/Questionnaire Analysis
- **Matrix**: Response values
- **Rows**: Respondent demographics
- **Columns**: Question metadata

### 2. Gene Expression Analysis
- **Matrix**: Expression values
- **Rows**: Gene annotations (symbols, pathways, chromosomes)
- **Columns**: Sample information (condition, batch, patient)

### 3. Experimental Data
- **Matrix**: Measurements
- **Rows**: Feature metadata
- **Columns**: Experimental conditions

## Comparison to Existing Solutions

### vs. SummarizedExperiment (Bioconductor)
- `tidymatrix` provides a tidyverse-friendly interface
- Simpler for users already familiar with dplyr
- Designed for general matrix+metadata use cases, not just genomics

### vs. Managing three objects manually
- No need to keep indices synchronized
- Operations automatically update all components
- Cleaner, more expressive code

## Future Development

Planned features:
- Matrix operations while preserving metadata
- Integration with ggplot2 for visualization
- Heatmap functionality (pheatmap-style with ggplot2)
- Support for joining tidymatrix objects
- Grouping and summarization operations

## Related Packages

- [tidygraph](https://tidygraph.data-imaginist.com/): Tidy API for graph manipulation
- [dplyr](https://dplyr.tidyverse.org/): Data manipulation grammar
- [SummarizedExperiment](https://bioconductor.org/packages/SummarizedExperiment/): Bioconductor container for matrix data

## License

MIT License - see LICENSE file for details

## Author

Raivo Kolde (rkolde@gmail.com)
