# Grouping and Summarization Design Plan

## The Challenge

When using `group_by()` and `summarize()` on a tidymatrix, we need to decide how to handle the matrix data. Unlike regular data frames, collapsing grouped rows/columns requires aggregating the underlying matrix values.

## Use Cases

### 1. Gene Expression: Average by Condition

```r
# Matrix: genes (rows) x samples (columns)
# Column metadata: sample_id, condition, batch
# Goal: Average expression per condition

expr_tm %>%
  activate(columns) %>%
  group_by(condition) %>%
  summarize(n_samples = n())
```

**Expected result:** Matrix with same genes (rows) but columns collapsed to one per condition.

**Matrix handling:** Average expression values across samples within each condition.

### 2. Survey Data: Group Demographics

```r
# Matrix: people (rows) x questions (columns)
# Row metadata: person_id, age, education, gender
# Goal: Average responses by age group

survey_tm %>%
  activate(rows) %>%
  group_by(age_group = cut(age, breaks = c(0, 30, 50, 100))) %>%
  summarize(
    n = n(),
    avg_age = mean(age)
  )
```

**Expected result:** Matrix with rows collapsed to age groups, same questions (columns).

**Matrix handling:** Average survey responses across people within each age group.

### 3. Time Series: Aggregate by Time Period

```r
# Matrix: features (rows) x timepoints (columns)
# Column metadata: timestamp, hour, day, week
# Goal: Daily averages

timeseries_tm %>%
  activate(columns) %>%
  group_by(day) %>%
  summarize(
    n_timepoints = n(),
    start_time = min(timestamp)
  )
```

**Expected result:** Matrix with features (rows) but columns collapsed to daily values.

**Matrix handling:** Aggregate feature values within each day.

## Design Options

### Option 1: Default Aggregation (mean)

**Approach:** Always use `mean()` for matrix aggregation by default.

```r
tm %>%
  activate(rows) %>%
  group_by(group) %>%
  summarize(n = n())
# Matrix automatically averaged across grouped rows
```

**Pros:**
- Simple and intuitive for most use cases
- Matches common analysis patterns (averaging replicates, conditions, etc.)
- Minimal user input needed

**Cons:**
- Not flexible for other aggregation needs (sum, median, etc.)
- What if matrix has NA values? Need na.rm = TRUE?
- Doesn't work for non-numeric matrices

### Option 2: User-Specified Matrix Function

**Approach:** Allow user to specify how to aggregate the matrix.

```r
tm %>%
  activate(rows) %>%
  group_by(group) %>%
  summarize(
    n = n(),
    .matrix_fn = mean
  )

# Or with additional arguments
tm %>%
  activate(rows) %>%
  group_by(group) %>%
  summarize(
    n = n(),
    .matrix_fn = list(fn = mean, na.rm = TRUE)
  )
```

**Pros:**
- Flexible - supports mean, median, sum, max, min, etc.
- User explicitly states their intention
- Can pass additional arguments (na.rm, etc.)

**Cons:**
- More verbose
- Extra parameter to remember
- What's the default if not specified?

### Option 3: Separate Matrix Summarization

**Approach:** Use normal `summarize()` for metadata, separate function for matrix.

```r
tm %>%
  activate(rows) %>%
  group_by(group) %>%
  summarize(n = n()) %>%
  summarize_matrix(mean)

# Or combined
tm %>%
  activate(rows) %>%
  group_by(group) %>%
  summarize_all(
    metadata = list(n = n(), avg_age = mean(age)),
    matrix = mean
  )
```

**Pros:**
- Clear separation of concerns
- Could allow more complex matrix operations
- Could have different functions: `collapse_matrix()`, `aggregate_matrix()`

**Cons:**
- Two-step process feels less fluent
- Breaks the dplyr pattern

### Option 4: Column-wise Matrix Summarization

**Approach:** Allow summarizing matrix columns individually within summarize().

```r
tm %>%
  activate(rows) %>%
  group_by(age_group) %>%
  summarize(
    n = n(),
    avg_age = mean(age),
    # Special syntax for matrix columns
    mean_q1 = mean_matrix_col(1),
    sum_q2 = sum_matrix_col(2)
  )
```

**Pros:**
- Maximum flexibility
- Can apply different functions to different matrix columns/rows
- Fits naturally into dplyr pattern

**Cons:**
- Verbose for matrices with many columns
- Need special functions for matrix access
- Less intuitive

### Option 5: Smart Default with Override

**Approach:** Use mean by default, allow override when needed.

```r
# Default: mean aggregation
tm %>%
  activate(rows) %>%
  group_by(group) %>%
  summarize(n = n())

# Override when needed
tm %>%
  activate(rows) %>%
  group_by(group) %>%
  summarize(n = n(), .matrix_fn = median)

# Or don't aggregate matrix at all (keep first)
tm %>%
  activate(rows) %>%
  group_by(group) %>%
  summarize(n = n(), .matrix_fn = first)
```

**Pros:**
- Best of both worlds - simple default, flexible when needed
- Common case is easy, complex cases are possible
- Clear and explicit

**Cons:**
- Need to handle edge cases (what if mean doesn't make sense?)
- Need good defaults for NA handling

## Recommendation: Option 5 (Smart Default with Override)

### Proposed Implementation

```r
group_by.tidymatrix <- function(.data, ...) {
  # Store grouping variables
  # Return grouped_tidymatrix object
}

summarize.grouped_tidymatrix <- function(.data, ..., .matrix_fn = NULL, .matrix_args = list()) {
  # 1. Identify groups in active metadata
  # 2. Summarize metadata as specified
  # 3. Determine matrix aggregation function:
  #    - If .matrix_fn is NULL:
  #        - Check if matrix is numeric
  #        - If numeric: use mean as default
  #        - If not numeric: throw informative error
  #    - Else: use provided .matrix_fn
  # 4. Aggregate matrix using determined function:
  #    - If rows active: aggregate rows within each group
  #    - If columns active: aggregate columns within each group
  # 5. Return ungrouped tidymatrix
}
```

### Key Features

1. **Default behavior for numeric matrices:** Use `mean()` for matrix aggregation
   ```r
   # Numeric matrix - works automatically
   tm %>% activate(rows) %>% group_by(group) %>% summarize(n = n())
   # Equivalent to .matrix_fn = mean

   # Non-numeric matrix - requires explicit function
   logical_tm %>% activate(rows) %>% group_by(group) %>% summarize(n = n())
   # Error: Cannot use default aggregation on logical matrix. Please specify .matrix_fn

   logical_tm %>% activate(rows) %>% group_by(group) %>%
     summarize(n = n(), .matrix_fn = any)
   # Works!
   ```

2. **Custom aggregation:**
   ```r
   tm %>% activate(rows) %>% group_by(group) %>%
     summarize(n = n(), .matrix_fn = median)
   ```

3. **Aggregation with arguments:**
   ```r
   tm %>% activate(rows) %>% group_by(group) %>%
     summarize(n = n(), .matrix_fn = mean, .matrix_args = list(na.rm = TRUE))
   ```

4. **No aggregation (keep first):**
   ```r
   tm %>% activate(rows) %>% group_by(group) %>%
     summarize(n = n(), .matrix_fn = first)
   ```

5. **Custom function:**
   ```r
   tm %>% activate(rows) %>% group_by(group) %>%
     summarize(n = n(), .matrix_fn = function(x) median(x, na.rm = TRUE))
   ```

### Matrix Aggregation Behavior

**When rows are active:**
- Group rows based on row_data
- Aggregate matrix rows within each group
- Result: fewer rows, same columns
- Each resulting row contains aggregated values from all rows in that group

**When columns are active:**
- Group columns based on col_data
- Aggregate matrix columns within each group
- Result: same rows, fewer columns
- Each resulting column contains aggregated values from all columns in that group

### Example Workflows

#### Gene Expression: Average Replicates

```r
# Original: 1000 genes x 12 samples (4 conditions, 3 replicates each)
expr_tm %>%
  activate(columns) %>%
  group_by(condition) %>%
  summarize(
    n_replicates = n(),
    batch = first(batch)
  )
# Result: 1000 genes x 4 samples (one per condition)
# Matrix values: mean expression across replicates
```

#### Survey: Demographics Summary

```r
# Original: 500 people x 20 questions
survey_tm %>%
  activate(rows) %>%
  mutate(age_group = cut(age, breaks = c(0, 30, 50, 100))) %>%
  group_by(age_group, gender) %>%
  summarize(
    n = n(),
    avg_age = mean(age),
    .matrix_fn = mean
  )
# Result: 6 groups (3 age groups x 2 genders) x 20 questions
# Matrix values: average response per group
```

#### Time Series: Daily Aggregation with Sum

```r
# Original: 50 features x 1440 timepoints (minute-level data)
ts_tm %>%
  activate(columns) %>%
  group_by(day) %>%
  summarize(
    n_minutes = n(),
    start = min(timestamp),
    .matrix_fn = sum  # Daily totals instead of averages
  )
# Result: 50 features x days
# Matrix values: sum of feature values per day
```

## Implementation Considerations

### 1. NA Handling

Default behavior should match base R:
- `mean()` without `na.rm` returns NA if any NA present
- User can override: `.matrix_args = list(na.rm = TRUE)`

### 2. Non-numeric Matrices

**Decision: No default for non-numeric matrices. Require explicit `.matrix_fn`.**

Behavior:
- Detect matrix type before aggregation
- If numeric: use `mean()` as default
- If logical/character/factor: require user to specify `.matrix_fn`
- Throw informative error with suggestions

Example error message:
```
Error: Cannot use default aggregation (mean) on non-numeric matrix.
Matrix type: logical
Please specify .matrix_fn explicitly. Suggestions for logical matrices:
  - any: TRUE if any value is TRUE
  - all: TRUE if all values are TRUE
  - mean: proportion of TRUE values
  - first: take first value in each group
```

Suggested functions by type:
- **Logical matrices:**
  - `any()` - TRUE if any TRUE in group
  - `all()` - TRUE if all TRUE in group
  - `mean()` - proportion of TRUE (if this makes sense conceptually)
  - `sum()` - count of TRUE values
  - `first()` - first value in group

- **Character/factor matrices:**
  - `first()` - first value in group
  - `last()` - last value in group
  - Custom mode function - most common value
  - `paste()` with collapse - concatenate values

Example usage:
```r
# Logical matrix: presence/absence data
presence_tm %>%
  activate(columns) %>%
  group_by(condition) %>%
  summarize(
    n = n(),
    .matrix_fn = any  # TRUE if present in any replicate
  )

# Character matrix: annotations
annotation_tm %>%
  activate(rows) %>%
  group_by(cluster) %>%
  summarize(
    n = n(),
    .matrix_fn = first  # Take first annotation
  )

# Or use a custom function for mode (most common value)
mode_fn <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

annotation_tm %>%
  activate(rows) %>%
  group_by(cluster) %>%
  summarize(
    n = n(),
    .matrix_fn = mode_fn  # Most common annotation
  )
```

### 3. Grouping Variables

- Allow multiple grouping variables (like dplyr)
- Create one group per unique combination
- Example: `group_by(condition, batch)` creates condition-batch groups

### 4. Empty Groups

- Follow dplyr behavior: `.drop = TRUE` by default (drop empty groups)
- Allow `.drop = FALSE` to keep empty groups

### 5. Preserve vs. Destroy

- After `summarize()`, should result be grouped or ungrouped?
- Follow dplyr: ungrouped by default
- Could add `.groups` parameter for control

### 6. Performance

- For large matrices, aggregation could be slow
- Use efficient matrix operations (colMeans, rowMeans when possible)
- Consider sparse matrix support in future

## Alternative: count() and tally()

For simple counting without matrix aggregation:

```r
# count() creates frequency table, discards matrix
tm %>%
  activate(rows) %>%
  count(group, gender)
# Returns: tibble with counts, no matrix

# tally() counts within groups, keeps matrix
tm %>%
  activate(rows) %>%
  group_by(group) %>%
  tally()
# Returns: tidymatrix with aggregated matrix (using default mean)
```

## Design Decisions Summary

### ✓ Resolved
- **Non-numeric matrices:** No default aggregation. Require explicit `.matrix_fn` with helpful error messages.
- **Numeric matrices:** Use `mean()` as default, allow override.

### Open Questions

1. **Does `mean()` as default make sense for numeric matrices in your use cases?**
   - Gene expression: Yes (average replicates)
   - Survey data: Yes (average responses)
   - Other domains?

2. **Should we support different aggregation per matrix column/row?**
   - Current plan: One function for entire matrix
   - Alternative: Allow specifying different functions for different columns
   - Trade-off: Simplicity vs. flexibility

3. **Should `count()` return a tibble or tidymatrix?**
   - Option A: Return simple tibble (no matrix) - matches dplyr behavior
   - Option B: Return tidymatrix with aggregated matrix
   - Recommendation: Return tibble (simpler, matches expectations)

4. **Any aggregation functions that should be built-in helpers?**
   - Currently: User provides any function via `.matrix_fn`
   - Could add: `collapse_rows()`, `collapse_columns()` helpers
   - Or special functions: `summarize_mean()`, `summarize_median()`, etc.

## Next Steps

1. Discuss and refine this plan
2. Implement `group_by()` to create grouped_tidymatrix class
3. Implement `summarize()` for grouped objects
4. Add comprehensive tests for various aggregation scenarios
5. Document with clear examples
6. Consider adding helper functions like `collapse_rows()`, `collapse_columns()`
