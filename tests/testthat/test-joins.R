test_that("left_join preserves all tidymatrix rows", {
  # Create test tidymatrix
  mat <- matrix(1:20, nrow = 4, ncol = 5)
  row_data <- data.frame(id = c("A", "B", "C", "D"), value = 1:4)
  col_data <- data.frame(sample = paste0("S", 1:5))
  tm <- tidymatrix(mat, row_data, col_data)

  # Create external data with partial matches
  external <- data.frame(id = c("A", "B", "E"), extra = c(10, 20, 30))

  # Left join
  result <- tm |>
    activate(rows) |>
    left_join(external, by = "id")

  # Check dimensions
  expect_equal(nrow(result$row_data), 4)
  expect_equal(nrow(result$matrix), 4)
  expect_equal(ncol(result$matrix), 5)

  # Check that all original rows are present
  expect_equal(result$row_data$id, c("A", "B", "C", "D"))

  # Check that extra column was added with NAs for non-matching rows
  expect_equal(result$row_data$extra[1:2], c(10, 20))
  expect_true(is.na(result$row_data$extra[3]))
  expect_true(is.na(result$row_data$extra[4]))

  # Check that matrix values are unchanged
  expect_equal(result$matrix, mat)
})

test_that("right_join keeps all rows from external data", {
  mat <- matrix(1:20, nrow = 4, ncol = 5)
  row_data <- data.frame(id = c("A", "B", "C", "D"), value = 1:4)
  col_data <- data.frame(sample = paste0("S", 1:5))
  tm <- tidymatrix(mat, row_data, col_data)

  # External data with some new rows
  external <- data.frame(id = c("A", "B", "E", "F"), extra = c(10, 20, 30, 40))

  result <- tm |>
    activate(rows) |>
    right_join(external, by = "id")

  # Check dimensions
  expect_equal(nrow(result$row_data), 4)
  expect_equal(nrow(result$matrix), 4)
  expect_equal(ncol(result$matrix), 5)

  # Check that we have external data rows
  expect_equal(result$row_data$id, c("A", "B", "E", "F"))
  expect_equal(result$row_data$extra, c(10, 20, 30, 40))

  # Check that original rows have their values
  expect_equal(result$matrix[1, ], mat[1, ])
  expect_equal(result$matrix[2, ], mat[2, ])

  # Check that new rows are filled with NA
  expect_true(all(is.na(result$matrix[3, ])))
  expect_true(all(is.na(result$matrix[4, ])))
})

test_that("inner_join keeps only matching rows", {
  mat <- matrix(1:20, nrow = 4, ncol = 5)
  row_data <- data.frame(id = c("A", "B", "C", "D"), value = 1:4)
  col_data <- data.frame(sample = paste0("S", 1:5))
  tm <- tidymatrix(mat, row_data, col_data)

  # External data with partial matches
  external <- data.frame(id = c("A", "C", "E"), extra = c(10, 30, 50))

  result <- tm |>
    activate(rows) |>
    inner_join(external, by = "id")

  # Check dimensions - only 2 matching rows
  expect_equal(nrow(result$row_data), 2)
  expect_equal(nrow(result$matrix), 2)
  expect_equal(ncol(result$matrix), 5)

  # Check that only matching rows are present
  expect_equal(result$row_data$id, c("A", "C"))
  expect_equal(result$row_data$extra, c(10, 30))

  # Check that matrix values are subset correctly
  expect_equal(result$matrix[1, ], mat[1, ])
  expect_equal(result$matrix[2, ], mat[3, ])
})

test_that("full_join combines all rows from both", {
  mat <- matrix(1:20, nrow = 4, ncol = 5)
  row_data <- data.frame(id = c("A", "B", "C", "D"), value = 1:4)
  col_data <- data.frame(sample = paste0("S", 1:5))
  tm <- tidymatrix(mat, row_data, col_data)

  # External data with some overlap
  external <- data.frame(id = c("B", "C", "E", "F"), extra = c(20, 30, 50, 60))

  result <- tm |>
    activate(rows) |>
    full_join(external, by = "id")

  # Check dimensions - 6 total rows (4 original + 2 new)
  expect_equal(nrow(result$row_data), 6)
  expect_equal(nrow(result$matrix), 6)
  expect_equal(ncol(result$matrix), 5)

  # Check that all IDs are present
  expect_setequal(result$row_data$id, c("A", "B", "C", "D", "E", "F"))

  # Check that original rows have their matrix values
  a_idx <- which(result$row_data$id == "A")
  expect_equal(result$matrix[a_idx, ], mat[1, ])

  # Check that new rows are filled with NA
  e_idx <- which(result$row_data$id == "E")
  expect_true(all(is.na(result$matrix[e_idx, ])))
})

test_that("semi_join filters without adding columns", {
  mat <- matrix(1:20, nrow = 4, ncol = 5)
  row_data <- data.frame(id = c("A", "B", "C", "D"), value = 1:4)
  col_data <- data.frame(sample = paste0("S", 1:5))
  tm <- tidymatrix(mat, row_data, col_data)

  # External data
  external <- data.frame(id = c("A", "C", "E"), extra = c(10, 30, 50))

  result <- tm |>
    activate(rows) |>
    semi_join(external, by = "id")

  # Check dimensions - only matching rows, no new columns
  expect_equal(nrow(result$row_data), 2)
  expect_equal(ncol(result$row_data), 2)  # Only id and value, no extra
  expect_equal(nrow(result$matrix), 2)
  expect_equal(ncol(result$matrix), 5)

  # Check that only matching rows are present
  expect_equal(result$row_data$id, c("A", "C"))
  expect_equal(result$row_data$value, c(1, 3))

  # Check that extra column was NOT added
  expect_false("extra" %in% names(result$row_data))

  # Check matrix values
  expect_equal(result$matrix[1, ], mat[1, ])
  expect_equal(result$matrix[2, ], mat[3, ])
})

test_that("anti_join filters to non-matching rows", {
  mat <- matrix(1:20, nrow = 4, ncol = 5)
  row_data <- data.frame(id = c("A", "B", "C", "D"), value = 1:4)
  col_data <- data.frame(sample = paste0("S", 1:5))
  tm <- tidymatrix(mat, row_data, col_data)

  # External data
  external <- data.frame(id = c("A", "C", "E"), extra = c(10, 30, 50))

  result <- tm |>
    activate(rows) |>
    anti_join(external, by = "id")

  # Check dimensions - only non-matching rows
  expect_equal(nrow(result$row_data), 2)
  expect_equal(ncol(result$row_data), 2)  # Only id and value, no extra
  expect_equal(nrow(result$matrix), 2)
  expect_equal(ncol(result$matrix), 5)

  # Check that only non-matching rows are present
  expect_equal(result$row_data$id, c("B", "D"))
  expect_equal(result$row_data$value, c(2, 4))

  # Check matrix values
  expect_equal(result$matrix[1, ], mat[2, ])
  expect_equal(result$matrix[2, ], mat[4, ])
})

test_that("joins work on columns when columns are active", {
  mat <- matrix(1:20, nrow = 4, ncol = 5)
  row_data <- data.frame(gene = paste0("G", 1:4))
  col_data <- data.frame(id = c("S1", "S2", "S3", "S4", "S5"), value = 1:5)
  tm <- tidymatrix(mat, row_data, col_data)

  # External data for columns
  external <- data.frame(id = c("S1", "S2", "S6"), extra = c(10, 20, 60))

  result <- tm |>
    activate(columns) |>
    inner_join(external, by = "id")

  # Check dimensions - only 2 matching columns
  expect_equal(nrow(result$matrix), 4)
  expect_equal(ncol(result$matrix), 2)
  expect_equal(nrow(result$col_data), 2)

  # Check that only matching columns are present
  expect_equal(result$col_data$id, c("S1", "S2"))
  expect_equal(result$col_data$extra, c(10, 20))

  # Check matrix values
  expect_equal(result$matrix[, 1], mat[, 1])
  expect_equal(result$matrix[, 2], mat[, 2])
})

test_that("joins invalidate analyses", {
  mat <- matrix(1:20, nrow = 4, ncol = 5)
  row_data <- data.frame(id = c("A", "B", "C", "D"))
  col_data <- data.frame(sample = paste0("S", 1:5))
  tm <- tidymatrix(mat, row_data, col_data)

  # Add a PCA analysis to test invalidation
  tm <- tm |> activate(rows) |> compute_prcomp(n_components = 2)

  # Verify analysis exists
  expect_true(length(list_analyses(tm)) > 0)

  # Perform join
  external <- data.frame(id = c("A", "B"), extra = c(10, 20))
  result <- tm |>
    activate(rows) |>
    left_join(external, by = "id")

  # Check that analyses were invalidated
  expect_equal(length(list_analyses(result)), 0)
})

test_that("joins handle duplicate matches correctly", {
  mat <- matrix(1:15, nrow = 3, ncol = 5)
  row_data <- data.frame(id = c("A", "B", "C"), value = 1:3)
  col_data <- data.frame(sample = paste0("S", 1:5))
  tm <- tidymatrix(mat, row_data, col_data)

  # External data with duplicate IDs
  external <- data.frame(id = c("A", "A", "B"), extra = c(10, 11, 20))

  result <- tm |>
    activate(rows) |>
    left_join(external, by = "id")

  # Check dimensions - A should be duplicated
  expect_equal(nrow(result$row_data), 4)  # A, A, B, C
  expect_equal(nrow(result$matrix), 4)

  # Check that A appears twice
  expect_equal(sum(result$row_data$id == "A"), 2)

  # Check that matrix rows are duplicated
  a_indices <- which(result$row_data$id == "A")
  expect_equal(result$matrix[a_indices[1], ], mat[1, ])
  expect_equal(result$matrix[a_indices[2], ], mat[1, ])
})

test_that("joins handle empty data.frames", {
  mat <- matrix(1:20, nrow = 4, ncol = 5)
  row_data <- data.frame(id = c("A", "B", "C", "D"), value = 1:4)
  col_data <- data.frame(sample = paste0("S", 1:5))
  tm <- tidymatrix(mat, row_data, col_data)

  # Empty external data
  external <- data.frame(id = character(0), extra = numeric(0))

  # Left join with empty data
  result <- tm |>
    activate(rows) |>
    left_join(external, by = "id")

  # Should keep all original rows
  expect_equal(nrow(result$row_data), 4)
  expect_equal(nrow(result$matrix), 4)

  # extra column should be added but all NA
  expect_true("extra" %in% names(result$row_data))
  expect_true(all(is.na(result$row_data$extra)))
})

test_that("joins use suffix for conflicting column names", {
  mat <- matrix(1:20, nrow = 4, ncol = 5)
  row_data <- data.frame(id = c("A", "B", "C", "D"), value = 1:4)
  col_data <- data.frame(sample = paste0("S", 1:5))
  tm <- tidymatrix(mat, row_data, col_data)

  # External data with conflicting column name
  external <- data.frame(id = c("A", "B", "C", "D"), value = 10:13)

  result <- tm |>
    activate(rows) |>
    left_join(external, by = "id", suffix = c("_orig", "_new"))

  # Check that both value columns exist with suffixes
  expect_true("value_orig" %in% names(result$row_data))
  expect_true("value_new" %in% names(result$row_data))
  expect_equal(result$row_data$value_orig, 1:4)
  expect_equal(result$row_data$value_new, 10:13)
})

test_that("joins work with by = NULL (auto-detect)", {
  mat <- matrix(1:20, nrow = 4, ncol = 5)
  row_data <- data.frame(id = c("A", "B", "C", "D"), value = 1:4)
  col_data <- data.frame(sample = paste0("S", 1:5))
  tm <- tidymatrix(mat, row_data, col_data)

  # External data with common column
  external <- data.frame(id = c("A", "B"), extra = c(10, 20))

  # Join without specifying by - should auto-detect "id"
  result <- tm |>
    activate(rows) |>
    inner_join(external)

  expect_equal(nrow(result$row_data), 2)
  expect_equal(result$row_data$id, c("A", "B"))
})

test_that("joins error when matrix is active", {
  mat <- matrix(1:20, nrow = 4, ncol = 5)
  row_data <- data.frame(id = c("A", "B", "C", "D"))
  col_data <- data.frame(sample = paste0("S", 1:5))
  tm <- tidymatrix(mat, row_data, col_data)

  external <- data.frame(id = c("A", "B"), extra = c(10, 20))

  # Should error when matrix is active
  expect_error(
    left_join(tm, external, by = "id"),
    "Cannot join when matrix is active"
  )
})

test_that("joins error with invalid inputs", {
  mat <- matrix(1:20, nrow = 4, ncol = 5)
  row_data <- data.frame(id = c("A", "B", "C", "D"))
  col_data <- data.frame(sample = paste0("S", 1:5))
  tm <- tidymatrix(mat, row_data, col_data)

  # Should error with non-data.frame
  expect_error(
    tm |> activate(rows) |> left_join("not a dataframe"),
    "must be a data.frame"
  )
})

test_that("full_join with no matches creates all NA rows", {
  mat <- matrix(1:20, nrow = 4, ncol = 5)
  row_data <- data.frame(id = c("A", "B", "C", "D"), value = 1:4)
  col_data <- data.frame(sample = paste0("S", 1:5))
  tm <- tidymatrix(mat, row_data, col_data)

  # External data with no matches
  external <- data.frame(id = c("E", "F"), extra = c(50, 60))

  result <- tm |>
    activate(rows) |>
    full_join(external, by = "id")

  # Check dimensions
  expect_equal(nrow(result$row_data), 6)
  expect_equal(nrow(result$matrix), 6)

  # Check that new rows are all NA
  e_idx <- which(result$row_data$id == "E")
  f_idx <- which(result$row_data$id == "F")
  expect_true(all(is.na(result$matrix[e_idx, ])))
  expect_true(all(is.na(result$matrix[f_idx, ])))

  # Check that original rows have NA for extra
  a_idx <- which(result$row_data$id == "A")
  expect_true(is.na(result$row_data$extra[a_idx]))
})

test_that("inner_join with no matches creates empty tidymatrix", {
  mat <- matrix(1:20, nrow = 4, ncol = 5)
  row_data <- data.frame(id = c("A", "B", "C", "D"))
  col_data <- data.frame(sample = paste0("S", 1:5))
  tm <- tidymatrix(mat, row_data, col_data)

  # External data with no matches
  external <- data.frame(id = c("E", "F"), extra = c(50, 60))

  result <- tm |>
    activate(rows) |>
    inner_join(external, by = "id")

  # Check dimensions - should be empty
  expect_equal(nrow(result$row_data), 0)
  expect_equal(nrow(result$matrix), 0)
  expect_equal(ncol(result$matrix), 5)  # columns preserved
})

test_that("joins preserve column names in matrix", {
  mat <- matrix(1:20, nrow = 4, ncol = 5)
  colnames(mat) <- paste0("col", 1:5)
  row_data <- data.frame(id = c("A", "B", "C", "D"))
  col_data <- data.frame(sample = paste0("S", 1:5))
  tm <- tidymatrix(mat, row_data, col_data)

  external <- data.frame(id = c("A", "B"), extra = c(10, 20))

  result <- tm |>
    activate(rows) |>
    inner_join(external, by = "id")

  # Check that column names are preserved
  expect_equal(colnames(result$matrix), colnames(mat))
})

test_that("joins preserve row names in matrix", {
  mat <- matrix(1:20, nrow = 4, ncol = 5)
  rownames(mat) <- paste0("row", 1:4)
  row_data <- data.frame(id = c("A", "B", "C", "D"))
  col_data <- data.frame(sample = paste0("S", 1:5))
  tm <- tidymatrix(mat, row_data, col_data)

  external <- data.frame(sample = c("S1", "S2"), extra = c(10, 20))

  result <- tm |>
    activate(columns) |>
    inner_join(external, by = "sample")

  # Check that row names are preserved (for column operations)
  expect_equal(rownames(result$matrix), rownames(mat))
})

test_that("column joins expand matrix with NA columns", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(gene = paste0("G", 1:4))
  col_data <- data.frame(id = c("S1", "S2", "S3"), value = 1:3)
  tm <- tidymatrix(mat, row_data, col_data)

  # External data with new samples
  external <- data.frame(id = c("S1", "S2", "S4", "S5"), extra = c(10, 20, 40, 50))

  result <- tm |>
    activate(columns) |>
    full_join(external, by = "id")

  # Check dimensions
  expect_equal(ncol(result$matrix), 5)  # 3 original + 2 new
  expect_equal(nrow(result$matrix), 4)

  # Find new column indices
  s4_idx <- which(result$col_data$id == "S4")
  s5_idx <- which(result$col_data$id == "S5")

  # Check that new columns are all NA
  expect_true(all(is.na(result$matrix[, s4_idx])))
  expect_true(all(is.na(result$matrix[, s5_idx])))
})
