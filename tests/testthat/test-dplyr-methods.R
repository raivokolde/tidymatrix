library(dplyr)

# Test filter ----

test_that("filter works on rows", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4, group = c("A", "A", "B", "B"))
  tm <- tidymatrix(mat, row_data)

  tm_filtered <- tm %>%
    activate(rows) %>%
    filter(group == "A")

  expect_equal(nrow(tm_filtered$matrix), 2)
  expect_equal(nrow(tm_filtered$row_data), 2)
  expect_equal(tm_filtered$row_data$group, c("A", "A"))
  expect_equal(tm_filtered$matrix[, 1], c(1, 2))
})

test_that("filter works on columns", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  col_data <- data.frame(id = 1:3, type = c("x", "y", "x"))
  tm <- tidymatrix(mat, col_data = col_data)

  tm_filtered <- tm %>%
    activate(columns) %>%
    filter(type == "x")

  expect_equal(ncol(tm_filtered$matrix), 2)
  expect_equal(nrow(tm_filtered$col_data), 2)
  expect_equal(tm_filtered$col_data$type, c("x", "x"))
  expect_equal(tm_filtered$matrix[1, ], c(1, 9))
})

test_that("filter with multiple conditions", {
  mat <- matrix(1:20, nrow = 5, ncol = 4)
  row_data <- data.frame(
    id = 1:5,
    group = c("A", "A", "B", "B", "C"),
    score = c(10, 20, 30, 40, 50)
  )
  tm <- tidymatrix(mat, row_data)

  tm_filtered <- tm %>%
    activate(rows) %>%
    filter(group == "B", score > 25)

  expect_equal(nrow(tm_filtered$matrix), 2)
  expect_equal(tm_filtered$row_data$id, c(3, 4))
})

test_that("filter errors when matrix is active", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat)

  expect_error(
    filter(tm, TRUE),
    "Cannot filter when matrix is active"
  )
})

# Test select ----

test_that("select works on row metadata", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4, group = c("A", "A", "B", "B"), score = 1:4)
  tm <- tidymatrix(mat, row_data)

  tm_selected <- tm %>%
    activate(rows) %>%
    select(id, group)

  expect_equal(ncol(tm_selected$row_data), 2)
  expect_equal(names(tm_selected$row_data), c("id", "group"))
  expect_identical(tm_selected$matrix, mat)
})

test_that("select works on column metadata", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  col_data <- data.frame(id = 1:3, type = c("x", "y", "z"), required = c(T, F, T))
  tm <- tidymatrix(mat, col_data = col_data)

  tm_selected <- tm %>%
    activate(columns) %>%
    select(type)

  expect_equal(ncol(tm_selected$col_data), 1)
  expect_equal(names(tm_selected$col_data), "type")
  expect_identical(tm_selected$matrix, mat)
})

test_that("select with dplyr helpers", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4, group_a = 1:4, group_b = 5:8, other = 9:12)
  tm <- tidymatrix(mat, row_data)

  tm_selected <- tm %>%
    activate(rows) %>%
    select(starts_with("group"))

  expect_equal(ncol(tm_selected$row_data), 2)
  expect_equal(names(tm_selected$row_data), c("group_a", "group_b"))
})

test_that("select errors when matrix is active", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat)

  expect_error(
    select(tm, 1),
    "Cannot select when matrix is active"
  )
})

# Test mutate ----

test_that("mutate adds columns to row metadata", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4, value = c(10, 20, 30, 40))
  tm <- tidymatrix(mat, row_data)

  tm_mutated <- tm %>%
    activate(rows) %>%
    mutate(doubled = value * 2)

  expect_equal(ncol(tm_mutated$row_data), 3)
  expect_equal(tm_mutated$row_data$doubled, c(20, 40, 60, 80))
  expect_identical(tm_mutated$matrix, mat)
})

test_that("mutate modifies existing columns in row metadata", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4, value = c(10, 20, 30, 40))
  tm <- tidymatrix(mat, row_data)

  tm_mutated <- tm %>%
    activate(rows) %>%
    mutate(value = value + 5)

  expect_equal(tm_mutated$row_data$value, c(15, 25, 35, 45))
})

test_that("mutate works on column metadata", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  col_data <- data.frame(id = 1:3, weight = c(0.5, 1.0, 1.5))
  tm <- tidymatrix(mat, col_data = col_data)

  tm_mutated <- tm %>%
    activate(columns) %>%
    mutate(normalized = weight / sum(weight))

  expect_equal(ncol(tm_mutated$col_data), 3)
  expect_equal(
    tm_mutated$col_data$normalized,
    c(0.5, 1.0, 1.5) / 3.0
  )
})

test_that("mutate with multiple new columns", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4)
  tm <- tidymatrix(mat, row_data)

  tm_mutated <- tm %>%
    activate(rows) %>%
    mutate(
      squared = id^2,
      cubed = id^3
    )

  expect_equal(ncol(tm_mutated$row_data), 3)
  expect_equal(tm_mutated$row_data$squared, c(1, 4, 9, 16))
  expect_equal(tm_mutated$row_data$cubed, c(1, 8, 27, 64))
})

test_that("mutate errors when matrix is active", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat)

  expect_error(
    mutate(tm, x = 1),
    "Cannot mutate when matrix is active"
  )
})

# Test arrange ----

test_that("arrange reorders rows and matrix rows", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = c(4, 2, 3, 1), value = c(40, 20, 30, 10))
  tm <- tidymatrix(mat, row_data)

  tm_arranged <- tm %>%
    activate(rows) %>%
    arrange(value)

  expect_equal(tm_arranged$row_data$id, c(1, 2, 3, 4))
  expect_equal(tm_arranged$row_data$value, c(10, 20, 30, 40))
  expect_equal(tm_arranged$matrix[, 1], c(4, 2, 3, 1))
})

test_that("arrange reorders columns and matrix columns", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  col_data <- data.frame(id = c(3, 1, 2), priority = c(30, 10, 20))
  tm <- tidymatrix(mat, col_data = col_data)

  tm_arranged <- tm %>%
    activate(columns) %>%
    arrange(priority)

  expect_equal(tm_arranged$col_data$id, c(1, 2, 3))
  expect_equal(tm_arranged$col_data$priority, c(10, 20, 30))
  expect_equal(tm_arranged$matrix[1, ], c(5, 9, 1))
})

test_that("arrange with descending order", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4, value = c(10, 40, 20, 30))
  tm <- tidymatrix(mat, row_data)

  tm_arranged <- tm %>%
    activate(rows) %>%
    arrange(desc(value))

  expect_equal(tm_arranged$row_data$value, c(40, 30, 20, 10))
  expect_equal(tm_arranged$row_data$id, c(2, 4, 3, 1))
})

test_that("arrange with multiple columns", {
  mat <- matrix(1:20, nrow = 5, ncol = 4)
  row_data <- data.frame(
    id = 1:5,
    group = c("B", "A", "B", "A", "A"),
    value = c(30, 20, 10, 40, 50)
  )
  tm <- tidymatrix(mat, row_data)

  tm_arranged <- tm %>%
    activate(rows) %>%
    arrange(group, value)

  expect_equal(tm_arranged$row_data$id, c(2, 4, 5, 3, 1))
})

test_that("arrange errors when matrix is active", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat)

  expect_error(
    arrange(tm, 1),
    "Cannot arrange when matrix is active"
  )
})

# Test chaining operations ----

test_that("can chain multiple operations", {
  mat <- matrix(1:20, nrow = 5, ncol = 4)
  row_data <- data.frame(
    id = 1:5,
    group = c("A", "A", "B", "B", "C"),
    score = c(10, 30, 20, 40, 50)
  )
  col_data <- data.frame(
    id = 1:4,
    type = c("x", "y", "x", "z")
  )
  tm <- tidymatrix(mat, row_data, col_data)

  tm_result <- tm %>%
    activate(rows) %>%
    filter(group %in% c("A", "B")) %>%
    mutate(rank = rank(score)) %>%
    arrange(score) %>%
    activate(columns) %>%
    filter(type == "x")

  expect_equal(nrow(tm_result$matrix), 4)
  expect_equal(ncol(tm_result$matrix), 2)
  expect_equal(tm_result$row_data$score, c(10, 20, 30, 40))
  expect_equal(tm_result$row_data$rank, c(1, 2, 3, 4))
  expect_equal(tm_result$col_data$type, c("x", "x"))
})

test_that("operations preserve tidymatrix class", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4, group = c("A", "A", "B", "B"))
  tm <- tidymatrix(mat, row_data)

  tm_result <- tm %>%
    activate(rows) %>%
    filter(group == "A") %>%
    mutate(new_col = id * 2) %>%
    select(id, new_col)

  expect_s3_class(tm_result, "tidymatrix")
})
