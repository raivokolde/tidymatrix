library(dplyr)

# Test group_by ----

test_that("group_by works on rows", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4, group = c("A", "A", "B", "B"))
  tm <- tidymatrix(mat, row_data)

  tm_grouped <- tm %>%
    activate(rows) %>%
    group_by(group)

  expect_s3_class(tm_grouped, "grouped_tidymatrix")
  expect_true(is_grouped_tidymatrix(tm_grouped))
  expect_equal(group_vars(tm_grouped), "group")
})

test_that("group_by works on columns", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  col_data <- data.frame(id = 1:3, type = c("x", "y", "x"))
  tm <- tidymatrix(mat, col_data = col_data)

  tm_grouped <- tm %>%
    activate(columns) %>%
    group_by(type)

  expect_s3_class(tm_grouped, "grouped_tidymatrix")
  expect_equal(group_vars(tm_grouped), "type")
})

test_that("group_by with multiple variables works", {
  mat <- matrix(1:20, nrow = 5, ncol = 4)
  row_data <- data.frame(
    id = 1:5,
    group = c("A", "A", "B", "B", "C"),
    subgroup = c("x", "y", "x", "y", "x")
  )
  tm <- tidymatrix(mat, row_data)

  tm_grouped <- tm %>%
    activate(rows) %>%
    group_by(group, subgroup)

  expect_equal(group_vars(tm_grouped), c("group", "subgroup"))
})

test_that("group_by errors when matrix is active", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat)

  expect_error(
    group_by(tm, x),
    "Cannot group_by when matrix is active"
  )
})

test_that("ungroup removes grouping", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4, group = c("A", "A", "B", "B"))
  tm <- tidymatrix(mat, row_data)

  tm_grouped <- tm %>%
    activate(rows) %>%
    group_by(group)

  tm_ungrouped <- ungroup(tm_grouped)

  expect_false(is_grouped_tidymatrix(tm_ungrouped))
  expect_s3_class(tm_ungrouped, "tidymatrix")
  expect_equal(group_vars(tm_ungrouped), character(0))
})

test_that("print.grouped_tidymatrix shows grouping info", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4, group = c("A", "A", "B", "B"))
  tm <- tidymatrix(mat, row_data)

  tm_grouped <- tm %>%
    activate(rows) %>%
    group_by(group)

  expect_output(print(tm_grouped), "grouped tidymatrix")
  expect_output(print(tm_grouped), "Groups: group")
  expect_output(print(tm_grouped), "Number of groups: 2")
})

# Test summarize ----

test_that("summarize aggregates numeric matrix with default mean", {
  mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
  row_data <- data.frame(id = 1:3, group = c("A", "A", "B"))
  tm <- tidymatrix(mat, row_data)

  result <- tm %>%
    activate(rows) %>%
    group_by(group) %>%
    summarize(n = n())

  expect_s3_class(result, "tidymatrix")
  expect_false(is_grouped_tidymatrix(result))
  expect_equal(nrow(result$matrix), 2)
  expect_equal(result$row_data$n, c(2, 1))
  expect_equal(result$matrix[1, 1], mean(c(1, 2)))
  expect_equal(result$matrix[1, 2], mean(c(4, 5)))
})

test_that("summarize with custom matrix function works", {
  mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
  row_data <- data.frame(id = 1:3, group = c("A", "A", "B"))
  tm <- tidymatrix(mat, row_data)

  result <- tm %>%
    activate(rows) %>%
    group_by(group) %>%
    summarize(n = n(), .matrix_fn = sum)

  expect_equal(result$matrix[1, 1], sum(c(1, 2)))
  expect_equal(result$matrix[1, 2], sum(c(4, 5)))
})

test_that("summarize on columns aggregates column-wise", {
  mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
  # Matrix is:
  #      [,1] [,2] [,3]
  # [1,]    1    3    5
  # [2,]    2    4    6
  col_data <- data.frame(id = 1:3, type = c("x", "y", "x"))
  tm <- tidymatrix(mat, col_data = col_data)

  result <- tm %>%
    activate(columns) %>%
    group_by(type) %>%
    summarize(n = n())

  expect_equal(ncol(result$matrix), 2)  # x and y
  expect_equal(nrow(result$matrix), 2)  # same rows
  expect_equal(result$col_data$n, c(2, 1))
  # Type x groups columns 1 and 3: row 1 has values 1 and 5, mean = 3
  expect_equal(result$matrix[1, 1], mean(c(1, 5)))
})

test_that("summarize requires .matrix_fn for logical matrix", {
  mat <- matrix(c(TRUE, FALSE, TRUE, FALSE), nrow = 2, ncol = 2)
  row_data <- data.frame(id = 1:2, group = c("A", "A"))
  tm <- tidymatrix(mat, row_data)

  expect_error(
    tm %>% activate(rows) %>% group_by(group) %>% summarize(n = n()),
    "Cannot use default aggregation.*on non-numeric matrix"
  )
})

test_that("summarize with .matrix_fn works on logical matrix", {
  mat <- matrix(c(TRUE, FALSE, TRUE, FALSE), nrow = 2, ncol = 2)
  # Matrix is:
  #       [,1]  [,2]
  # [1,] TRUE  TRUE
  # [2,] FALSE FALSE
  row_data <- data.frame(id = 1:2, group = c("A", "A"))
  tm <- tidymatrix(mat, row_data)

  result <- tm %>%
    activate(rows) %>%
    group_by(group) %>%
    summarize(n = n(), .matrix_fn = any)

  expect_equal(nrow(result$matrix), 1)
  expect_equal(result$matrix[1, 1], TRUE)   # any(c(TRUE, FALSE))
  expect_equal(result$matrix[1, 2], TRUE)   # any(c(TRUE, FALSE))
})

test_that("summarize with .matrix_args works", {
  mat <- matrix(c(1, 2, NA, 4, 5, 6), nrow = 3, ncol = 2)
  # Matrix is:
  #      [,1] [,2]
  # [1,]    1    4
  # [2,]    2    5
  # [3,]   NA    6
  row_data <- data.frame(id = 1:3, group = c("A", "A", "B"))
  tm <- tidymatrix(mat, row_data)

  result <- tm %>%
    activate(rows) %>%
    group_by(group) %>%
    summarize(n = n(), .matrix_args = list(na.rm = TRUE))

  # Group A (rows 1-2): mean(c(1, 2), na.rm=TRUE) = 1.5
  expect_equal(result$matrix[1, 1], mean(c(1, 2), na.rm = TRUE))
  # Group B (row 3): has single value NA, mean(NA, na.rm=TRUE) = NaN or NA
  expect_true(is.na(result$matrix[2, 1]) || is.nan(result$matrix[2, 1]))
})

test_that("summarize with multiple grouping variables", {
  mat <- matrix(1:20, nrow = 5, ncol = 4)
  row_data <- data.frame(
    id = 1:5,
    group = c("A", "A", "B", "B", "B"),
    subgroup = c("x", "y", "x", "y", "x")
  )
  tm <- tidymatrix(mat, row_data)

  result <- tm %>%
    activate(rows) %>%
    group_by(group, subgroup) %>%
    summarize(n = n())

  expect_equal(nrow(result$matrix), 4)  # A-x, A-y, B-x, B-y
  expect_equal(result$row_data$n, c(1, 1, 2, 1))
})

test_that("summarize with character matrix and first function", {
  mat <- matrix(c("a", "b", "c", "d"), nrow = 2, ncol = 2)
  row_data <- data.frame(id = 1:2, group = c("A", "A"))
  tm <- tidymatrix(mat, row_data)

  result <- tm %>%
    activate(rows) %>%
    group_by(group) %>%
    summarize(n = n(), .matrix_fn = dplyr::first)

  expect_equal(nrow(result$matrix), 1)
  expect_equal(result$matrix[1, 1], "a")
  expect_equal(result$matrix[1, 2], "c")
})

# Test count ----

test_that("count works and returns tidymatrix", {
  mat <- matrix(1:12, nrow = 6, ncol = 2)
  row_data <- data.frame(
    id = 1:6,
    group = c("A", "A", "B", "B", "C", "C")
  )
  tm <- tidymatrix(mat, row_data)

  result <- tm %>%
    activate(rows) %>%
    count(group)

  expect_s3_class(result, "tidymatrix")
  expect_equal(nrow(result$matrix), 3)
  expect_equal(result$row_data$n, c(2, 2, 2))
  expect_equal(result$row_data$group, c("A", "B", "C"))
})

test_that("count with multiple variables", {
  mat <- matrix(1:20, nrow = 10, ncol = 2)
  row_data <- data.frame(
    id = 1:10,
    group = rep(c("A", "B"), each = 5),
    subgroup = rep(c("x", "y"), 5)
  )
  tm <- tidymatrix(mat, row_data)

  result <- tm %>%
    activate(rows) %>%
    count(group, subgroup)

  expect_equal(nrow(result$matrix), 4)
  expect_true(all(result$row_data$n > 0))
})

test_that("count with sort works", {
  mat <- matrix(1:12, nrow = 6, ncol = 2)
  row_data <- data.frame(
    id = 1:6,
    group = c("A", "A", "A", "B", "C", "C")
  )
  tm <- tidymatrix(mat, row_data)

  result <- tm %>%
    activate(rows) %>%
    count(group, sort = TRUE)

  expect_equal(result$row_data$group[1], "A")  # Most frequent
  expect_equal(result$row_data$n[1], 3)
})

test_that("count with custom .matrix_fn", {
  mat <- matrix(1:12, nrow = 6, ncol = 2)
  row_data <- data.frame(
    id = 1:6,
    group = c("A", "A", "B", "B", "C", "C")
  )
  tm <- tidymatrix(mat, row_data)

  result <- tm %>%
    activate(rows) %>%
    count(group, .matrix_fn = sum)

  expect_equal(result$matrix[1, 1], sum(c(1, 2)))
})

# Test tally ----

test_that("tally works on grouped tidymatrix", {
  mat <- matrix(1:12, nrow = 6, ncol = 2)
  row_data <- data.frame(
    id = 1:6,
    group = c("A", "A", "B", "B", "C", "C")
  )
  tm <- tidymatrix(mat, row_data)

  result <- tm %>%
    activate(rows) %>%
    group_by(group) %>%
    tally()

  expect_s3_class(result, "tidymatrix")
  expect_equal(nrow(result$matrix), 3)
  expect_equal(result$row_data$n, c(2, 2, 2))
})

test_that("tally with custom name", {
  mat <- matrix(1:12, nrow = 6, ncol = 2)
  row_data <- data.frame(
    id = 1:6,
    group = c("A", "A", "B", "B", "C", "C")
  )
  tm <- tidymatrix(mat, row_data)

  result <- tm %>%
    activate(rows) %>%
    group_by(group) %>%
    tally(name = "count")

  expect_true("count" %in% names(result$row_data))
  expect_equal(result$row_data$count, c(2, 2, 2))
})

# Test complex chaining ----

test_that("complex chain with grouping and summarize", {
  mat <- matrix(rnorm(40), nrow = 10, ncol = 4)
  row_data <- data.frame(
    id = 1:10,
    condition = rep(c("ctrl", "treat"), each = 5),
    batch = rep(1:2, 5)
  )
  col_data <- data.frame(
    id = 1:4,
    type = c("A", "B", "A", "B")
  )
  tm <- tidymatrix(mat, row_data, col_data)

  result <- tm %>%
    activate(rows) %>%
    filter(batch == 1) %>%
    group_by(condition) %>%
    summarize(n = n(), .matrix_fn = median) %>%
    activate(columns) %>%
    filter(type == "A")

  expect_s3_class(result, "tidymatrix")
  expect_equal(nrow(result$matrix), 2)  # ctrl and treat
  expect_equal(ncol(result$matrix), 2)  # type A only
})
