library(dplyr)

# Test slice ----

test_that("slice works on rows by position", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4, value = c(10, 20, 30, 40))
  tm <- tidymatrix(mat, row_data)

  tm_sliced <- tm %>%
    activate(rows) %>%
    slice(2, 3)

  expect_equal(nrow(tm_sliced$matrix), 2)
  expect_equal(nrow(tm_sliced$row_data), 2)
  expect_equal(tm_sliced$row_data$id, c(2, 3))
  expect_equal(tm_sliced$matrix[, 1], c(2, 3))
})

test_that("slice works on columns by position", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  col_data <- data.frame(id = 1:3, type = c("x", "y", "z"))
  tm <- tidymatrix(mat, col_data = col_data)

  tm_sliced <- tm %>%
    activate(columns) %>%
    slice(1, 3)

  expect_equal(ncol(tm_sliced$matrix), 2)
  expect_equal(nrow(tm_sliced$col_data), 2)
  expect_equal(tm_sliced$col_data$id, c(1, 3))
  expect_equal(tm_sliced$matrix[1, ], c(1, 9))
})

test_that("slice with range works", {
  mat <- matrix(1:20, nrow = 5, ncol = 4)
  row_data <- data.frame(id = 1:5)
  tm <- tidymatrix(mat, row_data)

  tm_sliced <- tm %>%
    activate(rows) %>%
    slice(2:4)

  expect_equal(nrow(tm_sliced$matrix), 3)
  expect_equal(tm_sliced$row_data$id, c(2, 3, 4))
})

test_that("slice errors when matrix is active", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat)

  expect_error(
    slice(tm, 1),
    "Cannot slice when matrix is active"
  )
})

# Test slice_head ----

test_that("slice_head takes first n rows", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4)
  tm <- tidymatrix(mat, row_data)

  tm_sliced <- tm %>%
    activate(rows) %>%
    slice_head(n = 2)

  expect_equal(nrow(tm_sliced$matrix), 2)
  expect_equal(tm_sliced$row_data$id, c(1, 2))
  expect_equal(tm_sliced$matrix[, 1], c(1, 2))
})

test_that("slice_head takes first n columns", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  col_data <- data.frame(id = 1:3)
  tm <- tidymatrix(mat, col_data = col_data)

  tm_sliced <- tm %>%
    activate(columns) %>%
    slice_head(n = 2)

  expect_equal(ncol(tm_sliced$matrix), 2)
  expect_equal(tm_sliced$col_data$id, c(1, 2))
})

test_that("slice_head with prop works", {
  mat <- matrix(1:20, nrow = 10, ncol = 2)
  row_data <- data.frame(id = 1:10)
  tm <- tidymatrix(mat, row_data)

  tm_sliced <- tm %>%
    activate(rows) %>%
    slice_head(prop = 0.3)

  expect_equal(nrow(tm_sliced$matrix), 3)
  expect_equal(tm_sliced$row_data$id, c(1, 2, 3))
})

# Test slice_tail ----

test_that("slice_tail takes last n rows", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4)
  tm <- tidymatrix(mat, row_data)

  tm_sliced <- tm %>%
    activate(rows) %>%
    slice_tail(n = 2)

  expect_equal(nrow(tm_sliced$matrix), 2)
  expect_equal(tm_sliced$row_data$id, c(3, 4))
  expect_equal(tm_sliced$matrix[, 1], c(3, 4))
})

test_that("slice_tail takes last n columns", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  col_data <- data.frame(id = 1:3)
  tm <- tidymatrix(mat, col_data = col_data)

  tm_sliced <- tm %>%
    activate(columns) %>%
    slice_tail(n = 2)

  expect_equal(ncol(tm_sliced$matrix), 2)
  expect_equal(tm_sliced$col_data$id, c(2, 3))
})

# Test slice_sample ----

test_that("slice_sample returns correct number of rows", {
  set.seed(123)
  mat <- matrix(1:20, nrow = 10, ncol = 2)
  row_data <- data.frame(id = 1:10)
  tm <- tidymatrix(mat, row_data)

  tm_sampled <- tm %>%
    activate(rows) %>%
    slice_sample(n = 5)

  expect_equal(nrow(tm_sampled$matrix), 5)
  expect_equal(nrow(tm_sampled$row_data), 5)
})

test_that("slice_sample with replacement can return more rows", {
  set.seed(123)
  mat <- matrix(1:8, nrow = 4, ncol = 2)
  row_data <- data.frame(id = 1:4)
  tm <- tidymatrix(mat, row_data)

  tm_sampled <- tm %>%
    activate(rows) %>%
    slice_sample(n = 10, replace = TRUE)

  expect_equal(nrow(tm_sampled$matrix), 10)
  expect_equal(nrow(tm_sampled$row_data), 10)
})

test_that("slice_sample with prop works", {
  set.seed(123)
  mat <- matrix(1:20, nrow = 10, ncol = 2)
  row_data <- data.frame(id = 1:10)
  tm <- tidymatrix(mat, row_data)

  tm_sampled <- tm %>%
    activate(rows) %>%
    slice_sample(prop = 0.5)

  expect_equal(nrow(tm_sampled$matrix), 5)
})

# Test rename ----

test_that("rename works on row metadata", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4, old_name = c("A", "B", "C", "D"))
  tm <- tidymatrix(mat, row_data)

  tm_renamed <- tm %>%
    activate(rows) %>%
    rename(new_name = old_name)

  expect_equal(names(tm_renamed$row_data), c("id", "new_name"))
  expect_equal(tm_renamed$row_data$new_name, c("A", "B", "C", "D"))
  expect_false("old_name" %in% names(tm_renamed$row_data))
})

test_that("rename works on column metadata", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  col_data <- data.frame(q_id = 1:3, question = c("Q1", "Q2", "Q3"))
  tm <- tidymatrix(mat, col_data = col_data)

  tm_renamed <- tm %>%
    activate(columns) %>%
    rename(question_id = q_id)

  expect_equal(names(tm_renamed$col_data), c("question_id", "question"))
  expect_equal(tm_renamed$col_data$question_id, c(1, 2, 3))
})

test_that("rename can rename multiple columns", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(old1 = 1:4, old2 = 5:8, old3 = 9:12)
  tm <- tidymatrix(mat, row_data)

  tm_renamed <- tm %>%
    activate(rows) %>%
    rename(new1 = old1, new2 = old2)

  expect_equal(names(tm_renamed$row_data), c("new1", "new2", "old3"))
})

test_that("rename errors when matrix is active", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat)

  expect_error(
    rename(tm, new = old),
    "Cannot rename when matrix is active"
  )
})

# Test pull ----

test_that("pull extracts column from row metadata", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4, value = c(10, 20, 30, 40))
  tm <- tidymatrix(mat, row_data)

  values <- tm %>%
    activate(rows) %>%
    pull(value)

  expect_equal(values, c(10, 20, 30, 40))
  expect_type(values, "double")
})

test_that("pull extracts column from column metadata", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  col_data <- data.frame(id = 1:3, type = c("x", "y", "z"))
  tm <- tidymatrix(mat, col_data = col_data)

  types <- tm %>%
    activate(columns) %>%
    pull(type)

  expect_equal(types, c("x", "y", "z"))
  expect_type(types, "character")
})

test_that("pull with no argument extracts last column", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4, value = c(10, 20, 30, 40))
  tm <- tidymatrix(mat, row_data)

  values <- tm %>%
    activate(rows) %>%
    pull()

  expect_equal(values, c(10, 20, 30, 40))
})

test_that("pull with numeric index works", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(id = 1:4, value = c(10, 20, 30, 40))
  tm <- tidymatrix(mat, row_data)

  ids <- tm %>%
    activate(rows) %>%
    pull(1)

  expect_equal(ids, c(1, 2, 3, 4))
})

test_that("pull errors when matrix is active", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat)

  expect_error(
    pull(tm, 1),
    "Cannot pull when matrix is active"
  )
})

# Test relocate ----

test_that("relocate moves columns to front by default", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(a = 1:4, b = 5:8, c = 9:12)
  tm <- tidymatrix(mat, row_data)

  tm_relocated <- tm %>%
    activate(rows) %>%
    relocate(c)

  expect_equal(names(tm_relocated$row_data), c("c", "a", "b"))
})

test_that("relocate with .before works", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(a = 1:4, b = 5:8, c = 9:12)
  tm <- tidymatrix(mat, row_data)

  tm_relocated <- tm %>%
    activate(rows) %>%
    relocate(c, .before = b)

  expect_equal(names(tm_relocated$row_data), c("a", "c", "b"))
})

test_that("relocate with .after works", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  row_data <- data.frame(a = 1:4, b = 5:8, c = 9:12)
  tm <- tidymatrix(mat, row_data)

  tm_relocated <- tm %>%
    activate(rows) %>%
    relocate(a, .after = c)

  expect_equal(names(tm_relocated$row_data), c("b", "c", "a"))
})

test_that("relocate works on column metadata", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  col_data <- data.frame(id = 1:3, type = c("x", "y", "z"), priority = c(1, 2, 3))
  tm <- tidymatrix(mat, col_data = col_data)

  tm_relocated <- tm %>%
    activate(columns) %>%
    relocate(priority, .before = type)

  expect_equal(names(tm_relocated$col_data), c("id", "priority", "type"))
})

test_that("relocate errors when matrix is active", {
  mat <- matrix(1:12, nrow = 4, ncol = 3)
  tm <- tidymatrix(mat)

  expect_error(
    relocate(tm, 1),
    "Cannot relocate when matrix is active"
  )
})

# Test chaining with new verbs ----

test_that("can chain new verbs with existing ones", {
  mat <- matrix(1:20, nrow = 5, ncol = 4)
  row_data <- data.frame(
    old_id = 1:5,
    group = c("A", "A", "B", "B", "C"),
    score = c(50, 30, 40, 20, 10)
  )
  tm <- tidymatrix(mat, row_data)

  result <- tm %>%
    activate(rows) %>%
    rename(id = old_id) %>%
    filter(group %in% c("A", "B")) %>%
    arrange(score) %>%
    slice_head(n = 3) %>%
    relocate(score, .before = id)

  expect_equal(nrow(result$matrix), 3)
  expect_equal(names(result$row_data), c("score", "id", "group"))
  expect_equal(result$row_data$score, c(20, 30, 40))
})
