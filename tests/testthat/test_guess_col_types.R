library(tibble)
library(dplyr)
library(readr)

context("guess_col_types")

# Tiny table
tiny <- tibble(
  a = c('asdf', 'fdsa'),
  b = c(1.1, 0),
  c = c('asdf', 3.0),
  d = c(1, 2))
file_tiny <- tempfile()
write_delim(tiny, file_tiny, ";")

# Large table
large <- storms
file_large <- tempfile()
write_delim(large, file_large, ",")

# Output vectors
ans_tiny <- c("character", "double", "character", "integer")
ans_large <- c(
  "character", "integer", "integer", "integer",
  "integer", "double", "double", "character", "integer",
  "integer", "integer", "character", "character")

test_that("guess_col_types can guess from tiny tables", {
  expect_identical(guess_col_types(file_tiny), ans_tiny)
  expect_message(guess_col_types(file_tiny, verbose = TRUE), "types")
})

test_that("guess_col_types can guess from large tables", {
  expect_identical(guess_col_types(file_large), ans_large)
})
