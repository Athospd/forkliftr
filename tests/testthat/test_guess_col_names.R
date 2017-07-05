library(tibble)
library(dplyr)
library(readr)

context("guess_col_names")

# Tiny table
tiny <- tibble(
  coluna1 = c('asdf', 'fdsa'),
  coluna2 = c(1.1, 0),
  coluna3 = c('asdf', 3.0),
  coluna4 = c(1, 2))
file_tiny <- tempfile()
write_delim(tiny, file_tiny, ";")

# Large table
large <- storms
file_large <- tempfile()
write_delim(large, file_large, ",")

# Output vectors
ans_tiny <- c("coluna1", "coluna2", "coluna3", "coluna4")
ans_large <- c(
  "name", "year", "month", "day", "hour",
  "lat", "long", "status", "category", "wind",
  "pressure", "ts_diameter", "hu_diameter")

test_that("guess_col_names can guess from tiny tables", {
  expect_identical(guess_col_names(file_tiny), ans_tiny)
  expect_message(guess_col_names(file_tiny, verbose = TRUE), "names")
})

test_that("guess_col_names can guess from large tables", {
  expect_identical(guess_col_names(file_large), ans_large)
})
