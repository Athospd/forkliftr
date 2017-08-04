library(tibble)
library(dplyr)
library(readr)

context("frk_summarise")

# Tables to save
tiny <- tibble(
  column1 = c('asdf', 'fdsa'),
  column2 = c(1.1, 0),
  column3 = c('asdf', 3.0),
  column4 = c(1, 2))
large <- as_tibble(nasa)

# Create directory
dir <- tempdir()
file_tiny <- tempfile(tmpdir = dir)
file_large1 <- tempfile(tmpdir = dir)
file_large2 <- tempfile(tmpdir = dir)
file_large3 <- tempfile(tmpdir = dir)
file_large4 <- tempfile(tmpdir = dir)

# Save files
write.table(tiny, file_tiny, sep = ",", col.names = TRUE, row.names = FALSE)
write.table(large, file_large1, sep = ";", col.names = FALSE, row.names = FALSE)
write.table(large, file_large2, sep = ",", col.names = TRUE, row.names = FALSE)
write.table(large, file_large3, sep = "|", col.names = FALSE, row.names = FALSE)
write.table(large, file_large4, sep = "\t", col.names = TRUE, row.names = FALSE)

# All files
dir <- c(file_tiny, file_large1, file_large2, file_large3, file_large4)

# Output of frk_summarise_tabular_file
ans <- tibble::tibble(
  file = file_tiny,
  delim = ",",
  encoding = "ASCII",
  has_header = TRUE,
  col_types = list(c("character", "double", "character", "integer")),
  col_names = list(c("column1", "column2", "column3", "column4")),
  suggested_col_names = list(c("column1", "column2", "column3", "column4")),
  quote = "\\\"",
  skip = 0,
  decimal_mark = ".",
  grouping_mark = ",",
  escape_backslash = FALSE,
  escape_double = FALSE,
  na = list(c("", "NA")),
  quoted_na = TRUE,
  comment = "",
  trim_ws = TRUE,
  n_max = Inf
)

test_that("frk_summarise works with one file", {
  expect_identical(frk_summarise(file_tiny, progress = FALSE), ans)
  expect_message(frk_summarise(file_tiny, progress = FALSE, verbose = TRUE))
})

test_that("frk_summarise works with more than one file", {
  expect_equal(nrow(frk_summarise(dir, progress = FALSE)), 5)
  expect_equal(ncol(frk_summarise(dir, progress = FALSE)), 18)
})
