library(tibble)
library(dplyr)
library(readr)

context("frk_summarise_tabular_file")

# Tables to save
tiny <- tibble(
  a = c('asdf', 'fdsa'),
  b = c(1.1, 0),
  c = c('asdf', 3.0),
  d = c(1, 2))
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

# Output of frk_summarise_tabular_file
ans <- list(
  file = file_tiny,
  guessed_delim = ",",
  guessed_encoding = "ASCII",
  guessed_has_header = FALSE,
  guessed_col_types = c("character", "double", "character", "integer")
)

test_that("frk_summarise_tabular_file outputs correctly", {
  expect_identical(frk_summarise_tabular_file(file_tiny), ans)
  expect_message(frk_summarise_tabular_file(file_tiny, verbose = TRUE))
})

test_that("frk_summarise_tabular_files outputs correctly", {
  expect_equal(length(frk_summarise_tabular_files(dir)), 5)
  expect_equal(length(frk_summarise_tabular_files(dir)[[3]]), 5)
})
