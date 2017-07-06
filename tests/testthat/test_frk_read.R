library(tibble)
library(dplyr)
library(readr)

context("frk_read")

# Tiny file
tiny <- tibble(
  column1 = c('as|df', 'fd|sa'),
  column2 = c(1.1, 0),
  column3 = c('asdf', 3.0),
  column4 = c(1, 2))
file_tiny <- tempfile()
write.table(
  tiny, file_tiny, sep = "|", dec = ",",
  col.names = TRUE, row.names = FALSE)

# Large file
large <- as_tibble(nasa)
file_large <- tempfile()
write.table(
  large, file_large, sep = ",",
  col.names = TRUE, row.names = FALSE)

# Columns types
types_tiny <- c("character", "numeric", "character", "integer")
types_large <- c(
  "numeric", "numeric", "integer", "integer",
  "numeric", "numeric", "numeric", "integer",
  "integer", "numeric", "numeric"
)

test_that("frk_read can read files", {
  expect_true(all(purrr::map_chr(frk_read(file_tiny), class) == types_tiny))
  expect_true(all(purrr::map_chr(frk_read(file_large), class) == types_large))
})
