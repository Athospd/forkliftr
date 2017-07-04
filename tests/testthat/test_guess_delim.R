library(tibble)
library(dplyr)
library(readr)

context("guess_delim")

# Tiny table
tiny <- tibble(
  a = c('0,0', '0,1'),
  b = c('1,0', '1,1'))
file_tiny <- tempfile()
write_delim(tiny, file_tiny, ";")

# Output table for file_tiny
ans <- tibble(
  char_raw = c("3b", "2c"),
  var = c(0, 0),
  n = c(3L, 2L),
  char = c(";", ","))

# Large tables
large <- as_tibble(nasa)
file_large1 <- tempfile()
file_large2 <- tempfile()
file_large3 <- tempfile()
file_large4 <- tempfile()
write_delim(large, file_large1, ",")
write_delim(large, file_large2, ";")
write_delim(large, file_large3, "|")
write_delim(large, file_large4, "\t")

test_that("guess_delim can guess from tiny tables", {
  expect_identical(guess_delim(file_tiny), ans)
  expect_message(guess_delim(file_tiny, verbose = TRUE), "probable")
})

test_that("guess_delim can guess from large tables", {
  expect_identical(guess_delim(file_large1)$char[1], ",")
  expect_identical(guess_delim(file_large2)$char[1], ";")
  expect_identical(guess_delim(file_large3)$char[1], "|")
  expect_identical(guess_delim(file_large4)$char[1], "\t")
})
