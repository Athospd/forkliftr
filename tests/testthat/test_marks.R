library(tibble)
library(dplyr)
library(readr)

context("guess_decimal_mark/guess_grouping_mark")

# Tiny table
tiny <- c(
  "", "", "", "",
  "teste|coluna|hello|world",
  "1|'as,df'|'asdf'|1,2",
  "2|'fd,df'|'0.1'|2,3",
  "3|'asdf'|'234'|4,5",
  "4|'qwe,r'|'234'|4,5"
)
file_tiny <- tempfile()
writeLines(tiny, file_tiny)

# Large table
large <- as_tibble(nasa)
file_large <- tempfile()
write_delim(large, file_large, ";")

test_that("guess_decimal_mark/guess_grouping_mark can guess from tiny tables", {
  expect_identical(guess_decimal_mark(file_tiny), ",")
  expect_identical(guess_grouping_mark(file_tiny), ".")
  expect_message(guess_decimal_mark(file_tiny, verbose = TRUE), "probable")
  expect_message(guess_grouping_mark(file_tiny, verbose = TRUE), "probable")
})

test_that("guess_decimal_mark/guess_grouping_mark can guess from large tables", {
  expect_identical(guess_decimal_mark(file_large), ".")
  expect_identical(guess_grouping_mark(file_large), ",")
})
