library(tibble)
library(dplyr)
library(readr)

context("guess_decimal_mark/guess_grouping_mark")

# Tiny table1
tiny1 <- c(
  "", "", "", "",
  "teste|coluna|hello|world",
  "1|'as,df'|'asdf'|1,2",
  "2|'fd,df'|'0.1'|2,3",
  "3|'asdf'|'234'|4,5",
  "4|'qwe,r'|'234'|4,5"
)
file_tiny1 <- tempfile()
writeLines(tiny1, file_tiny1)

# Tiny table2 - quoted with ' and has comma as both decimal mark and delim.
tiny2 <- c(
  "", "", "", "",
  "'1,1','2,1','3,1','4,1'",
  "'5,1','6,1','7,1','8,1'"
)
file_tiny2 <- tempfile()
writeLines(tiny2, file_tiny2)

# Large table
large <- as_tibble(nasa)
file_large <- tempfile()
write_delim(large, file_large, ";")

test_that("guess_decimal_mark/guess_grouping_mark can guess from tiny tables", {
  expect_identical(guess_decimal_mark(file_tiny1), ",")
  expect_identical(guess_grouping_mark(file_tiny1), ".")
  expect_message(guess_decimal_mark(file_tiny1, verbose = TRUE), "probable")
  expect_message(guess_grouping_mark(file_tiny1, verbose = TRUE), "probable")
  
  expect_identical(guess_decimal_mark(file_tiny2), ",")
  expect_identical(guess_grouping_mark(file_tiny2), ".")
  expect_message(guess_decimal_mark(file_tiny2, verbose = TRUE), "probable")
  expect_message(guess_grouping_mark(file_tiny2, verbose = TRUE), "probable")
})

test_that("guess_decimal_mark/guess_grouping_mark can guess from large tables", {
  expect_identical(guess_decimal_mark(file_large), ".")
  expect_identical(guess_grouping_mark(file_large), ",")
})
