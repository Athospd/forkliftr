library(tibble)
library(dplyr)
library(readr)

context("guess_skip")

# Tiny table
tiny <- c(
  "", "", "", "",
  "teste|coluna|hello|world",
  "1|'as,df'|'asdf'|1.2",
  "2|'fd,df'|'0.1'|2.3",
  "3|'asdf'|'234'|4.5",
  "3|'qwe,r'|'234'|4.5"
)
file_tiny <- tempfile()
writeLines(tiny, file_tiny)

# Large table
large <- as_tibble(nasa)
file_large <- tempfile()
write_delim(large, file_large, ";")

test_that("guess_skip can guess from tiny tables", {
  expect_identical(guess_skip(file_tiny), 4)
  expect_message(guess_skip(file_tiny, verbose = TRUE), "probably")
})

test_that("guess_skip can guess from large tables", {
  expect_identical(guess_skip(file_large), 0)
})
