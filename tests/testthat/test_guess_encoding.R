library(tibble)
library(dplyr)
library(readr)

context("guess_encoding")

# Large table
large <- as_tibble(nasa)
file_large <- tempfile()
write_csv(large, file_large, ",")

test_that("guess_encoding wraps correclty", {
  expect_identical(guess_encoding(file_large), "ASCII")
  expect_message(guess_encoding(file_large, verbose = TRUE), "probable")
})
