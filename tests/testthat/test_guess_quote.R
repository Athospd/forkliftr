library(tibble)
library(dplyr)
library(readr)

context("guess_quote")

# Tiny tables
tiny <- tibble(
  a = c(1, 2, 3),
  b = c("as,df", "as,df", "gf,ds"))
file_tiny1 <- tempfile()
file_tiny2 <- tempfile()
file_tiny_unquoted1 <- tempfile()
write_csv(tiny, file_tiny1)
write_delim(tiny, file_tiny2, ";")
write.csv(tiny, file_tiny_unquoted1, quote = FALSE)

test_that("guess_quote can guess from tiny tables", {
  expect_identical(guess_quote(file_tiny1), "\"")
  expect_identical(guess_quote(file_tiny2), "")
  expect_message(guess_quote(file_tiny1, verbose = TRUE), "probable")
})

test_that("guess_quote can guess that there is no quote", {
  expect_message(guess_quote(file_tiny_unquoted1, verbose = TRUE), "unquoted")
  expect_identical(guess_quote(file_tiny_unquoted1), "")
})
