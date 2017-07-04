library(tibble)
library(dplyr)
library(readr)

context("guess_delim")

# Tiny table
tiny <- tibble(
  a = c('abbaab', 'ababaa'),
  b = c('babbbaa', 'abababb'))
file_tiny <- tempfile()
write.table(tiny, file_tiny, sep = ",", col.names = TRUE)

# Large tables
large <- as_tibble(nasa)
file_large1 <- tempfile()
file_large2 <- tempfile()
file_large3 <- tempfile()
file_large4 <- tempfile()
write.table(large, file_large1, sep = ";", col.names = FALSE)
write.table(large, file_large2, sep = ",", col.names = TRUE)
write.table(large, file_large3, sep = "|", col.names = FALSE)
write.table(large, file_large4, sep = "\t", col.names = TRUE)

test_that("guess_has_header can guess from tiny tables", {
  expect_true(guess_has_header(file_tiny))
  expect_message(guess_has_header(file_tiny, verbose = TRUE), "probably")
})

test_that("guess_has_header can guess from large tables", {
  expect_false(guess_has_header(file_large1))
  expect_true(guess_has_header(file_large2))
  expect_false(guess_has_header(file_large3))
  expect_true(guess_has_header(file_large4))
})
