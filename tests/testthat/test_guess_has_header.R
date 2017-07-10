library(tibble)
library(dplyr)
library(readr)

context("guess_has_header")

# Tiny table
tiny <- tibble(
  a = c('abbaab', 'ababaa'),
  b = c('babbbaa', 'abababb'))
file_tiny <- tempfile()
write.table(tiny, file_tiny, sep = ",", col.names = TRUE)

tiny_encoding_iso_8859_1 <- iconv("A,B,C\ná,é,í\nã,ÿ,&", to = "ISO-8859-1")
file_tiny_encoding_iso_8859_1 <- tempfile()
write.table(tiny_encoding_iso_8859_1, file_tiny_encoding_iso_8859_1, sep = ",", col.names = TRUE)

# Large tables
large <- as_tibble(nasa)
file_large1 <- tempfile()
file_large2 <- tempfile()
file_large3 <- tempfile()
file_large4 <- tempfile()
file_large5 <- tempfile()
file_large6 <- tempfile()
file_large7 <- tempfile()
file_large8 <- tempfile()
file_large9 <- tempfile()
file_large10 <- tempfile()
file_large11 <- tempfile()

write.table(large, file_large1, sep = ";", col.names = FALSE)
write.table(large, file_large2, sep = ",", col.names = TRUE)
write.table(large, file_large3, sep = "|", col.names = FALSE)
write.table(large, file_large4, sep = "\t", col.names = TRUE)

write.table(large, file_large5, sep = "\t", col.names = TRUE, fileEncoding = "UTF-8")
write.table(large, file_large6, sep = "\t", col.names = TRUE, fileEncoding = "UCS-2LE")
write.table(large, file_large7, sep = "\t", col.names = TRUE, fileEncoding = "UTF-16LE")
write.table(large, file_large8, sep = "\t", col.names = FALSE, fileEncoding = "ASCII")
write.table(large, file_large9, sep = "\t", col.names = FALSE, fileEncoding = "latin1")

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

test_that("guess_has_header can guess from different encodings", {
  expect_true(guess_has_header(file_tiny_encoding_iso_8859_1))
  expect_true(guess_has_header(file_large5))
  expect_true(guess_has_header(file_large6))
  expect_true(guess_has_header(file_large7))
  expect_false(guess_has_header(file_large8))
  expect_false(guess_has_header(file_large9))
})
