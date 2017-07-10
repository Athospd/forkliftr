library(tibble)
library(dplyr)
library(readr)

context("guess_encoding")

# Large table
large <- as_tibble(nasa)
file_large <- tempfile()
write_csv(large, file_large, ",")

tiny_encoding_iso_8859_1 <- iconv("A,B,C\ná,é,í\nã,ÿ,&", to = "ISO-8859-1")
file_tiny_encoding_iso_8859_1 <- tempfile()
write.table(tiny_encoding_iso_8859_1, file_tiny_encoding_iso_8859_1, sep = ",", col.names = TRUE)


test_that("guess_encoding wraps correclty", {
  expect_identical(guess_encoding(file_large)$encoding[1], "ASCII")
  expect_message(guess_encoding(file_large, verbose = TRUE), "probable")
  expect_identical(guess_encoding(file_tiny_encoding_iso_8859_1, threshold = 0), "ISO-8859-1")
})
