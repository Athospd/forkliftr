library(tibble)
library(dplyr)
library(readr)
library(stringi)

context("guess_encoding")

# Large table
large <- as_tibble(nasa)
file_large <- tempfile()
write_csv(large, file_large, ",")



tiny_ugly <- "A,B,C\ná,é,í\nã,ÿ,&"
guess_encoding(tiny_ugly, threshold = 0.1)
stri_enc_detect(tiny_ugly)

file_tiny_ugly <- tempfile()
write.table(tiny_ugly, file_tiny_ugly, quote = FALSE, sep = ",", col.names = TRUE, fileEncoding = "UTF-8")

writeLines(stri_replace_all_regex(file_tiny_ugly, "\\\\", "/"), "clipboard")

guess_encoding(file_tiny_ugly, guess_max = 10, threshold = 0)


test_that("guess_encoding wraps correclty", {
  expect_identical(guess_encoding(file_large), "ASCII")
  expect_message(guess_encoding(file_large, verbose = TRUE), "probable")
})

test_that("guess_encoding can guess each of every encoding listed in 'iconvlist()'", {
  encoding_passado <- "UTF-8"
  for(encoding in iconvlist()[80:370]) {
    iconv(file_tiny_ugly, from = encoding_passado, to = encoding)
    encoding_passado <- encoding
    expect_identical(guess_encoding(file_tiny_ugly), "UTF-8")
  }
})

