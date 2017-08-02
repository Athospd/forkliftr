library(tibble)
library(dplyr)
library(readr)
library(stringi)

context("guess_encoding")

# Large table
large <- as_tibble(nasa)
file_large <- tempfile()
write_csv(large, file_large, ",")

frk_summarise(file_large, guess_max = 3, verbose = TRUE)
file = file_large
guess_max = 10
verbose = FALSE
encoding = guess_encoding(file, guess_max)
skip = guess_skip(file, guess_max)

lines <- forkliftr:::safe_read(file, 
                   n_max = guess_max, 
                   skip = skip, 
                   locale = locale(encoding = encoding))
first_line <- lines[1]


# tiny ugly table
tiny_ugly <- "A,B,C\ná,é,í\nã,ÿ,&"
file_tiny_ugly <- tempfile()
write.table(tiny_ugly, file_tiny_ugly, quote = FALSE, sep = ",", col.names = TRUE, fileEncoding = "UTF-8")



test_that("guess_encoding returns just one atomic character", {
  expect_identical(length(guess_encoding(file_large)), 1)
  expect_identical(length(guess_encoding(file_tiny_ugly)), 1)
})

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

