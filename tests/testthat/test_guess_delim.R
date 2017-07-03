library(tibble)
library(dplyr)

context("guess_delim")

test_that("guess_delim can guess from tiny tables", {
  expect_identical(guess_delim("'0,0';'0,1'\n'1,0';'1,1'"), tibble(char_raw = c("3b", "2c"),
                                                                   var = c(0, 0),
                                                                   n = c(2L, 2L),
                                                                   char = c(";", ",")))
})
