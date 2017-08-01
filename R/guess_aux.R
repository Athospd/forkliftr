#' Auxiliar function to count characters in a line
#' 
#' @param line A string of characters
count_chars <- function(line) {
  
  # Create table with chars
  t <- tibble::tibble(char_raw = charToRaw(line) %>% as.character)
  
  # Return counts
  t %>% 
    dplyr::count(char_raw) %>% 
    dplyr::rename(count = n)
}

#' Auxiliar function to safely read lines
#' 
#' @param ... Arguments passed on to [readr::read_lines()]
safe_read <- purrr::possibly(readr::read_lines, NULL)

#' Auxiliar function to convert vector of raws to chars
#' 
#' @param raw Vector of raw characters
raw_to_char <- function(raw) {
  purrr::map_chr(raw, ~ .x %>% as.hexmode %>% as.raw %>% rawToChar)
}

#' Auxiliar function to read file while guessing delimiter and skip
#' 
#' @param file Path to file.
#' @param guess_max Maximum number of records to use for guesses.
#' @param delim Single character used to separate fields within a record. Guessed if not specified.
#' @param skip Number of lines to skip before reading data. Guessed if not specified.
read_with_guess <- function(file, guess_max, delim = guess_delim(file, guess_max)$char[1], skip = guess_skip(file, guess_max), encoding = guess_encoding(file, guess_max)) {
  
  # Read file
  readr::read_delim(file, delim, n_max = guess_max, guess_max = guess_max, skip = skip, locale = locale(encoding = encoding))
}