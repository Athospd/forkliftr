# Count char
count_chars <- function(line) {
  
  # Create table with chars
  t <- tibble::tibble(char_raw = charToRaw(line) %>% as.character)
  
  # Return counts
  t %>% 
    dplyr::count(char_raw) %>% 
    dplyr::rename(count = n)
}

# Safely read lines
safe_read <- purrr::possibly(readr::read_lines, NULL)

# Convert vector of raws to chars
raw_to_char <- function(raw) {
  purrr::map_chr(raw, ~ .x %>% as.hexmode %>% as.raw %>% rawToChar)
}

# Read file with guessed delimiter and skip
read_with_guess <- function(file, guess_max) {
  delim <- guess_delim(file, guess_max)$char[1]
  skip <- guess_skip(file, guess_max)
  
  readr::read_delim(file, delim, n_max = guess_max, skip = skip)
}