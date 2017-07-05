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