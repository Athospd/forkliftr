# Count char
count_chars <- function(line) {
  
  # Create table with chars
  t <- tibble(char_raw = charToRaw(line) %>% as.character)
  
  # Return counts
  t %>% 
    dplyr::count(char_raw) %>% 
    dplyr::rename(count = n)
}

# Guess delimiter of a file
guess_delim <- function(file, n_max = 10, verbose = FALSE) {
  
  # Read lines safely
  safe_read <- purrr::possibly(readr::read_lines, NULL)
  lines <- safe_read(file, n_max = n_max)
  
  # a priori delimiter ranks (to deal with the ties)
  data("a_priori_delimiter_ranks", package = "forkliftr")
  
  # the candidates to be delims
  delims_ordered_by_probability <- lines %>%
    purrr::map_df(~count_chars(.x), .id = "line") %>%  # Get char count for each line
    dplyr::left_join(a_priori_delimiter_ranks, by = "char_raw") %>%
    dplyr::mutate(rank = if_else(rank %>% is.na, 1, rank)) %>%
    dplyr::filter(rank > 0) %>% # disconsider letters and numbers for delimiter candidates
    dplyr::group_by(rank, char_raw) %>%                    # Get chars with same count
    dplyr::summarise(var = var(count), n = n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(char = purrr::map_chr(char_raw, ~ .x %>% as.hexmode %>% as.raw %>% rawToChar)) %>%
    dplyr::arrange(var, n %>% desc, rank %>% desc, char_raw) %>%
    dplyr::slice(1:10) %>%
    dplyr::select(-rank)
  
  most_probable_delim <- delims_ordered_by_probability$char[1]
  
  # Message delimiter found
  if(verbose) message(sprintf("Most probable delimiter: '%s'", most_probable_delim))
  
  return(delims_ordered_by_probability)
}

# detect_first_row_with_content

# detect_blank_lines

# guess_locale

# guess_na_string

# guess_quote

# guess_has_col_names

# guess_col_types

# guess_comment

# 

# Detect and return a tabular file configuration
frk_summarise_tabular_file <- function(file) {
  # guess delim
  guessed_delim = guess_delim(file)$char[1]
  
  # guess encoding
  guessed_encoding = readr::guess_encoding(file)$encoding[1]
  
  return(list(
    file = file,
    guessed_delim = guessed_delim,
    guessed_encoding = guessed_encoding
  ))
}

# Detect and return all tabular files configurations from a directory
frk_summarise_tabular_files <- function(path) {
  files <- list.files(path, full.names = TRUE)
  
  summary <- purrr::map_df(files, frk_summarise_tabular_file)
}