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
  
  # non_delimiter_characters
  non_delimiter_characters <- purrr::map_chr(c(letters, LETTERS, 0:9, ".", "'", '"'), ~ .x %>% charToRaw %>% as.character)
  
  
  # the candidates to be delims
  probable_delims <- lines %>%
    purrr::map_df(~count_chars(.x), .id = "line") %>%  # Get char count for each line
    dplyr::filter(!char_raw %in% non_delimiter_characters) %>% # disconsider letters and numbers for delimiter candidates
    dplyr::group_by(char_raw) %>%                    # Get chars with same count
    dplyr::summarise(var = var(count), n = n()) %>%
    dplyr::mutate(char = purrr::map_chr(char_raw, ~ .x %>% as.hexmode %>% as.raw %>% rawToChar)) %>%
    dplyr::arrange(var, n %>% desc, char_raw) 
  
  most_probable_delim <- probable_delims$char[1]
  
  # Message delimiter found
  if(verbose) message(sprintf("Most probable delimiter: '%s'", most_probable_delim))
  
  return(probable_delims)
}

# detect_first_row_with_content

# detect_blank_lines

# guess_locale

# guess_na_string

# guess_quote

# Guess whether file has header
guess_has_header <- function(file, n_max = 10, verbose = FALSE) {
  
  # Read lines safely
  safe_read <- purrr::possibly(readr::read_lines, NULL)
  lines <- safe_read(file, n_max = n_max)
  
  # Get string distances
  w_header <- stringdist::stringdist(lines[1], stringr::str_c(lines[2:length(lines)], collapse = ""))
  wo_header <- stringdist::stringdist(lines[2], stringr::str_c(lines[3:length(lines)], collapse = ""))
  
  # Check whether header exists
  header <- w_header*0.9 > wo_header
    
  # Message header found
  if(header) message("The file probably has a header")
  else message("The file probably doesn't have a header")
  
  return(header)
}

# guess_col_types

# guess_comment

# 

# Detect and return a tabular file configuration
frk_summarise_tabular_file <- function(file, n_max = 10) {
  # guess delim
  guessed_delim = guess_delim(file)$char[1]
  
  # guess encoding
  guessed_encoding = readr::guess_encoding(file)$encoding[1]
  
  # guess has header
  guess_has_header = guess_has_header(file)
  
  return(list(
    file = file,
    guessed_delim = guessed_delim,
    guessed_encoding = guessed_encoding,
    guess_has_col_names = guess_has_col_names
  ))
}

# Detect and return all tabular files configurations from a directory
frk_summarise_tabular_files <- function(path) {
  files <- list.files(path, full.names = TRUE)
  
  summary <- purrr::map_df(files, frk_summarise_tabular_file)
}