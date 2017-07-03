# Count char
count_chars <- function(line) {
  
  # Create table with chars
  t <- tibble::tibble(char_raw = charToRaw(line) %>% as.character)
  
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
  
  # A priori delimiter ranks (to deal with the ties)
  data("a_priori_delimiter_ranks", package = "forkliftr")
  
  # The candidates to be delims
  delims_ordered_by_probability <- lines %>%
    purrr::map_df(~count_chars(.x), .id = "line") %>%  # Get char count for each line
    dplyr::left_join(a_priori_delimiter_ranks, by = "char_raw") %>%
    dplyr::mutate(rank = dplyr::if_else(rank %>% is.na, 1, rank)) %>%
    dplyr::filter(rank > 0) %>% # Disconsider letters and numbers for delimiter candidates
    dplyr::group_by(rank, char_raw) %>% # Get chars with same count
    dplyr::summarise(var = var(count), n = n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(char = purrr::map_chr(char_raw, ~ .x %>% as.hexmode %>% as.raw %>% rawToChar)) %>%
    dplyr::arrange(var, n %>% dplyr::desc(), rank %>% dplyr::desc(), char_raw) %>%
    dplyr::slice(1:10) %>%
    dplyr::select(-rank)
  
  most_probable_delim <- delims_ordered_by_probability$char[1]
  
  # Message delimiter found
  if(verbose) message(sprintf("Most probable delimiter: '%s'", most_probable_delim))
  
  return(delims_ordered_by_probability)
}

# Guess encoding of a file
guess_encoding <- function(file, verbose = FALSE) {
  
  # Guess encoding
  encoding <- readr::guess_encoding(file)$encoding[1]
  
  # Message encoding found
  if(verbose) message(sprintf("Most probable encoding: '%s'", encoding))
  
  return(encoding)
}

# Guess whether file has header
guess_has_header <- function(file, n_max = 10, verbose = FALSE) {
  
  # Read lines safely
  safe_read <- purrr::possibly(readr::read_lines, NULL)
  lines <- safe_read(file, n_max = n_max)
  
  # Get string distances
  w_header <- mean(stringdist::stringsim(lines[1], lines[2:length(lines)]))
  wo_header <- mean(stringdist::stringsim(lines[2], lines[3:length(lines)]))
  
  # Check whether header exists
  header <- w_header < wo_header*0.5
    
  # Message header found
  if(verbose & header) message("File probably has a header")
  else if (verbose & !header) message("File probably doesn't have a header")
  
  return(header)
}

# Guess column types
guess_col_types <- function(file, n_max = 10, verbose = FALSE) {
  
  # Get file column specification
  if (verbose) read_file <- readr::read_csv(file, n_max = n_max)
  else read_file <- suppressMessages(readr::read_csv(file, n_max = n_max))
  col_spec <- attr(read_file, "spec")$cols
  
  # Get colum types
  collectors <- purrr::map_chr(col_spec, ~attr(.x, "class")[1])
  types <- stringr::str_replace(collectors, "collector_", "")
  
  return(types)
}

# detect_first_row_with_content

# detect_blank_lines

# guess_locale

# guess_na_string

# guess_quote

# guess_comment

# Detect and return a tabular file configuration
frk_summarise_tabular_file <- function(file, n_max = 10, verbose = FALSE) {
  
  # Guess delim
  guessed_delim = guess_delim(file, n_max, verbose)$char[1]
  
  # Guess encoding
  guessed_encoding = guess_encoding(file, verbose)
  
  # Guess has header
  guessed_has_header = guess_has_header(file, n_max, verbose)
  
  # Guess col types
  guessed_col_types = guess_col_types(file, n_max, verbose)
  
  return(list(
    file = file,
    guessed_delim = guessed_delim,
    guessed_encoding = guessed_encoding,
    guessed_has_header = guessed_has_header,
    guessed_col_types = guessed_col_types
  ))
}

# Detect and return all tabular files configurations from a directory
frk_summarise_tabular_files <- function(path) {
  files <- list.files(path, full.names = TRUE)
  summary <- purrr::map_df(files, frk_summarise_tabular_file)
}
