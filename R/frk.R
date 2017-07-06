# Detect and return a tabular file configuration
frk_summarise_tabular_file <- function(file, guess_max = 10, verbose = FALSE) {
  
  # Guess delim
  guessed_delim = guess_delim(file, guess_max, verbose)$char[1]
  
  # Guess encoding
  guessed_encoding = guess_encoding(file, verbose)$encoding[1]
  
  # Guess has header
  guessed_has_header = guess_has_header(file, guess_max, verbose)
  
  # Guess col types
  guessed_col_types = guess_col_types(file, guess_max, verbose)
  
  # Gues col names
  guessed_col_names = guess_col_names(file, guess_max, verbose)
  
  # Guess quote
  guessed_quote = guess_quote(file, guess_max, verbose)
  
  # Guess lines to skip
  guessed_skip <- guess_skip(file, guess_max, verbose)
  
  # Guess decimal and grouping marks
  guessed_decimal_mark <- guess_decimal_mark(file, guess_max, verbose)
  guessed_grouping_mark <- guess_grouping_mark(file, guess_max, verbose)
  
  return(list(
    file = file,
    guessed_delim = guessed_delim,
    guessed_encoding = guessed_encoding,
    guessed_has_header = guessed_has_header,
    guessed_col_types = guessed_col_types,
    guessed_col_names = guessed_col_names,
    guessed_quote = guessed_quote,
    guessed_skip = guessed_skip,
    guessed_decimal_mark = guessed_decimal_mark,
    guessed_grouping_mark = guessed_grouping_mark
  ))
}

# Detect and return all tabular files configurations from a directory
frk_summarise_tabular_files <- function(path) {
  
  # Get summary for all files
  files <- list.files(path, full.names = TRUE)
  summary <- purrr::map(files, frk_summarise_tabular_file)
  
  # Rename elements of list
  names(summary) <- files
  
  return(summary)
}
