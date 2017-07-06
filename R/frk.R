# Detect and return a tabular file configuration
frk_summarise_ <- function(file, guess_max = 10, verbose = FALSE) {
  
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
    delim = guessed_delim,
    encoding = guessed_encoding,
    has_header = guessed_has_header,
    col_types = guessed_col_types,
    col_names = guessed_col_names,
    quote = guessed_quote,
    skip = guessed_skip,
    decimal_mark = guessed_decimal_mark,
    grouping_mark = guessed_grouping_mark
  ))
}

# Detect and return all tabular files configurations from a directory
frk_summarise <- function(files, guess_max = 10, verbose = FALSE) {
  
  # Handle verbose
  if (length(files) > 1 & verbose) {
    warning("Only use verbose = TRUE when summarizing one file")
    verbose <- FALSE
  }
  
  # Get summary for all files
  summary <- purrr::map(files, ~frk_summarise_(.x, guess_max, verbose))
  
  # Rename elements of list
  names(summary) <- files
  
  return(summary)
}
