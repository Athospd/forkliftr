#' Guess all structural characteristics of one or more flat files
#' 
#' @description
#' Runs all functions from the `guess` family to gather all structural
#' characteristics of given file(s).
#' 
#' @param files Path to file or character vector of paths to files
#' @param guess_max Maximum number of records to use for guess
#' @param verbose Whether to output guess as message
#' @return A list of lists with the guesses for each file
#' 
#' @seealso [guess_delim()], [guess_col_names()], and the whole `guess` family
#' 
#' @examples
#' \dontrun{
#' # Create sample files
#' file <- tempfile()
#' write.table(
#'   dplyr::storms, file, sep = "|", dec = ",",
#'   col.names = TRUE, row.names = FALSE)
#' file2 <- tempfile()
#' write_csv(dplyr::storms, file2)
#' 
#' # Run all guesses
#' summ <- frk_summarise(file)
#' str(summ)
#' 
#' # Run all guesses on multiple files
#' summ <- frk_summarise(c(file, file2))
#' str(summ)
#' }
#' 
#' @export
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

#' Guess all structural characteristics of a flat file
#' 
#' @param file Path to file
#' @param guess_max Maximum number of records to use for guess
#' @param verbose Whether to output guess as message
#' @return A list with the guesses for each file
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


#' Read flat files knowing nothing about them
#' 
#' @description
#' This is a wrapper for [readr::read_delim()] that guesses the main arguments,
#' namely `delim`, `quote`, `col_names`, `locale`, and `skip`.
#' 
#' @param file Path to file
#' @param delim (Guessed) Delimiter of file
#' @param quote (Guessed) Character used to quote strings
#' @param escape_backslash Whether backslashed are used to escape special characters
#' @param escape_double Whether quotes are escaped by doubling them
#' @param col_names (Guessed) Whether file has column names or vector with column names
#' @param col_types Vector with types to be forced on columns or NULL for guess
#' @param locale (Guessed) Some region-dependent information
#' @param na String or character vector to use for missing values
#' @param quoted_na Whether missing values can be quoted
#' @param comment String that indicates comments
#' @param trim_ws Whether to trim whitespace from each field
#' @param skip (Guessed) Number of lines to skip before reading data
#' @param n_max Maximum number of records to read
#' @param guess_max Maximum number of records to use for guess
#' 
#' @seealso [readr::read_delim()], [readr::locale()]
#' 
#' @examples
#' \dontrun{
#' # Create sample file
#' file <- tempfile()
#' write.table(
#'   dplyr::storms, file, sep = "|", dec = ",",
#'   col.names = TRUE, row.names = FALSE)
#' 
#' # Read file
#' tbl <- frk_read(file)
#' glimpse(tbl)
#' }
#' 
#' @export
frk_read <- function(file,
                     delim = guess_delim(file, guess_max)$char[1],
                     quote = guess_quote(file, guess_max),
                     escape_backslash = FALSE,
                     escape_double = TRUE,
                     col_names = guess_has_header(file, guess_max),
                     col_types = NULL,
                     locale = readr::locale(
                       encoding = guess_encoding(file, guess_max)$encoding[1],
                       decimal_mark = guess_decimal_mark(file, guess_max),
                       grouping_mark = guess_grouping_mark(file, guess_max)),
                     na = c("", "NA"),
                     quoted_na = TRUE,
                     comment = "",
                     trim_ws = TRUE,
                     skip = guess_skip(file, guess_max),
                     n_max = Inf,
                     guess_max = min(10, n_max)) {
  
  # Use read delim with new arguments
  out <- readr::read_delim(
    file, delim, quote, escape_backslash, escape_double,
    col_names, col_types, locale, na, quoted_na, comment,
    trim_ws, skip, n_max, guess_max)
  
  return(out)
}
