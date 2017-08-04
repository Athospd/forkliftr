#' Guess all structural characteristics of one or more flat files
#' 
#' @description
#' Runs all functions from the `guess` family to gather all structural
#' characteristics of given file(s).
#' 
#' @param path a character vector of full path names; the default corresponds to the working directory, getwd(). Tilde expansion (see path.expand) is performed. Missing values will be ignored.
#' @param pattern an optional regular expression. Only file names which match the regular expression will be returned.
#' @param recursive logical. Should the listing recurse into directories?
#' @param guess_max Maximum number of records to use for guess
#' @param verbose Whether to output guess as message
#' @return A tibble whose lines stores the guesses for each file
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
frk_summarise <- function(path, pattern = NULL, recursive = FALSE, guess_max = 10, verbose = FALSE, progress = TRUE) {
  
  # If path is a directory, list all the files.
  files <- list.files(path = path, 
                      pattern = pattern, 
                      all.files = FALSE, 
                      full.names = TRUE,
                      recursive = recursive,
                      ignore.case = FALSE,
                      include.dirs = FALSE,
                      no.. = FALSE)
  
  # If path is a single file then it will return a length zero object.
  if(length(files) == 0) files <- path
  
  
  # Handle verbose
  if (length(files) > 1 & verbose) {
    warning("Only use verbose = TRUE when summarizing one file")
    verbose <- FALSE
  }
  
  message("Summarising files")
  
  # Get summary for all files
  if(progress) {
    pb <- progress::progress_bar$new(total = length(files),
                                     format = "[:bar] :current of :total")
    summary <- purrr::map_df(files, ~{
      pb$tick()
      frk_summarise_(.x, guess_max, verbose)
    })
  } else {
    summary <- purrr::map_df(files, ~frk_summarise_(.x, guess_max, verbose))
  }
  
  return(summary)
}

#' Guess all structural characteristics of a flat file
#' 
#' @param file Path to file
#' @param guess_max Maximum number of records to use for guess
#' @param verbose Whether to output guess as message
#' @return A named list with the guesses for the specified file
frk_summarise_ <- function(file, guess_max = 10, verbose = FALSE) {
  
  # file extension
  file_ext <- base::tolower(tools::file_ext(file))
  
  if(file_ext %in% c("txt", "tsv", "csv", "dat", "")) {
    # Guess encoding
    guessed_encoding <- guess_encoding(file, guess_max, verbose)
    
    # Guess lines to skip
    guessed_skip <- guess_skip(file, guess_max, verbose)
    
    # Guess delim
    guessed_delim <- guess_delim(file, guess_max, verbose, encoding = guessed_encoding, skip = guessed_skip)$char[1]
    
    # Guess has header
    guessed_has_header = guess_has_header(file, guess_max, verbose, skip = guessed_skip, encoding = guessed_encoding)
    
    # Guess quote
    guessed_quote = guess_quote(file, guess_max, verbose, skip = guessed_skip)
    
    # Guess col types
    guessed_col_types = guess_col_types(file, guess_max, verbose, delim = guessed_delim, skip = guessed_skip, encoding = guessed_encoding)
    
    # Gues col names
    guessed_col_names = guess_col_names(file, guess_max, verbose, delim = guessed_delim, header = guessed_has_header, quote = guessed_quote, encoding = guessed_encoding, skip = guessed_skip)
    
    # Guess decimal and grouping marks
    guessed_decimal_mark <- guess_decimal_mark(file, guess_max, verbose, delim = guessed_delim, quote = guessed_quote, skip = guessed_skip)
    guessed_grouping_mark <- guess_grouping_mark(file, guess_max, verbose)
  } else {
    # Guess encoding
    guessed_encoding <- as.character(NA)
    
    # Guess lines to skip
    guessed_skip <- as.numeric(NA)
    
    # Guess delim
    guessed_delim <- as.character(NA)
    
    # Guess has header
    guessed_has_header <- as.logical(NA)
    
    # Guess quote
    guessed_quote <- as.character(NA)
    
    # Guess col types
    guessed_col_types <- as.list(NA)
    
    # Gues col names
    guessed_col_names <- as.list(NA)
    
    # Guess decimal and grouping marks
    guessed_decimal_mark <- as.character(NA)
    guessed_grouping_mark <- as.character(NA)
  }
  
  
  if(verbose) message(sprintf("%s\n", file))
  
  return(list(
    file = file,
    file_ext = file_ext,
    delim = guessed_delim,
    encoding = guessed_encoding,
    has_header = guessed_has_header,
    col_types = list(guessed_col_types),
    col_names = list(guessed_col_names),
    suggested_col_names = list(tide_names(guessed_col_names)),
    quote = guessed_quote,
    skip = guessed_skip,
    decimal_mark = guessed_decimal_mark,
    grouping_mark = guessed_grouping_mark,
    escape_backslash = FALSE,
    escape_double = FALSE,
    na = list(c("", "NA")),
    quoted_na = TRUE,
    comment = "",
    trim_ws = TRUE,
    n_max = Inf
  ))
}


#' Read flat file knowing nothing about them
#' 
#' @description
#' This is a wrapper for [readr::read_delim()] that guesses the main arguments,
#' namely `delim`, `quote`, `col_names`, `encoding`, `decimal_mark`, `grouping_mark`, and `skip`.
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
#' tbl <- frk_read_delim(file)
#' glimpse(tbl)
#' }
#' 
#' @export
frk_read_delim <- function(file,
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


#' Stack a set of files from a instruction
#' 
#' @description
#' This is a wrapper for [readr::read_delim()] that guesses the main arguments,
#' namely `delim`, `quote`, `col_names`, `encoding`, `decimal_mark`, `grouping_mark`, and `skip`.
#' 
#' @param file A tibble returned by \code{frk_summarise()}.
#' 
#' @seealso [frk_summarise()], [frk_read_delim()]
#' 
#' @examples
#' \dontrun{
#' dir <- "~/folder_full_of_flat_files"
#' stacked_files <- dir %>% frk_summarise %>% frk_stack
#' }
#' 
#' @export
frk_stack <- function(instruction) {
  purrr::map(instruction, ~frk_read_delim)
}


