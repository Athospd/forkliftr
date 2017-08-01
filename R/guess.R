#' Guess structural characteristic of a flat file
#' 
#' @description
#' The `guess` family of functions helps you figure out certain
#' structural characteristics of flat files without you having
#' to open the file and examine it manually.
#' 
#' What each specific function does can usually be found out by
#' taking a look at the corresponding [readr::read_delim()] argument or
#' by running `?guess_*` and reading the "Details" section.
#' 
#' @details
#' Here is a list of all currently available `guess` functions and their respective
#' descriptions:
#' - [guess_col_names()]: guesses the column names (if the file has a header)
#' - [guess_col_types()]: guesses the column types
#' - [guess_decimal_mark()]: guesses the decimal mark used in the file
#' - [guess_delim()]: guesses the file's delimiter
#' - [guess_encoding()]: guesses the file's encoding
#' - [guess_grouping_mark()]: guesses the grouping/thousands mark used in the file
#' - [guess_has_header()]: gusses whether the file has column names
#' - [guess_quote()]: gusses the character used to quote strings (if strings are quoted)
#' - [guess_skip()]: guesses how many blank lines are at the beginning of the file
#' 
#' @param file Path to file
#' @param guess_max Maximum number of records to use for guess
#' @param verbose Whether to output guess as message
#' @param encoding Default encoding. This only affects how the file is read. Guessed if not specified.
#' @param skip Number of lines to skip before reading data. Guessed if not specified.
#' @return All `guess` functions return an object that can be used as
#' it's appropriate argument in [readr::read_delim()] or [readr::locale()]
#' 
#' @seealso [readr::read_delim()], [readr::locale()]
#' 
#' @name guess
#' @family guess variants
#' 
#' @examples
#' \dontrun{
#' # Create sample file
#' file <- tempfile()
#' write.table(
#'   dplyr::storms, file, sep = "|", dec = ",",
#'   col.names = TRUE, row.names = FALSE)
#'   
#' # Run some guesses independetly
#' guess_delim(file)
#' guess_decimal_mark(file)
#' guess_col_names(file)
#' }
NULL

#' @rdname guess
#' @export
guess_delim <- function(file, guess_max = 10, verbose = FALSE, encoding = guess_encoding(file, guess_max), skip = guess_skip(file, guess_max)) {
  
  # Read lines safely
  lines <- safe_read(file, 
                     n_max = guess_max, 
                     skip = skip, 
                     locale = locale(encoding = encoding))
  first_line <- lines[1]
  
  # A priori delimiter ranks (to deal with the ties)
  utils::data("a_priori_delimiter_ranks", package = "forkliftr")
  
  # The candidates to be delims
  delims_ordered_by_probability <- lines %>%
    purrr::map_df(~count_chars(.x), .id = "line") %>% # Get char count for each line
    dplyr::left_join(a_priori_delimiter_ranks, by = "char_raw") %>%
    dplyr::mutate(rank = dplyr::if_else(rank %>% is.na, 1, rank)) %>%
    dplyr::filter(rank > 0) %>% # Disconsider letters and numbers as candidates
    dplyr::group_by(rank, char_raw) %>% # Get chars with same count
    dplyr::summarise(var = var(count), n = n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(char = raw_to_char(char_raw),
                  is_present_at_first_line = stringi::stri_detect_fixed(first_line, char)) %>%
    # if it has header, the candidate char must figure at least once at the first line of the file.
    dplyr::arrange(desc(is_present_at_first_line), var, -n, -rank, char_raw) %>%
    dplyr::slice(1:10) %>%
    dplyr::select(-rank)
  
  most_probable_delim <- delims_ordered_by_probability$char[1]
  
  # Message delimiter found
  if(verbose) message(sprintf("Most probable delimiter: '%s'", most_probable_delim))
  
  return(delims_ordered_by_probability)
}

#' @rdname guess
#' @export
guess_encoding <- function(file, guess_max = 10, verbose = FALSE, threshold = 0.2) {
  
  # Guess encoding
  encoding <- readr::guess_encoding(file, n_max = guess_max, threshold = threshold)$encoding[1]
  
  # If readr::guess_encoding has no clue about the encoding, we assume that it is
  if(is.na(encoding)) {
    msg <- sprintf("Unable to guess encoding. NA returned.")
  } else {
    msg <- sprintf("Most probable encoding: %s", encoding)
  }
  
  # Message encoding found
  if(verbose) message(msg)
  
  return(encoding)
}

#' @rdname guess
#' @export
guess_has_header <- function(file, guess_max = 10, verbose = FALSE) {
  
  # Read lines safely
  lines <- safe_read(file, n_max = guess_max, skip = guess_skip(file, guess_max))
  
  lines <- iconv(lines, from = guess_encoding(file, guess_max), to = "UTF-8")
  
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

#' @rdname guess
#' @export
guess_col_types <- function(file, guess_max = 10, verbose = FALSE, delim = guess_delim(file, guess_max), skip = guess_skip(file, guess_max)) {
  
  # Get file column specification
  read_file <- suppressWarnings(suppressMessages(read_with_guess(file, guess_max, delim = delim, skip = skip)))
  col_spec <- attr(read_file, "spec")$cols
  
  # Get colum types
  collectors <- purrr::map_chr(col_spec, ~attr(.x, "class")[1])
  types <- stringr::str_replace(collectors, "collector_", "")
  
  # Message header
  if (verbose) {
    
    # Create output string
    string <- stringr::str_c(types, collapse = ", ")
    string <- stringr::str_trunc(string, 50)
    
    # Print message
    message(sprintf("Column types: %s", string))
  }
  
  return(types)
}

#' @rdname guess
#' @export
guess_col_names <- function(file, guess_max = 10, verbose = FALSE, delim = guess_delim(file, guess_max)$char[1], header = guess_has_header(file, guess_max),  quote = guess_quote(file, guess_max), encoding = guess_encoding(file, guess_max), skip = guess_skip(file, guess_max)) {
  
  # Remove quotes in string
  remove_with_quote <- function(string, quote) {
    if (quote != "") {
      string <- stringr::str_replace_all(string, quote, "")
    }
    return(string)
  }
  
  # Get delim (escaped)
  delim <- delim %>% stringr::str_replace_all("(\\W)", "\\\\\\1")
  quote <- quote %>% stringr::str_replace_all("(\\W)", "\\\\\\1")
  
  # Read lines safely
  if (header) {
    header <- file %>%
      safe_read(n_max = 1, skip = skip, locale = locale(encoding = encoding)) %>%
      remove_with_quote(quote)
    header <- stringr::str_split(header, delim)[[1]]
  } else {
    header <- ""
  }
  
  # Message header
  if (verbose & any(header != "")) {
    
    # Create output string
    string <- stringr::str_c(header, collapse = ", ")
    string <- stringr::str_trunc(string, 50)
    
    # Print message
    message(sprintf("Column names: %s", string))
  } else if (verbose & all(header == "")) {
    
    # Print message
    message("File probably doesn't have column names")
  }
  
  return(header)
}

#' @rdname guess
#' @export
guess_quote <- function(file, guess_max = 10, verbose = FALSE, skip = guess_skip(file, guess_max)) {
  
  # Read lines safely
  lines <- safe_read(file, n_max = guess_max, skip = skip)
  
  # The candidates to be delims
  quotes_ordered_by_probability <- lines %>%
    purrr::map_df(~count_chars(.x), .id = "line") %>%
    dplyr::mutate(even = ifelse(count %% 2 == 0, TRUE, FALSE)) %>%
    dplyr::filter(char_raw %in% c("22", "27") & even) %>%
    dplyr::group_by(char_raw) %>%
    dplyr::summarise(mean = mean(count)) %>%
    dplyr::mutate(char = raw_to_char(char_raw)) %>%
    dplyr::arrange(-mean)
  
  most_probable_quote <- quotes_ordered_by_probability$char[1]
  most_probable_quote <- ifelse(is.na(most_probable_quote), "", most_probable_quote) 
  
  # Message delimiter found
  if(verbose & most_probable_quote != "") {
    message(sprintf("Most probable quote: '%s'", most_probable_quote))
  } else if(verbose & most_probable_quote == "") {
    message("Most probable quote: '' (unquoted)")
  }
  
  return(most_probable_quote)
}

#' @rdname guess
#' @export
guess_skip <- function(file, guess_max = 10, verbose = FALSE) {
  
  # Guess if file has header
  
  # Detect first non-empty line
  lines <- safe_read(file, n_max = guess_max)
  skip = min(which(lines != "")) - 1
  
  # Messsage skip found
  if (verbose) message(sprintf("File contents probably start at row: %s", skip))
  
  return(skip)
}

#' @rdname guess
#' @export
guess_decimal_mark <- function(file, guess_max = 10, verbose = FALSE, delim = guess_delim(file, guess_max)$char[1], quote = guess_quote(file, guess_max), skip = guess_skip(file, guess_max)) {
  
  # Function to filter lines given a quote
  filter_lines <- function(lines, quote) {
    if (quote == "") {
      lines
    } else {
      lines <- lines[!stringr::str_detect(lines, quote)]
    }
    return(lines)
  }
  
  # Read lines safely
  lines <- safe_read(file, n_max = guess_max, skip = skip + 1)
  
  # Get delim and quote (escaped)
  delim <- delim %>% stringr::str_replace_all("(\\W)", "\\\\\\1")
  quote <- quote %>% stringr::str_replace_all("(\\W)", "\\\\\\1")
  
  # Compute stats for each mark
  stats <- lines %>%
    stringr::str_split(delim) %>%
    purrr::flatten_chr() %>%
    filter_lines(quote) %>%
    tibble::tibble(char = .) %>%
    dplyr::mutate(
      comma = stringr::str_count(char, "[0-9],"),
      period = stringr::str_count(char, "[0-9]\\."),
      comma_g1 = comma > 1,
      period_g1 = period > 1
    ) %>%
    dplyr::summarise(
      comma = sum(comma), period = sum(period),
      comma_g1 = any(comma_g1), period_g1 = any(period_g1)
    )

  # Determine mark
  if ((stats$comma > 0 & !stats$comma_g1) | stats$period == 0 | stats$period_g1) {
    decimal_mark <- ","
  } else {
    decimal_mark <- "."
  }
  
  # Message mark found
  if (verbose) message(sprintf("Most probable decimal mark: '%s'", decimal_mark))
  
  return(decimal_mark)
}

#' @rdname guess
#' @export
guess_grouping_mark <- function(file, guess_max = 10, verbose = FALSE) {
  
  # Guess decimal mark
  decimal_mark <- guess_decimal_mark(file, guess_max)
  if (decimal_mark == ".") {
    grouping_mark <- ","
  } else {
    grouping_mark <- "."
  }
  
  # Message mark found
  if (verbose) message(sprintf("Most probable grouping mark: '%s'", grouping_mark))
  
  return(grouping_mark)
}
