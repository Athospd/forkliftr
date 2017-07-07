#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

# Get rid of spurious NOTEs
globalVariables(c(
  "char_raw", "count", "n", "var",
  "a_priori_delimiter_ranks", "even",
  ".", "char", "comma", "period",
  "comma_g1", "period_g1", "header"))