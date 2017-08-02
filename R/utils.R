#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

# Get rid of spurious NOTEs
globalVariables(c(
  "char_raw", "count", "n", "var",
  "a_priori_delimiter_ranks", "even",
  ".", "char", "comma", "period",
  "comma_g1", "period_g1", "header"))

#' Tide up names to fit as column names of a data.frame
#' 
#' @description
#' Sanitize a character vector to fit as column names.
#' 
#' @param x a character vector.
#' 
#' @examples
#' 
#' tide_names(names(iris))
#' tide_names(c("repeated_name", "Repeated___NAME", "RePeated...nAMe  "))
#' tide_names(c("snakeCase", "snake_case", "snake CasE", "snake Cas E"))
#' 
#' @export
tide_names <- function(x) {
  x %>%
    # remove extra spaces from extremes
    stringr::str_trim(side = "both") %>%
    # all chars to lower case
    base::tolower() %>%
    # remove accents
    rslp:::remove_accents() %>%
    # replace spaces, periods and slashes to _
    stringr::str_replace_all("[ \\./\\\\]", "_") %>%
    # replace spaces or sequence of _'s to a single _
    stringr::str_replace_all(" |_{2,}", "_") %>%
    # remove everything that is no alphanumeric nor _
    stringr::str_replace_all("[^a-z0-9_]|_+$", "") %>%
    # enumerate repeated names if any
    enum_repeated_names()
}

#' Enumarate repeated names
#' 
#' @description
#' Enumarate repeated names and put it as suffix to make every name from a vector unique.
#' 
#' @param str a character vector.
#' 
#' @seealso [tide_names()]
#' 
#' @examples
#' 
#' enum_repeated_names(c("a", "a", "b", "c", "c", "c", "d"))
#' 
#' @export
enum_repeated_names <- function(str) {
  data.frame(name = str) %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(rank = 1:length(name),
                  repeated = sum(rank) > 1,
                  suffix = dplyr::if_else(repeated, as.character(rank), "")) %>%
    tidyr::unite(name_suffix, name, suffix, sep = "") %>%
    dplyr::pull(name_suffix)
}
