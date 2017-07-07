# forkliftr <img src="hex.png" align="right" />

forkliftr: an R Package to stack tabular files

## Overview

forkliftr is an R Package whose main purpose is to extract the structural data
of flat files. It provides you with two families of functions:

- `guess`: these functions look into a flat file and try to guess a specific
structural characteristic (like the file's delimiter, decimal mark, column types,
etc.)

- `frk`: these use `guess` functions in order to either stack many flat files
(`frk_summarise()`) or read a file without knowing nothing about it (`frk_read()`)

## Installation

To install forkliftr simply run the code bellow:

```r
# install.packages("devtools")
devtools::install_github("tidyverse/dplyr")
```

## Usage

Every `guess` function looks basically the same: it's arguments are the path to a
file, the maximum number of records to consider when making the guess, and whether
to print a message with the result. To learn more about this family of functions
simply run `?guess`.

```r
# Create sample file
file <- tempfile()
write.table(
  dplyr::storms, file, sep = "|", dec = ",",
  col.names = TRUE, row.names = FALSE)
   
# Guess file delimiter
guess_delim(file)
#> # A tibble: 5 x 4
#>   char_raw       var     n  char
#>      <chr>     <dbl> <int> <chr>
#> 1       7c 0.0000000    10     |
#> 2       20 0.0000000     9      
#> 3       2d 0.1111111     9     -
#> 4       2c 0.2678571     8     ,
#> 5       5f        NA     1     _

# Guess file's decimal mark
guess_decimal_mark(file)
#> [1] ","

# Guess the names of the columns
guess_col_names(file)
#>  [1] "name"        "year"        "month"       "day"         "hour"       
#>  [6] "lat"         "long"        "status"      "category"    "wind"       
#> [11] "pressure"    "ts_diameter" "hu_diameter"
```

There are two `frk` functions: `frk_summarise()` and `frk_read()`. The first one
returns all available guesses for one or more files, while the second one uses
guesses to read a file without us knowing its exact format. For more information
on those run `?frk_summarise` and `?frk_read`.

```r
# Create sample files
file <- tempfile()
write.table(
  dplyr::storms, file, sep = "|", dec = ",",
  col.names = TRUE, row.names = FALSE)
file2 <- tempfile()
write_csv(dplyr::storms, file2)

# Run all guesses
summ <- frk_summarise(file)
str(summ)
#> List of 1
#>  $ /tmp/RtmpROut6Y/file7f4c612d5234:List of 10
#>   ..$ file         : chr "/tmp/RtmpROut6Y/file7f4c612d5234"
#>   ..$ delim        : chr "|"
#>   ..$ encoding     : chr "ASCII"
#>   ..$ has_header   : logi TRUE
#>   ..$ col_types    : chr [1:13] "character" "integer" "integer" "integer" ...
#>   ..$ col_names    : chr [1:13] "name" "year" "month" "day" ...
#>   ..$ quote        : chr "\""
#>   ..$ skip         : num 0
#>   ..$ decimal_mark : chr ","
#>   ..$ grouping_mark: chr "."

# Run all guesses on multiple files
summ <- frk_summarise(c(file, file2))
str(summ)
#> List of 2
#>  $ /tmp/RtmpROut6Y/file7f4c612d5234:List of 10
#>   ..$ file         : chr "/tmp/RtmpROut6Y/file7f4c612d5234"
#>   ..$ delim        : chr "|"
#>   ..$ encoding     : chr "ASCII"
#>   ..$ has_header   : logi TRUE
#>   ..$ col_types    : chr [1:13] "character" "integer" "integer" "integer" ...
#>   ..$ col_names    : chr [1:13] "name" "year" "month" "day" ...
#>   ..$ quote        : chr "\""
#>   ..$ skip         : num 0
#>   ..$ decimal_mark : chr ","
#>   ..$ grouping_mark: chr "."
#>  $ /tmp/RtmpROut6Y/file7f4c3770ccf4:List of 10
#>   ..$ file         : chr "/tmp/RtmpROut6Y/file7f4c3770ccf4"
#>   ..$ delim        : chr ","
#>   ..$ encoding     : chr "ASCII"
#>   ..$ has_header   : logi TRUE
#>   ..$ col_types    : chr [1:13] "character" "integer" "integer" "integer" ...
#>   ..$ col_names    : chr [1:13] "name" "year" "month" "day" ...
#>   ..$ quote        : chr ""
#>   ..$ skip         : num 0
#>   ..$ decimal_mark : chr "."
#>   ..$ grouping_mark: chr ","

# Read file
tbl <- frk_read(file)
tbl
#> # A tibble: 10,010 x 13
#>     name  year month   day  hour   lat  long              status category
#>    <chr> <int> <int> <int> <int> <dbl> <dbl>               <chr>    <int>
#>  1   Amy  1975     6    27     0  27.5 -79.0 tropical depression       -1
#>  2   Amy  1975     6    27     6  28.5 -79.0 tropical depression       -1
#>  3   Amy  1975     6    27    12  29.5 -79.0 tropical depression       -1
#>  4   Amy  1975     6    27    18  30.5 -79.0 tropical depression       -1
#>  5   Amy  1975     6    28     0  31.5 -78.8 tropical depression       -1
#>  6   Amy  1975     6    28     6  32.4 -78.7 tropical depression       -1
#>  7   Amy  1975     6    28    12  33.3 -78.0 tropical depression       -1
#>  8   Amy  1975     6    28    18  34.0 -77.0 tropical depression       -1
#>  9   Amy  1975     6    29     0  34.4 -75.8      tropical storm        0
#> 10   Amy  1975     6    29     6  34.0 -74.8      tropical storm        0
#> # ... with 10,000 more rows, and 4 more variables: wind <int>,
#> #   pressure <int>, ts_diameter <chr>, hu_diameter <chr>
```
