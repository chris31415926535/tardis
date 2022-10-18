# Documentation for data included with tardis package.
# This should all be beefed up.



#' Modifier dictionary.
#'
#' A `tbl_df` with two columns: `token` and `score`, identifying the tokens
#' that increase or decrease other words' sentiments, and the percentage by
#' which they do so.
#'
#' Derived originally from the VADER dictionary, but modified.
#'
#' @source \url{https://CRAN.R-project.org/package=vader}
"dict_modifiers"

#' Negation dictionary.
#'
#' A `tbl_df` with one column: token.
#'
#' Can include apostrophes or not, but they're removed in processing so there's
#'  no need to include *both* words with and without apostrophes.
#'
#' Derived originally from the VADER dictionary, but modified.
#'
#' @source \url{https://CRAN.R-project.org/package=vader}
"dict_negations"


#' Sentiment dictionary for TARDIS package.
#'
#' Combines VADER and emoji dictionaries.
#'
#' @encoding UTF-8
"dict_tardis_sentiment"
