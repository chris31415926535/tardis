# Documentation for data included with tardis package.
# This should all be beefed up.

#' Sentiment dictionary from VADER.
#'
#' A dataset containing the words and sentiments.
#'
#' @format A data frame with 7506 rows and 2 variables:
#' \describe{
#'   \item{word}{word}
#'   \item{sentiment}{sentiment as number}
#'   ...
#' }
#' @source \url{https://cran.r-project.org/web/packages/vader/index.html}
"dict_vader"

#' Modifier dictionary from VADER.
#'
#' @source \url{https://cran.r-project.org/web/packages/vader/index.html}
"dict_vader_modifiers"

#' Negation dictionary from VADER.
#'
#' @source \url{https://cran.r-project.org/web/packages/vader/index.html}
"dict_vader_negations"

#' Sentiment dictionary from Hu and Liu (2004) and package TidyText.
#'
#' @source {Minqing Hu and Bing Liu, “Mining and summarizing customer reviews.”,
#'  Proceedings of the ACM SIGKDD International Conference on Knowledge Discovery
#'  & Data Mining (KDD-2004), Seattle, Washington, USA, Aug 22-25, 2004.}
"dict_liu"

#' Sentiment dictionary for emojis.
#'
#' @references {Kralj Novak P, Smailović J, Sluban B, Mozetič I (2015) Sentiment of
#'  Emojis. PLoS ONE 10(12): e0144296.}
#' @source \url{https://doi.org/10.1371/journal.pone.0144296}
"dict_emoji"

#' Sentiment dictionary for TARDIS package.
#'
#' Combines VADER and emoji dictionaries.
"dict_tardis_sentiment"