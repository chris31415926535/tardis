
#' Text Analysis with Rules and Dictionaries for Inferring Sentiment (TARDIS)
#'
#' This function uses dictionaries (either the included defaults or user-supplied)
#' custom dictionaries) and simple rules to measure the sentiment of supplied text.
#' "Sentiment" means roughly the emotion expressed in the text, where emotions are
#' collapsed into positive (e.g. happy) or negative (e.g. sad, angry).
#'
#' Roughly, each word's sentiment is a property of its dictionary-given sentiment,
#' whether it's written in all-caps or not, and the three preceding words. A preceding
#' negation (e.g. "not") will reverse and reduce the sentiment--turning a positive
#' into a slightly less extreme negative, or vice-versa--and a preceding modifier
#' can either increase/decrease the sentiment (e.g. "very" will increase it,
#' "somewhat" will decrease it).
#'
#' Sentences are scored based on their words and the presence of exclamation or
#' question marks.
#'
#' If a supplied text string has more than one sentence, this function will also
#' return the mean, standard deviation, and range of sentiments expressed in its
#' sentences. The rationale is that it doesn't make sense to apply sentence-level
#' analysis to paragraphs, especially for online communications where people can
#' use quick swings in sentiment to express irony.
#'
#' Input can be supplied in a data.frame or character vector.
#'
#' @param input_text Text to analyze, either a character vector or a data.frame with a column of text.
#' @param text_column If using data.frame input, the name of the column of text to analyze.
#' @param dict_sentiments Optional sentiment dictionary, defaults to internal tardis dictionary.
#'                        A data.frame with two columns: `word` and `value`.
#' @param dict_modifiers Optional modifiers dictionary, or "none" to disable modifiers.
#'                       Defaults to internal tardis dictionary. A data.frame with two columns: `word` and `value`.
#' @param dict_negations Optional negation dictionary, or "none" to disable negations.
#'                       Defaults to internal tardis dictionary. A data.frame with one column: `word`.
#' @param sigmoid_factor Numeric, default 15. Factor for scaling sentence scores to -1/+1
#'                       using a sigmoid function. Set to NA to disable the sigmoid function
#'                       and just return sums of scores, adjusted by any applicable
#'                       negators, modifiers, or punctuation/caps effects.
#' @param negation_factor Numeric, default 0.75. Multiplier for damping effects of
#'                        sentiment-bearing terms after negations. Stacks multiplicatively.
#'                        Should probably be less than 1.
#' @param allcaps_factor Numeric, default 1.25. Multiplier for scaling effects of
#'                       of sentiment-bearing terms in ALL CAPS. Should probably
#'                       be more than 1, to increase effects.
#' @param punctuation_factor Numeric, default 1.15. Multiplier for scaling effects of
#'                           punctuation. A single question mark has no effect, but
#'                           one or more exclamation marks does, and question marks
#'                           have effects in the presence of exclamation marks, up
#'                           to three punctuation marks total.
#' @param use_punctuation Boolean, default TRUE. Should we consider sentence-level punctuation?
#' @param summary_function For multi-sentence texts, how should we summarise sentence
#'                         scores into a text score? Default "mean", also accepts
#'                         "median", "max", "min", and "sum".
#' @param simple_count Boolean, default FALSE. Convenience parameter that overrides many
#'                     other parameters to enable simple counts of dictionary words:
#'                     no modifiers, negations, capitalization, or punctuation
#'                     effects are considered and no sigmoid function is applied.
#' @param verbose For debugging--should it print lots of messages to the console?
#'
#' @return A `tbl_df` with one row for each input text and three new columns:
#'         `sentiment_mean`: the average sentiment for each sentence in each text.
#'         `sentiment_sd`: the standard deviation of sentence sentiments for each text.
#'         `sentiment_range`: the range of sentence sentiments for each text.
#' @export
tardis <- function(
  input_text= c("I am happy.", "I am VERY happy!!", ":)", "Not sad.", "Bad.", "Not bad.", "A happy sentence! And a sad one. In the same text."),
  text_column = NA,
  dict_sentiments = NA,
  dict_modifiers = NA,
  dict_negations = NA,
  sigmoid_factor = 15,
  negation_factor = 0.75,
  allcaps_factor = 1.25,
  punctuation_factor = 1.15,
  use_punctuation = TRUE,
  summary_function = c("mean","median", "max", "min", "sum"),
  simple_count = FALSE,
  verbose = FALSE
) {
  # for dplyr data masking
  sentences <- sentence <- word <- negation1 <- negation2 <- negation3 <- modifier1 <- modifier2 <- modifier3 <- text_id <- sentence_id <- sentiment_word <- punct_exclamation <- punct_question <- sentence_sum <- sentence_punct <- sentence_score <- NULL

  summary_function <- match.arg(summary_function, summary_function)

  # in case it's null by accident, set it to default. this happens sometimes with ... calls, need to debug
  if (is.null(sigmoid_factor)) sigmoid_factor <- 15

  if (simple_count) {
    warning("Parameter simple_count = TRUE overrides most other parameters. Make sure this is intended!")
    dict_modifiers <- "none"
    dict_negations <- "none"
    sigmoid_factor <- NA
    negation_factor <- 1
    allcaps_factor <- 1
    use_punctuation <- FALSE
    summary_function <- "sum"
  }

  # set up summary function
  sum_fun <- switch(summary_function,
         "mean" = mean,
         "median" = stats::median,
         "max" = max,
         "min" = min,
         "sum" = sum)

  # multiplicative scale factors for negations and all caps words
  # allcaps_factor needs 1 subtracted here because of how it's treated later
  allcaps_factor <- allcaps_factor - 1

  ################################.
  # INPUT TEXT SETUP ----

  # VECTOR INPUT: set up
  if (is.vector(input_text)){
    if (verbose) message ("vector input")
    text_column <- "text"
    original_input <- stringr::str_trim(input_text)
    sentences <- dplyr::tibble(sentences = original_input)
    final_output <- sentences
  }

  # DATAFRAME INPUT: set up

  if (is.data.frame(input_text)){
    if (verbose) message ("data frame input")
    original_input <- unlist(input_text[text_column])
    sentences <- dplyr::rename(input_text,
                               sentences = dplyr::all_of(text_column))
    final_output <- input_text
  }


  ########################################.
  # DICTIONARY SETUP ----

  # Sentiments
  if (all(is.na(dict_sentiments)) | all(is.null(dict_sentiments))){
    if (verbose) message ("Using default sentiments dictionary.")
    dict_sentiments <- tardis::dict_tardis_sentiment
  }

  dict_sentiments$token <- stringr::str_squish(dict_sentiments$token)

  # IF MULTI-WORD NGRAMS IN SENTIMENT DICTIONARY
  # if there are any multi-word ngrams (e.g. "supreme court")
  multi_word_indices <- grep(pattern = " ", x = dict_sentiments$token, fixed = TRUE)
  if (length(multi_word_indices) > 0) {
    if (verbose) message ("Found multi-word ngrams in sentiment dictionary.")
    for (i in 1:length(multi_word_indices)) {
      old_word <- dict_sentiments$token[[multi_word_indices[[i]]]]
      new_word <- gsub(x = old_word, pattern = " ", replacement = "x", fixed = TRUE)

      dict_sentiments$token[[multi_word_indices[[i]]]] <- new_word
      sentences$sentences <- gsub(x = sentences$sentences, pattern = old_word, replacement = new_word, fixed = TRUE)

    }

  }

  dict_sentiments_vec <- dict_sentiments$score
  names(dict_sentiments_vec) <- dict_sentiments$token

  # Modifiers

  # if no dictionary supplied by user, use default dictionary
  if (all(is.na(dict_modifiers)) | all(is.null(dict_modifiers))){
    dict_modifiers <- tardis::dict_modifiers
  }

  # if user said "none", we're not using modifiers. otherwise set up vector dictionary
  if (all(dict_modifiers == "none")) {
    if (verbose) message ("Disabling modifiers.")
    use_modifiers <- FALSE
  } else {
    use_modifiers <- TRUE

    # IF MULTI-WORD NGRAMS IN MODIFIERS DICTIONARY
    # if there are any multi-word ngrams (e.g. "gosh darn", "bad ass")
    multi_word_indices_mod <- grep(pattern = " ", x = dict_modifiers$token, fixed = TRUE)
    if (length(multi_word_indices_mod) > 0) {

      if (verbose) message ("Found multi-word ngrams in modifiers dictionary.")

      for (i in 1:length(multi_word_indices_mod)) {
        old_word <- dict_modifiers$token[[multi_word_indices_mod[[i]]]]
        new_word <- gsub(x = old_word, pattern = " ", replacement = "x", fixed = TRUE)

        dict_modifiers$token[[multi_word_indices_mod[[i]]]] <- new_word
        sentences$sentences <- gsub(x = sentences$sentences, pattern = old_word, replacement = new_word, fixed = TRUE)

      }

    }

    dict_modifiers_vec <- dict_modifiers$score
    names(dict_modifiers_vec) <- dict_modifiers$token
  }

  # Negations

  # if no dictionary supplied by user, use default dictionary
  if (all(is.na(dict_negations)) | all(is.null(dict_negations))){
    dict_negations <- tardis::dict_vader_negations
  }

  # if user said "none", we're not using negators. otherwise set up vector dictionary
  if (all(dict_negations == "none")) {
    if (verbose) message ("Disabling negations.")
    use_negations <- FALSE
  } else {
    use_negations <- TRUE

    # IF MULTI-WORD NGRAMS IN NEGATIONS DICTIONARY
    # if there are any multi-word ngrams (e.g. "ain't no")
    multi_word_indices_neg <- grep(pattern = " ", x = dict_negations$token, fixed = TRUE)
    if (length(multi_word_indices_neg) > 0) {

      if (verbose) message ("Found multi-word ngrams in modifiers dictionary.")

      for (i in 1:length(multi_word_indices_neg)) {
        old_word <- dict_negations$token[[multi_word_indices_neg[[i]]]]
        new_word <- gsub(x = old_word, pattern = " ", replacement = "x", fixed = TRUE)

        dict_negations$token[[multi_word_indices_neg[[i]]]] <- new_word
        sentences$sentences <- gsub(x = sentences$sentences, pattern = old_word, replacement = new_word, fixed = TRUE)

      }

    }

    dict_negations_vec <- rep(1, nrow(dict_negations))
    names(dict_negations_vec) <- dict_negations$token
  }

  #################### -
  # SPLIT TEXT INTO SENTENCES ----

  result <- split_text_into_sentences_cpp11(sentences, emoji_regex_internal = emoji_regex_internal, dict_sentiments = dict_sentiments)

  ######################## -
  # SENTENCE PUNCTUATION  ----

  # count instances of exclamation points and double question marks
  # or set them to zero if we're not considering punctuation
  if (use_punctuation){
    result$punct_exclamation <- stringi::stri_count_fixed(result$sentence, pattern = "!")
    result$punct_question <- stringi::stri_count_fixed(result$sentence, pattern = "?")
  } else {
    result$punct_exclamation <- result$punct_question <- 0
  }
  ######################## -
  # SPLIT INTO WORDS ----
  # NB need stri_enc_tooutf8 if using rcpp
  result$word <- stringi::stri_split_regex(str = stringi::stri_trim_both(result$sentence), pattern = "\\s+")

  result <- tidyr::unnest(result, word)

  ######################## -
  # EMOJI SENTIMENTS ----
  # get sentiment now before we lose any emojis
  # doing it twice is inefficient but is i think the only way
  result$sentiment1 <- dict_sentiments_vec[result$word]

  ####################### -
  # STRIP LEADING/TRAILING PUNCTUATION
  # we've already handled emojis
  # gsub is faster than stringii
  result$word <-  gsub(x = result$word, pattern = "^[[:punct:]]*|[[:punct:]]*$", replacement = "")
  #stringi::stri_replace_all(str = result$word, replacement = "", regex = "^[[:punct:]]+|[[:punct:]]$")

  ########################## -
  # WORD CAPITALIZATION ----
  # this is surprisingly slow, toupper()
  # roughly 10x faster if we do it on the original character vectors before
  # breaking it into words
  result <- handle_capitalizations(result, allcaps_factor)

  ###################### -
  # NEGATIONS ----

  # find all negations
  # much faster than the capitalization stuff
  result <- handle_negations(result, dict_negations_vec, negation_factor, use_negations)

  ##########################-
  # MODIFIERS ----
  result <- handle_modifiers(result, dict_modifiers_vec, use_modifiers)


  ########################-
  # WORD-LEVEL SENTIMENTS ----
  # get sentiments for words with punctuation removed

  result$sentiment2 <- dict_sentiments_vec[result$word]

  # get sentiment by using either the with-punctuation or without-punctuation values
  # this should let us capture both emojis and plaintext..
  result$sentiment1[is.na(result$sentiment1)] <- 0
  result$sentiment2[is.na(result$sentiment2)] <- 0

  # original purrr::map was kind of slow
  # rcpp function brings ~ 100ms down to ~ 18
  result$sentiment <- get_nonzero_value_cpp11(result$sentiment1, result$sentiment2)

  # process word-level sentiments
  # here we apply all the vectors we've built so far: applying to the sentiment-bearing
  # words the cumulative effects of any allcaps factor, any negations, and any modifiers

  result$sentiment_word <- result$sentiment * result$negations * result$modifiers * result$allcaps


  ########################-
  # SENTENCE SCORES ----
  # get sentence-level scores
  # This is a bottleneck, but using base R helped

  result_sentences <- handle_sentence_scores(result, sigmoid_factor = sigmoid_factor)


  #################-
  # TEXT SCORES ----
  # we're using the summary function defined up top
  result_text <- result_sentences %>%
    dplyr::group_by(text_id) %>%
    dplyr::summarise(
      score = sum_fun(sentence_score),
      score_sd = stats::sd(sentence_score),
      score_range = max(sentence_score) - min(sentence_score))

  result_text <- dplyr::as_tibble(result_text)

  # add back original text, remove text_id column
  result_text$text_id <- NULL

  dplyr::bind_cols(final_output, result_text)

}



#' Analyze text with more than one dictionary
#'
#' This convenience function takes a text and a set of dictionaries, and calls
#' `tardis::tardis()` once for each dictionary. Other parameters are also passed
#' along to `tardis()`.
#'
#' Dictionaries must be in a single `tbl_df` with at least two columns:
#' `token`, containing the tokens belonging to each dictionary; and `dicionary`,
#'  which contains a unique identifier mapping each token to a dictionary.
#'  Weights, if present, must be in a column named `score`.
#'
#'  Tokens can be mapped to multiple dictionaries, but each row maps one token
#'  to one dictionary.
#'
#' @param input_text A text to be analyzed, either a `tbl_df` or a character vector.
#' @param text_column If `tbl_df` input, a character with the name of the input
#'                    column containing the text to be analyzed.
#' @param dictionaries A single `tbl_df` with columns `dictionary`, `token`, and
#'                     (optionally, for weighted dictionaries) `score`.
#' @param ... Other parameters passed on to `tardis::tardis()`.
#'
#' @return A `tbl_df` with new columns for each dictionary.
#'
#' @examples
#' \dontrun{
#' library(magrittr)
#' # Get NRC emotions dataset from textdata package
#' nrc_emotion <- textdata::lexicon_nrc() %>%
#'   dplyr::rename(token = word, dictionary = sentiment) %>%
#'   dplyr::mutate(score = 1)
#'
#' # set up some input text
#' text <- dplyr::tibble(body = c("I am so angry!", "I am angry.",
#'   "I'm not angry.", "Your mother and I aren't angry, we're just disappointed."))
#'
#' emotions <- tardis_multidict(input_text = text, text_column = "body",
#'   dictionaries = nrc_emotion) %>%
#'   dplyr::select(body, score_anger, score_sadness)
#'
#'  emotions
#' }
#' @export
tardis_multidict <- function(input_text, text_column = NA, dictionaries, ...) {

  # for dplyr data masking
  score <- score_range <- score_sd <- NULL

  # manual import for clean R CMD CHECK
  `:=` <- rlang::`:=`


  ################################.
  # INPUT TEXT SETUP ----

  # VECTOR INPUT: set up
  if (is.vector(input_text)){
    text_column <- "sentences"
    input_text <- dplyr::tibble(sentences = stringr::str_trim(input_text))
  }


  dict_names <- unique(dictionaries$dictionary)

  if (!"score" %in% colnames(dictionaries)) dictionaries$score <- 1

  results <- dplyr::tibble(.rows = nrow(input_text))

  just_text <- input_text[,text_column]

  for (dict_name in dict_names){
    #message(dict_name)
    dictionary <- dplyr::filter(dictionaries, dictionary == dict_name)

    result <- tardis::tardis(input_text = just_text, text_column = text_column, dict_sentiments = dictionary, ... )

    result <- dplyr::rename(result,
                            !!rlang::sym(paste0("score_", dict_name))  := score,
                            !!rlang::sym(paste0("score_", dict_name, "_sd"))    := score_sd,
                            !!rlang::sym(paste0("score_", dict_name, "_range")) := score_range,
    )

    result[,text_column] <- NULL

    results <- dplyr::bind_cols(results, result)
  }

  dplyr::bind_cols(input_text, results)
}
#

# https://nrc.canada.ca/en/research-development/products-services/technical-advisory-services/sentiment-emotion-lexicons
# dictionaries <- readr::read_csv("/mnt/c/Users/chris/Downloads/tardis_dict_test.csv") %>%
#   dplyr::mutate(score = 1, token = ngram) %>%
#   dplyr::rename(dictionary = cluster)

# input_text <- readr::read_csv("/mnt/c/Users/chris/Downloads/tardis_dict_text.csv")

# text_column <- "body"
# nrc_emotion <- textdata::lexicon_nrc() %>%
#   dplyr::rename(token = word, dictionary = sentiment) %>%
#   dplyr::mutate(score = 1)
#
# input_text <- pushshiftR::get_reddit_comments(q = "corgis", size = 500)
#
# text <- input_text %>%
#   select(body)
#
# text <- dplyr::tibble(body = c("I am so angry!", "I am angry.",
# "I'm not angry.", "Your mother and I aren't angry, we're just disappointed.",
# "I can't wait!"))
#
# test <- tardis_multidict(input_text = text, text_column = "body", dictionaries = nrc_emotion, dict_negations = "none") %>%
#   dplyr::select(-tidyselect::contains(c("sd", "range")))
#
# test <- tardis_multidict(input_text = text, text_column = "body", dictionaries = nrc_emotion) %>%
#   dplyr::select(-tidyselect::contains(c("sd", "range")))
#
#
#
# test <- tardis_multidict(input_text, "body", sdf, simple_count = TRUE)
#
#
# testt <- dplyr::select(test, body, tidyselect::contains("score"))
#
#
# z <- function(input_text, ...) {
#   tardis::tardis(input_text, ...)
# }
#
# z(" very happy")
#
# #
# library(magrittr)
# nrc_emotion <- textdata::lexicon_nrc() %>%
# dplyr::rename(token = word, dictionary = sentiment)
#
# # set up some input text
# text <- dplyr::tibble(body = c("I am so angry!", "I am angry.",
# "I'm not angry.", "Your mother and I aren't angry, we're just disappointed."))
#
# emotions <- tardis::tardis_multidict(input_text = text, text_column = "body", dictionaries = nrc_emotion) %>%
# dplyr::select(body, score_anger, score_sadness)
#
# emotions
