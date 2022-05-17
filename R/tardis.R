# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


# custom function to lag a vector based on the values in an index vector
# essentially a custom version of dplyr::group() %>% dplyr::lag() but MUCH faster.
# vector_index is a vector of monotonically increasing integers indicating groups
# vec_to_lag is the vector to be lagged
# NOTE this seems 50x faster than the dplyr functions, which were the bottleneck
# this could be easily converted to Rcpp if necessary / worth it
lag1_indexed_vector <- function(vector_index, vec_to_lag) {

  vec_length <- length(vector_index)
  vec_lagged1 <- rep(0, vec_length)
  last_id <- 0

  for (i in 1:vec_length){
    #message(i)
    # new sentence id
    if ((vector_index[[i]] != last_id)  ) {
      last_id <- vector_index[[i]]
      vec_lagged1[[i]] <- 0
    } else {
      vec_lagged1[[i]] <- vec_to_lag[[i-1]]
    }

  }

  return (vec_lagged1)
}



#' Text Analysis with Rules and Dictionaries for Inferring Sentiment (TARDIS)
#'
#' function takes a single text string with several sentences (e.g. a reddit post)
#' runs vaderish sentiment algorithm on each sentence
#' returns mean, sd, and range of sentiments so acquired
#' rationale: doesn't make sense to apply sentence-level analysis to paragraphs.
#' especially for online communications where people can use quick swings in sentiment
#' to express irony.
#'
#' @param input_text Text to analyze, either a character vector or a data.frame with a column of text.
#' @param text_column If using data.frame input, the name of the column of text to analyze.
#' @param dict_sentiments Optional sentiment dictionary. A data.frame with two columns: `word` and `value`
#' @param dict_modifiers Optional modifiers dictionary. A data.frame with two columns: `word` and `value`
#' @param dict_negations Optional negation dictionary. A data.frame with one column: `word`
#'
#' @return A data.frame with one row for each input text and three new columns:
#'         `sentiment_mean`: the average sentiment for each sentence in each text.
#'         `sentiment_sd`: the standard deviation of sentence sentiments for each text.
#'         `sentiment_range`: the range of sentence sentiments for each text.
#' @export
tardis1 <- function(
  input_text= "I am happy. I am VERY happy!! ❤️ Not sad. Bad. Not bad. Not very bad.",
  text_column = NA,
  dict_sentiments = NA,
  dict_modifiers = NA,
  dict_negations = NA


) {

  verbose <- TRUE

  warning("Still need to support ascii emojis like :) and :(")

  # multiplicative scale factors for negations and all caps words
  negation_factor <- 0.75
  allcaps_factor  <- 0.25 # this has 1 added to it before multiplying! anything over 0 represents an increase, anything below 1 represents a decrease


  ################################.
  # INPUT TEXT SETUP




  # VECTOR INPUT: set up
  if (is.vector(input_text)){
    if (verbose) message ("vector input")
    text_column <- "text"
    original_input <- stringr::str_trim(input_text)
    sentences <- dplyr::tibble(sentences_orig = original_input)
  }

  # DATAFRAME INPUT: set up

  if (is.data.frame(input_text)){
    if (verbose) message ("data frame input")
    original_input <- unlist(input_text[text_column])
    sentences <- dplyr::rename(input_text,
                               sentences_orig = dplyr::all_of(text_column))

  }


  ########################################.
  ##### DICTIONARY SETUP

  # Sentiments
  if (all(is.na(dict_sentiments))){
    if (verbose) message ("using default sentiments dictionary")
    dict_sentiments <- tidyvader::get_vader_dictionaries()$dictionary[[1]] %>%
      dplyr::add_row(word = "❤️", sentiment = 2.7) %>%
      dplyr::add_row(word = "🤣", sentiment = 2.7)
  }

  dict_sentiments$word <- stringr::str_squish(dict_sentiments$word)

  # IF MULTI-WORD NGRAMS IN SENTIMENT DICTIONARY
  # if there are any multi-word ngrams (e.g. "supreme court")
  multi_word_indices <- grep(pattern = " ", x = dict_sentiments$word, fixed = TRUE)
  if (length(multi_word_indices) > 0) {
    if (verbose) message ("Found multi-word ngrams in sentiment dictionary.")
    for (i in 1:length(multi_word_indices)) {
      old_word <- dict_sentiments$word[[multi_word_indices[[i]]]]
      new_word <- gsub(x = old_word, pattern = " ", replacement = "X", fixed = TRUE)

      dict_sentiments$word[[multi_word_indices[[i]]]] <- new_word
      sentences$sentences_orig <- gsub(x = sentences$sentences_orig, pattern = old_word, replacement = new_word, fixed = TRUE)

    }

  }

  dict_sentiments_vec <- dict_sentiments$sentiment
  names(dict_sentiments_vec) <- dict_sentiments$word

  # Modifiers
  if (all(is.na(dict_modifiers))){
    dict_modifiers <- tidyvader::get_vader_dictionaries()$dictionary[[4]]
  }
  dict_modifiers_vec <- dict_modifiers$booster_value
  names(dict_modifiers_vec) <- dict_modifiers$word

  # Negations
  if (all(is.na(dict_negations))){
    dict_negations <- dplyr::tibble(word = tidyvader::get_vader_dictionaries()$dictionary[[3]])
  }
  dict_negations_vec <- rep(1, nrow(dict_negations))
  names(dict_negations_vec) <- dict_negations$word

  # REGEX TO SPLIT TEXT INTO SENTENCES

  #look behind for punctuation, look ahead for emojis
  # but only look for emojis that are present in the dictionary! huge time saver
  emojis_in_dictionary <- dict_sentiments$word %>% stringr::str_subset(emo::ji_rx)

  emoji_regex <- paste0(emojis_in_dictionary, collapse = "|")
  regex_pattern <- "(?<=(\\.|!|\\?){1,5}\\s)"

  if (length(emojis_in_dictionary) > 0) regex_pattern <- paste0(regex_pattern, "|(?=",emoji_regex,")") # emo::ji_rx



  # need an id for each text, then one for each sentence.
  # tidying it and keeping track...
  sentences$text_id <- 1:nrow(sentences)

  sentences <- dplyr::mutate(sentences,
                             sentence = stringi::stri_split_regex(str=sentences_orig, pattern = regex_pattern, simplify = FALSE, omit_empty = TRUE))

  sentences <- tidyr::unnest(sentences, sentence)

  result <- sentences

  # assign unique sentence ids
  #result <- dplyr::mutate(result, sentence_id = 1:nrow(result))
  result$sentence_id <- 1:nrow(result)

  # count instances of exclamation points and double question marks
  result$punct_exclamation <- stringi::stri_count_fixed(result$sentence, pattern = "!")
  result$punct_question <- stringi::stri_count_fixed(result$sentence, pattern = "?")

  result$word <- stringi::stri_split_regex(str = stringi::stri_trim_both(result$sentence), pattern = "\\s+")

  result <- tidyr::unnest(result, word)

  # FIXME need to handle ascii emojis here like :) before removing leading/trailing punctuation

  result$word <- stringi::stri_replace_all(str = result$word, replacement = "", regex = "^[:punct:]+|[:punct:]$")

  # find any all-caps words
  result$allcaps <- 1 + (allcaps_factor * (result$word == toupper(result$word)))

  # make all words lowercase
  result$word <- tolower(result$word)

  # find all negations
  result$negation <- dict_negations_vec[result$word]
  result$negation <- (dplyr::if_else(is.na(result$negation ), 0, result$negation))

  # get lagged negations
  result$negation1 <- lag1_indexed_vector(vector_index = result$sentence_id, vec_to_lag = result$negation)
  result$negation2 <- lag1_indexed_vector(vector_index = result$sentence_id, vec_to_lag = result$negation1)
  result$negation3 <- lag1_indexed_vector(vector_index = result$sentence_id, vec_to_lag = result$negation2)
  #
  # result <- result %>%
  #   dplyr::group_by(text_id, sentence_id) %>%
  #   dplyr::mutate(negation1  = dplyr::lag(negation, default = 0),
  #                 negation2 = dplyr::lag(negation1, default = 0),
  #                 negation3 = dplyr::lag(negation2, default = 0),
  #   )


  # here we apply the negation factor, doing the scaling and the -1 powers separately so we can have fractional powers
  # if there are ALL CAPS negations. not presently implemented
  # negations
  result$negations <- (negation_factor)^(result$negation1 + result$negation2 + result$negation3)*(-1)^floor(result$negation1 + result$negation2 + result$negation3)

  result$modifier <- dict_modifiers_vec[result$word]
  result$modifier <- (dplyr::if_else(is.na(result$modifier ), 0, result$modifier))

  result$modifier_value <- result$modifier * result$allcaps

  # get laggd modifiers
  result$modifier1 <- lag1_indexed_vector(vector_index = result$sentence_id, vec_to_lag = result$modifier)
  result$modifier2 <- lag1_indexed_vector(vector_index = result$sentence_id, vec_to_lag = result$modifier1)
  result$modifier3 <- lag1_indexed_vector(vector_index = result$sentence_id, vec_to_lag = result$modifier2)

  # result <- result %>%
  #   dplyr::mutate(modifier1 = dplyr::lag(modifier, default = 0),
  #                 modifier2 = dplyr::lag(modifier1, default = 0),
  #                 modifier3 = dplyr::lag(modifier2, default = 0)
  #   )

  result$modifiers <- 1 + (result$modifier1 + 0.95 * result$modifier2 + 0.9 * result$modifier3)


  # cleanup for testing
  result <- dplyr::select(result, -negation1, -negation2, -negation3, -modifier1, -modifier2, -modifier3)

  # get sentiments

  result$sentiment <- dict_sentiments_vec[result$word]

  # process word-level sentiments
  # here we apply all the vectors we've built so far: applying to the sentiment-bearing
  # words the cumulative effects of any allcaps factor, any negations, and any modifiers

  result$sentiment_word <- result$sentiment * result$negations * result$modifiers * result$allcaps


  # get sentence-level scores
  result_sentences <- result %>%
    dplyr::group_by(text_id, sentence_id) %>%
    dplyr::summarise(sentence_sum = sum(sentiment_word, na.rm = TRUE),
                     sentence_punct = min(punct_exclamation, 4) * 0.292 + min(punct_question * 0.18, 0.96)
                     #,sentence_swing = max(sentiment_word) - min(sentiment_word)
                     , .groups = "drop_last"
    ) %>%
    #dplyr::mutate(sentence_score = dplyr::if_else(sentence_sum > 0, sentence_sum + sentence_punct, sentence_sum - sentence_punct)) %>%
    # vectorized add punctuation in the signed direction, only if not zero. (otherwise subtracted when it shouldn't)
    dplyr::mutate(sentence_score = dplyr::if_else(abs(sentence_sum) > 0, sentence_sum + sentence_punct* (sentence_sum/abs(sentence_sum)), sentence_sum)) %>%
    dplyr::mutate(sentence_score = sentence_score / sqrt((sentence_score * sentence_score) + 15)) %>%
    dplyr::select(-sentence_sum, -sentence_punct)

  # result_sentences <- dplyr::tibble(sentence_score = manual_summary_rcpp(sentence_id = result$sentence_id,
  #                                                                        sentiment_word = result$sentiment_word,
  #                                                                        punct_exclamation = result$punct_exclamation,
  #                                                                        punct_question = result$punct_question))


  # result_summary %>%
  #   dplyr::summarise(text = input_text,
  #                    sentiment_mean = mean(sentence_score),
  #                    sentiment_sd = sd(sentence_score),
  #                    sentiment_range = max(sentence_score) - min(sentence_score))

  # text-level scores, combining all sentences
  # result_text <- dplyr::tibble(text = input_text,
  #                              sentiment_mean = mean(result_sentences$sentence_score),
  #                              sentiment_sd = sd(result_sentences$sentence_score),
  #                              sentiment_range = max(result_sentences$sentence_score) - min(result_sentences$sentence_score))
  result_text <- dplyr::summarise(result_sentences,
                                  #text = input_text,
                                  sentiment_mean = mean(sentence_score),
                                  sentiment_sd = stats::sd(sentence_score),
                                  sentiment_range = max(sentence_score) - min(sentence_score))

  # add back original text, remove text_id column
  result_text[text_column] <- original_input
  result_text$text_id <- NULL

  result_text
}

#
# # trying it using vectorized stuff instead of dplyr for speed
# tardis2 <- function(
#   input_text= "happy. very happy. VERY happy! ❤️ ❤️❤️  very not happy. not sad. not very sad. hello there lovely. not happy. not happy! i am happy! i am happy!!! i am not happy. i am not not happy. i am not sad."
# ) {
#
#   # multiplicative scale factors for negations and all caps words
#   negation_factor <- 0.75
#   allcaps_factor  <- 0.25 # this has 1 added to it before multiplying! anything over 0 represents an increase, anything below 1 represents a decrease
#
#   dict_sentiments <- tidyvader::get_vader_dictionaries()$dictionary[[1]] %>%
#     dplyr::add_row(word = "❤️", sentiment = 2.7)
#
#   dict_modifiers <- tidyvader::get_vader_dictionaries()$dictionary[[4]]
#
#   dict_negations <- dplyr::tibble(
#     word = tidyvader::get_vader_dictionaries()$dictionary[[3]],
#     negation = 1
#   )
#   # dict_negations <- dplyr::tribble(
#   #   ~word, ~negation,
#   #   "not", 1,
#   #   "never", 1,
#   #   "don't", 1
#   # )
#
#
#
#   #regex_pattern <- "(?<=[:punct:]\\s)" # emo::ji_rx
#   #regex_pattern <- paste0("(?<=[:punct:]\\s|",emo::ji_rx,")") # emo::ji_rx
#   #regex_pattern <- paste0("(?<=(\\.|!|\\?){1,5}\\s|",emo::ji_rx,")") # emo::ji_rx
#
#   #look behind for punctuation, look ahead for emojis
#   regex_pattern <- paste0("(?<=(\\.|!|\\?){1,5}\\s)|(?=",emo::ji_rx,")") # emo::ji_rx
#
#
#   #input_text <- paste0(input_text, "  ")
#   sentences <- dplyr::tibble(sentence = as.vector(stringi::stri_split_regex(str=input_text, pattern = regex_pattern, simplify = TRUE, omit_empty = TRUE)))
#
#
#
#   result <- sentences %>%
#     # count instances of exclamation points and double question marks
#     dplyr::mutate(punct_exclamation = stringi::stri_count_fixed(sentence, pattern = "!")) %>%
#     dplyr::mutate(punct_question = stringi::stri_count_fixed(sentence, pattern = "?")) %>%
#     tibble::rowid_to_column("sentence_id") %>%
#     dplyr::group_by(sentence_id) %>%
#     dplyr::mutate(word = stringi::stri_split_regex(str = stringi::stri_trim_both(sentence), pattern = "\\s+")) %>%
#     dplyr::select(-sentence) %>%
#     tidyr::unnest(word) %>%
#     # FIXME need to handle ascii emojis here like :) before removing leading/trailing punctuation
#     dplyr::mutate(word = stringi::stri_replace_all(str = word, replacement = "", regex = "^[:punct:]+|[:punct:]$") ) %>%
#
#     #tidytext::unnest_tokens(word, sentence, to_lower = FALSE) %>% stop("unnest emojis")
#
#     # find any all-caps words
#     dplyr::mutate(allcaps = 1 + (allcaps_factor * (word == toupper(word)))) %>%
#
#     # make all words lowercase
#     dplyr::mutate(word = tolower(word)) %>%
#
#     # find all negations
#     dplyr::left_join(dict_negations, by = "word") %>%
#     dplyr::mutate(negation = tidyr::replace_na(negation, 0)) %>%
#     #dplyr::mutate(negation = negation * (allcaps)) %>%
#     dplyr::group_by(sentence_id) %>%
#     dplyr::mutate(negation1  = dplyr::lag(negation),
#                   negation2 = dplyr::lag(negation1),
#                   negation3 = dplyr::lag(negation2),
#     ) %>%
#     dplyr::mutate(dplyr::across(c("negation1", "negation2", "negation3"), tidyr::replace_na, 0)) %>%
#     # here we apply the negation factor, doing the scaling and the -1 powers separately so we can have fractional powers
#     # if there are ALL CAPS negations. not presently implemented
#     # negations
#     dplyr::mutate(negations = (negation_factor)^(negation1 + negation2 + negation3)*(-1)^floor(negation1 + negation2 + negation3)) %>%
#     dplyr::select(-negation, -negation1, -negation2, -negation3) %>%
#
#     # get modifiers
#     dplyr::left_join(dict_modifiers, by = "word") %>%
#     dplyr::mutate(booster_value = booster_value * allcaps) %>%
#     dplyr::mutate(modifier1 = dplyr::lag(booster_value),
#                   modifier2 = dplyr::lag(modifier1),
#                   modifier3 = dplyr::lag(modifier2),
#     ) %>%
#     dplyr::mutate(dplyr::across(c("modifier1", "modifier2", "modifier3"), tidyr::replace_na, 0)) %>%
#     dplyr::mutate(modifiers = 1 + (modifier1 + 0.95 * modifier2 + 0.9 * modifier3)) %>%
#     dplyr::select(-booster_value, -booster_sign, -modifier1, -modifier2, -modifier3)  %>%
#
#
#
#
#
#     # get sentiments
#     dplyr::left_join(dict_sentiments, by = "word") %>%
#
#     # process word-level sentiments
#     # here we apply all the vectors we've built so far: applying to the sentiment-bearing
#     # words the cumulative effects of any allcaps factor, any negations, and any modifiers
#     dplyr::mutate(sentiment_word = sentiment * negations * modifiers * allcaps) %>%
#
#     # get sentence-level scores
#     dplyr::summarise(sentence_sum = sum(sentiment_word, na.rm = TRUE),
#                      sentence_punct = min(punct_exclamation, 4) * 0.292 + min(punct_question * 0.18, 0.96)
#                      #,sentence_swing = max(sentiment_word) - min(sentiment_word)
#     ) %>%
#     dplyr::mutate(sentence_score = dplyr::if_else(sentence_sum > 0, sentence_sum + sentence_punct, sentence_sum - sentence_punct)) %>%
#     dplyr::mutate(sentence_score = sentence_score / sqrt((sentence_score * sentence_score) + 15)) %>%
#     dplyr::select(-sentence_sum, -sentence_punct)
#
#
#   result %>%
#     dplyr::summarise(text = input_text,
#                      sentiment_mean = mean(sentence_score),
#                      sentiment_sd = sd(sentence_score),
#                      sentiment_range = max(sentence_score) - min(sentence_score))
#
# }


