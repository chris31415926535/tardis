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
#' @param verbose For debugging--should it print lots of messages to the console?
#'
#' @return A data.frame with one row for each input text and three new columns:
#'         `sentiment_mean`: the average sentiment for each sentence in each text.
#'         `sentiment_sd`: the standard deviation of sentence sentiments for each text.
#'         `sentiment_range`: the range of sentence sentiments for each text.
#' @export

tardis <- function(
  input_text= "I am happy. I am VERY happy!! \u2764 Not sad. Bad. Not bad. Not very bad.",
  text_column = NA,
  dict_sentiments = NA,
  dict_modifiers = NA,
  dict_negations = NA,
  verbose = FALSE
  #  , use_cpp11 = FALSE


) {
  # for dplyr data masking
  sentences_orig <- sentence <- word <- negation1 <- negation2 <- negation3 <- modifier1 <- modifier2 <- modifier3 <- text_id <- sentence_id <- sentiment_word <- punct_exclamation <- punct_question <- sentence_sum <- sentence_punct <- sentence_score <- NULL

  #verbose <- TRUE

  #if (verbose) warning("Still need to support ascii emojis like :) and :(")

  # multiplicative scale factors for negations and all caps words
  negation_factor <- 0.75
  allcaps_factor  <- 0.25 # this has 1 added to it before multiplying! anything over 0 represents an increase, anything below 1 represents a decrease


  ################################.
  # INPUT TEXT SETUP ----

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
  # DICTIONARY SETUP ----

  # Sentiments
  if (all(is.na(dict_sentiments))){
    if (verbose) message ("Using default sentiments dictionary.")
    dict_sentiments <- tardis::dict_tardis_sentiment
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
    dict_modifiers <- tardis::dict_vader_modifiers
  }
  dict_modifiers_vec <- dict_modifiers$booster_value
  names(dict_modifiers_vec) <- dict_modifiers$word

  # Negations
  if (all(is.na(dict_negations))){
    dict_negations <- tardis::dict_vader_negations
  }
  dict_negations_vec <- rep(1, nrow(dict_negations))
  names(dict_negations_vec) <- dict_negations$word

  #################### -
  # SPLIT TEXT INTO SENTENCES ----

  #look behind for punctuation, look ahead for emojis
  # but only look for emojis that are present in the dictionary! huge time saver
  emojis_in_dictionary <- dict_sentiments$word %>% stringr::str_subset(emoji_regex_internal)

  emoji_regex <- paste0(emojis_in_dictionary, collapse = "|")
  regex_pattern <- "(?<=(\\.|!|\\?){1,5}\\s)"

  # FIXME TODO: lookahead and lookbehind emoji regexes are VERY slow, so just lookahead for now
  if (length(emojis_in_dictionary) > 0) regex_pattern <- paste0(regex_pattern, "|(?=",emoji_regex,")") # |(?<=",emoji_regex,")") # emo::ji_rx


  # need an id for each text, then one for each sentence.
  # tidying it and keeping track...
  # FIXME this is a big bottleneck with lots of emojis
  # unnesting is the big time suck
  # right now splits emojis into own sentence with lookahead and lookbehind
  sentences$text_id <- 1:nrow(sentences)

  sentences <- dplyr::mutate(sentences,
                             sentence = stringi::stri_split_regex(str=sentences_orig, pattern = regex_pattern, simplify = FALSE, omit_empty = TRUE))

  sentences <- tidyr::unnest(sentences, sentence)

  result <- sentences

  # assign unique sentence ids
  result$sentence_id <- 1:nrow(result)

  ######################## -
  # PUNCTUATION AND CAPS ----

  # count instances of exclamation points and double question marks
  result$punct_exclamation <- stringi::stri_count_fixed(result$sentence, pattern = "!")
  result$punct_question <- stringi::stri_count_fixed(result$sentence, pattern = "?")

  result$word <- stringi::stri_split_regex(str = stringi::stri_trim_both(result$sentence), pattern = "\\s+")

  result <- tidyr::unnest(result, word)


  # get sentiment now before we lose any emojis
  # doing it twice is inefficient but is i think the only way
  result$sentiment1 <- dict_sentiments_vec[result$word]

  # FIXME need to handle ascii emojis here like :) before removing leading/trailing punctuation
  # gsub is faster than stringii
  result$word <-  gsub(x = result$word, pattern = "^[[:punct:]]*|[[:punct:]]*$", replacement = "")
  #stringi::stri_replace_all(str = result$word, replacement = "", regex = "^[[:punct:]]+|[[:punct:]]$")

  # find any all-caps words
  result$allcaps <- 1 + (allcaps_factor * (result$word == toupper(result$word)))

  # make all words lowercase
  result$word <- tolower(result$word)

  ###################### -
  # NEGATIONS ----

  # find all negations
  result$negation <- dict_negations_vec[result$word]
  result$negation <- (dplyr::if_else(is.na(result$negation ), 0, result$negation))

  # get lagged negations
  result$negation1 <- lag1_indexed_vector(vector_index = result$sentence_id, vec_to_lag = result$negation)
  result$negation2 <- lag1_indexed_vector(vector_index = result$sentence_id, vec_to_lag = result$negation1)
  result$negation3 <- lag1_indexed_vector(vector_index = result$sentence_id, vec_to_lag = result$negation2)

  # here we apply the negation factor, doing the scaling and the -1 powers separately so we can have fractional powers
  # if there are ALL CAPS negations. not presently implemented
  # negations
  result$negations <- (negation_factor)^(result$negation1 + result$negation2 + result$negation3)*(-1)^floor(result$negation1 + result$negation2 + result$negation3)

  ##########################-
  # MODIFIERS ----

  result$modifier <- dict_modifiers_vec[result$word]
  result$modifier <- (dplyr::if_else(is.na(result$modifier ), 0, result$modifier))

  result$modifier_value <- result$modifier * result$allcaps

  # get laggd modifiers
  result$modifier1 <- lag1_indexed_vector(vector_index = result$sentence_id, vec_to_lag = result$modifier)
  result$modifier2 <- lag1_indexed_vector(vector_index = result$sentence_id, vec_to_lag = result$modifier1)
  result$modifier3 <- lag1_indexed_vector(vector_index = result$sentence_id, vec_to_lag = result$modifier2)

  result$modifiers <- 1 + (result$modifier1 + 0.95 * result$modifier2 + 0.9 * result$modifier3)


  # cleanup for testing
  result <- dplyr::select(result, -negation1, -negation2, -negation3, -modifier1, -modifier2, -modifier3)

  ########################-
  # WORD-LEVEL SENTIMENTS ----
  # get sentiments for words with punctuation removed

  result$sentiment2 <- dict_sentiments_vec[result$word]

  # get sentiment by using either the with-punctuation or without-punctuation values
  # this should let us capture both emojis and plaintext..
  result$sentiment1[is.na(result$sentiment1)] <- 0
  result$sentiment2[is.na(result$sentiment2)] <- 0

  result$sentiment <- purrr::map2_dbl(result$sentiment1, result$sentiment2, function(x,y) {
    if (x == y) output <- x
    if (x ==0 & y != 0) output <- y
    if (x !=0 & y == 0) output <- x
    if (x !=0 & y != 0 & x != y) output <- x # should we issue a warning here?
    return(output)
  })

  # process word-level sentiments
  # here we apply all the vectors we've built so far: applying to the sentiment-bearing
  # words the cumulative effects of any allcaps factor, any negations, and any modifiers

  result$sentiment_word <- result$sentiment * result$negations * result$modifiers * result$allcaps


  ########################-
  # SENTENCE SCORES ----

  # get sentence-level scores
  if (TRUE) { #(!use_cpp11){
    result_sentences <- result %>%
      dplyr::group_by(text_id, sentence_id) %>%
      dplyr::summarise(sentence_sum = sum(sentiment_word, na.rm = TRUE),
                       sentence_punct = min(punct_exclamation, 4) * 0.292 + min(punct_question * 0.18, 0.96)
                       #,sentence_swing = max(sentiment_word) - min(sentiment_word)
                       , .groups = "drop_last"
      ) %>%
      # vectorized add punctuation in the signed direction, only if not zero. (otherwise subtracted when it shouldn't)
      dplyr::mutate(sentence_score = dplyr::if_else(abs(sentence_sum) > 0, sentence_sum + sentence_punct* (sentence_sum/abs(sentence_sum)), sentence_sum)) %>%
      dplyr::mutate(sentence_score = sentence_score / sqrt((sentence_score * sentence_score) + 15)) %>%
      dplyr::select(-sentence_sum, -sentence_punct)
  } else {
    # not doing this now, it's not the bottleneck
    # result_sentences <- manual_summary_rcppdf2(result)

  }

  #################-
  # TEXT SCORES ----
  result_text <- result_sentences %>%
    dplyr::group_by(text_id) %>%
    dplyr::summarise(
      sentiment_mean = mean(sentence_score),
      sentiment_sd = stats::sd(sentence_score),
      sentiment_range = max(sentence_score) - min(sentence_score))

  # add back original text, remove text_id column
  result_text[text_column] <- original_input
  result_text$text_id <- NULL

  # reorder to put text first
  result_text <- result_text[,c(4,1,2,3)]

  result_text
}




#' @export
tardis1 <- function(
  input_text= "I am happy. I am VERY happy!! \u2764 Not sad. Bad. Not bad. Not very bad.",
  text_column = NA,
  dict_sentiments = NA,
  dict_modifiers = NA,
  dict_negations = NA,
  verbose = FALSE
  , use_cpp11 = FALSE


) {
  # for dplyr data masking
  sentences_orig <- sentence <- word <- negation1 <- negation2 <- negation3 <- modifier1 <- modifier2 <- modifier3 <- text_id <- sentence_id <- sentiment_word <- punct_exclamation <- punct_question <- sentence_sum <- sentence_punct <- sentence_score <- NULL

  #verbose <- TRUE

  #if (verbose) warning("Still need to support ascii emojis like :) and :(")

  # multiplicative scale factors for negations and all caps words
  negation_factor <- 0.75
  allcaps_factor  <- 0.25 # this has 1 added to it before multiplying! anything over 0 represents an increase, anything below 1 represents a decrease


  ################################.
  # INPUT TEXT SETUP ----

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
  # DICTIONARY SETUP ----

  # Sentiments
  if (all(is.na(dict_sentiments))){
    if (verbose) message ("Using default sentiments dictionary.")
    dict_sentiments <- tardis::dict_tardis_sentiment
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
    dict_modifiers <- tardis::dict_vader_modifiers
  }
  dict_modifiers_vec <- dict_modifiers$booster_value
  names(dict_modifiers_vec) <- dict_modifiers$word

  # Negations
  if (all(is.na(dict_negations))){
    dict_negations <- tardis::dict_vader_negations
  }
  dict_negations_vec <- rep(1, nrow(dict_negations))
  names(dict_negations_vec) <- dict_negations$word

  #################### -
  # SPLIT TEXT INTO SENTENCES ----

  if (!use_cpp11)  result <- split_text_into_sentences(sentences, emoji_regex_internal = emoji_regex_internal, dict_sentiments = dict_sentiments)
  if (use_cpp11)  result <- split_text_into_sentences_rcpp(sentences, emoji_regex_internal = emoji_regex_internal, dict_sentiments = dict_sentiments)

  ######################## -
  # SENTENCE PUNCTUATION  ----

  # count instances of exclamation points and double question marks
  result$punct_exclamation <- stringi::stri_count_fixed(result$sentence, pattern = "!")
  result$punct_question <- stringi::stri_count_fixed(result$sentence, pattern = "?")

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
  result <- handle_negations(result, dict_negations_vec, negation_factor)

  ##########################-
  # MODIFIERS ----
  result <- handle_modifiers(result, dict_modifiers_vec)


  ########################-
  # WORD-LEVEL SENTIMENTS ----
  # get sentiments for words with punctuation removed

  result$sentiment2 <- dict_sentiments_vec[result$word]

  # get sentiment by using either the with-punctuation or without-punctuation values
  # this should let us capture both emojis and plaintext..
  result$sentiment1[is.na(result$sentiment1)] <- 0
  result$sentiment2[is.na(result$sentiment2)] <- 0

  # this purrr::map is kind of slow
  # rcpp function brings ~ 100ms down to ~ 18
  if (use_cpp11) result$sentiment <- get_nonzero_value_cpp11(result$sentiment1, result$sentiment2)
  if (!use_cpp11) {
    result$sentiment <- purrr::map2_dbl(result$sentiment1, result$sentiment2, function(x,y) {
      if (x == y) output <- x
      if (x ==0 & y != 0) output <- y
      if (x !=0 & y == 0) output <- x
      if (x !=0 & y != 0 & x != y) output <- x # should we issue a warning here?
      return(output)
    })
  }

  # process word-level sentiments
  # here we apply all the vectors we've built so far: applying to the sentiment-bearing
  # words the cumulative effects of any allcaps factor, any negations, and any modifiers

  result$sentiment_word <- result$sentiment * result$negations * result$modifiers * result$allcaps


  ########################-
  # SENTENCE SCORES ----
  # get sentence-level scores
  # This is a bottleneck, but using base R and data.table internally helped

  result_sentences <- handle_sentence_scores(result, with_dplyr = FALSE, with_dt = TRUE)


  #################-
  # TEXT SCORES ----
  result_text <- result_sentences %>%
    dplyr::group_by(text_id) %>%
    dplyr::summarise(
      sentiment_mean = mean(sentence_score),
      sentiment_sd = stats::sd(sentence_score),
      sentiment_range = max(sentence_score) - min(sentence_score))

  result_text <- dplyr::as_tibble(result_text)

  # add back original text, remove text_id column
  result_text[text_column] <- original_input
  result_text$text_id <- NULL

  # reorder to put text first
  result_text <- result_text[,c(4,1,2,3)]

  result_text
}
