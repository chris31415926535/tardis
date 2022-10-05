#' @useDynLib tardis, .registration = TRUE

handle_sentence_scores <- function(result, sigmoid_factor = 15) {
  # dplyr data masking
  punct_exclamation <- punct_question <- sentence <- sentence_id <- sentence_punct <- sentence_score <- sentence_sum <- sentences_orig <- sentiment_word <- text_id <- . <- NULL

  #step1 now takes roughly 90% of the time in this function. can it be sped up?
  # data.table was faster but won't work inside of the package with a clean
  # R CMD CHECK no matter what I try
  # https://stackoverflow.com/questions/50768717/failure-using-data-table-inside-package-function

  step1 <- result %>%
    dplyr::group_by(text_id, sentence_id) %>%
    dplyr::summarise(sentence_sum = sum(sentiment_word, na.rm = TRUE),
                     sentence_punct = min(punct_exclamation, 4) * 0.292 + min(punct_question * 0.18, 0.96)
                     #,sentence_swing = max(sentiment_word) - min(sentiment_word)
                     , .groups = "drop_last"
    )


  # vectorized add punctuation in the signed direction, only if not zero. (otherwise subtracted when it shouldn't)
  # much faster to use primitive sign() function than comparing abs() values
  # about twice as fast without dplyr here
    step1$sentence_score <- step1$sentence_sum + sign(step1$sentence_sum) * step1$sentence_punct

    if (!is.na(sigmoid_factor)){
      step1$sentence_score <- step1$sentence_score / sqrt((step1$sentence_score^2) + sigmoid_factor)
    }

    step1$sentence_sum <- step1$sentence_punct <- NULL

  return(step1)
}

handle_capitalizations <- function(result, allcaps_factor){
  result$allcaps <- 1 + (allcaps_factor * (result$word == toupper(result$word)))

  # make all words lowercase
  # again this is surprisingly slow
  result$word <- tolower(result$word)

  return(result)
}

handle_negations <- function(result, dict_negations_vec, negation_factor){
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

  result$negation <- result$negation1 <- result$negation2 <- result$negation3 <- NULL

  return(result)
}

handle_modifiers <- function(result, dict_modifiers_vec) {
  result$modifier <- dict_modifiers_vec[result$word]
  result$modifier <- (dplyr::if_else(is.na(result$modifier ), 0, result$modifier))

  result$modifier_value <- result$modifier * result$allcaps

  # get laggd modifiers
  result$modifier1 <- lag1_indexed_vector(vector_index = result$sentence_id, vec_to_lag = result$modifier)
  result$modifier2 <- lag1_indexed_vector(vector_index = result$sentence_id, vec_to_lag = result$modifier1)
  result$modifier3 <- lag1_indexed_vector(vector_index = result$sentence_id, vec_to_lag = result$modifier2)

  result$modifiers <- 1 + (result$modifier1 + 0.95 * result$modifier2 + 0.9 * result$modifier3)

  result$modifier <- result$modifier1 <- result$modifier2 <- result$modifier3 <- NULL

  return(result)
}

split_text_into_sentences <- function(sentences, emoji_regex_internal, dict_sentiments){
  punct_exclamation <- punct_question <- sentence <- sentence_id <- sentence_punct <- sentence_score <- sentence_sum <- sentences_orig <- sentiment_word <- text_id <- NULL
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
  #bench::mark(unnest = {
  sentences <- tidyr::unnest(sentences, sentence)
  #})
  result <- sentences

  # assign unique sentence ids
  result$sentence_id <- 1:nrow(result)

  return(result)
}


split_text_into_sentences_cpp11 <- function(sentences, emoji_regex_internal, dict_sentiments){
  # dplyr data masking
  sentence <- sentences_orig <- sentences_noemojis <- sentence <- emojis <- NULLsentence <- sentences_orig <- sentences_noemojis <- sentence <- emojis <- NULL
  #look behind for punctuation, look ahead for emojis
  # but only look for emojis that are present in the dictionary! huge time saver
  # 2022-09-25 regex string splitcontinues to be huge bottleneck, even with
  # extracting emojis separately and only splitting on "(?<=(\\.|!|\\?){1,5}\\s)"
  # 2022-09-26 this version does stringsplit using Rcpp to split after string of !,?,.
  #            which is ~25x faster than the regex
  emojis_in_dictionary <- dict_sentiments$word %>% stringr::str_subset(emoji_regex_internal)

  emoji_regex <- paste0(emojis_in_dictionary, collapse = "|")
  regex_pattern <- "(?<=(\\.|!|\\?){1,5}\\s)"

  # need an id for each text, then one for each sentence.
  # unnesting is the big time suck
  sentences$text_id <- 1:nrow(sentences)

  # only extract emojis if there are any in the dictionary. otherwise create a
  # tibble with empty column for emojis
  if (emoji_regex != ""){
    step1 <- sentences %>%
      dplyr::mutate(emojis = stringr::str_extract_all(sentences_orig, emoji_regex))

    step2 <- step1 %>%
      dplyr::mutate(sentences_noemojis = stringr::str_remove_all(sentences_orig, emoji_regex))

  } else {
    step2 <- sentences %>%
      dplyr::mutate(sentences_noemojis = sentences_orig, emojis = list(rep(character(length = 0L), times = nrow(sentences))))
  }

  step3cpp11 <- step2 %>%
    #dplyr::mutate(sentence = stringi::stri_split_regex(str = sentences_noemojis, pattern = regex_pattern, simplify = FALSE, omit_empty = TRUE))
    dplyr::mutate(sentence = purrr::map(sentences_noemojis, split_string_after_punctuation_cpp11)) #split_string_after_punctuation))

  step4 <- step3cpp11 %>%
    dplyr::mutate(sentences = purrr::map2(sentence, emojis, function(x,y) {
      c(x, y)
    }))

  result <- step4 %>%
    dplyr::select(-emojis, -sentences_noemojis, -sentence) %>%
    tidyr::unnest(sentences) %>%
    dplyr::rename(sentence = sentences)

  # assign unique sentence ids
  result$sentence_id <- 1:nrow(result)

  #print(result)
  return(result)
}



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
