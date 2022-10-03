#' @useDynLib tardis, .registration = TRUE



# reddit_data <- pushshiftR::get_reddit_comments(q="corgis", size = 500)
# dict_sentiments <- tardis::dict_tardis_sentiment
#
# sentences <- dplyr::tibble(sentences_orig = reddit_data$body)
#
# result1 <- split_text_into_sentences(sentences, emoji_regex_internal, dict_sentiments)
# resultrcpp <- split_text_into_sentences_rcpp(sentences, emoji_regex_internal, dict_sentiments)
# split_text_into_sentences_rcpp(sentences, emoji_regex_internal, dict_sentiments)
# test <- split_text_into_sentences2(sentences, emoji_regex_internal, dict_sentiments)
#
# bench::mark(z <- split_text_into_sentences(sentences, emoji_regex_internal, dict_sentiments))
# bench::mark(zrcpp <- split_text_into_sentences_rcpp(sentences, emoji_regex_internal, dict_sentiments))


handle_sentence_scores <- function(result, with_dplyr = TRUE, with_dt = FALSE, sigmoid_factor = 15) {
  # dplyr data masking
  punct_exclamation <- punct_question <- sentence <- sentence_id <- sentence_punct <- sentence_score <- sentence_sum <- sentences_orig <- sentiment_word <- text_id <- . <- NULL

  #step1 now takes roughly 90% of the time in this function. can it be sped up?
  # data.table was faster but won't work inside of the package with a clean
  # R CMD CHECK no matter what I try
  # https://stackoverflow.com/questions/50768717/failure-using-data-table-inside-package-function
  if (with_dt){
    # step1 <- data.table::as.data.table(result)
    # step1 <- step1[, .(sentence_sum = sum(sentiment_word, na.rm = TRUE),
    #                      sentence_punct = min(punct_exclamation, 4) * 0.292 + min(punct_question *
    #                                                                                 0.18, 0.96)), keyby = .(text_id, sentence_id)]

  } else {

    step1 <- result %>%
      dplyr::group_by(text_id, sentence_id) %>%
      dplyr::summarise(sentence_sum = sum(sentiment_word, na.rm = TRUE),
                       sentence_punct = min(punct_exclamation, 4) * 0.292 + min(punct_question * 0.18, 0.96)
                       #,sentence_swing = max(sentiment_word) - min(sentiment_word)
                       , .groups = "drop_last"
      )
  }

  # vectorized add punctuation in the signed direction, only if not zero. (otherwise subtracted when it shouldn't)
  # much faster to use primitive sign() function than comparing abs() values
  # about twice as fast without dplyr here
  if (with_dplyr) {
    result_sentences <- step1 %>%
      dplyr::mutate(sentence_score = dplyr::if_else(abs(sentence_sum) > 0, sentence_sum + sentence_punct* (sentence_sum/abs(sentence_sum)), sentence_sum)) %>%
      dplyr::mutate(sentence_score = sentence_score / sqrt((sentence_score * sentence_score) + sigmoid_factor)) %>%
      dplyr::select(-sentence_sum, -sentence_punct)
  } else {
    step1$sentence_score <- step1$sentence_sum + sign(step1$sentence_sum) * step1$sentence_punct
    step1$sentence_score <- step1$sentence_score / sqrt((step1$sentence_score^2) + sigmoid_factor)
    step1$sentence_sum <- step1$sentence_punct <- NULL
    result_sentences <- step1
  }

  # result_sentences <- dplyr::as_tibble(result_sentences)

  return(result_sentences)
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

#
# Rcpp::cppFunction('Rcpp::CharacterVector rcpp_foo() {
#   Rcpp::String let1("ف");
#   Rcpp::String let2("خ");
#
#   Rcpp::CharacterVector out;
#   out.push_back(let1);
#   out.push_back(let2);
#
#   return out;
# }')
#
# #
# Rcpp::cppFunction("Rcpp::CharacterVector split_string_after_punctuation(std::string string_to_split) {
#   //Rcpp::String let1('ف');
#   //Rcpp::String let2('خ');
#
#   Rcpp::CharacterVector out;
#
#   int string_chars = string_to_split.length();
#
#   int found_punct = 0;
#   int last_sentence_start = 0;
#
#   for (int i = 0; i < string_chars; i++){
#   //Rcout << i << '\n';
#     if (found_punct == 0){
#
#        if(string_to_split[i] == '!' | string_to_split[i] == '?') {
#        Rcout << 'found punct at ' << i;
#           found_punct = 1;
#        }
#
#     } else if (found_punct == 1){
#        if ((string_to_split[i] != '!' & string_to_split[i] != '?') | i == (string_chars-1)) {
#           out.push_back(string_to_split.substr(last_sentence_start, i));
#           found_punct = 0;
#           last_sentence_start = i; // FIXME this will double-count some things
#
#        }
#
#     } // end of found_punct == 1
#
#
#   } // end for loop
#   //out.push_back(let1);
#   //out.push_back(let2);
#
#   return out;
# }")
#
# #
#
# bench::mark(rcpp_foo())
#
# split_string_after_punctuation("asd!! safd !")
#
# bench::mark(stringr::str_extract_all(sentences$sentences_orig, emoji_regex))
# bench::mark(stringr::str_remove_all(sentences$sentences_orig, emoji_regex))
