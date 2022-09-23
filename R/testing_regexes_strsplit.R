# library(tidyverse)
# sentences <- sentences_old %>% select(any_of("sentences_orig"))
#
# sentences$text_id <- 1:nrow(sentences)
#
# sentences <- dplyr::mutate(sentences,
#                            sentence = stringi::stri_split_regex(str=sentences_orig, pattern = regex_pattern, simplify = FALSE, omit_empty = TRUE))
#
#
#
# # SENTENCE ORDER DOESN'T MATTER AND WE TREAT EMOJIS AS THEIR OWN SENTENCES!
# # lookaheads are super expensive...
# sentence_sep_regex <- "(?<=(\\.|!|\\?){1,5}\\s)"
# sentence_sep_regex <- "(?<=(\\.|!|\\?){1,5}\\s)"
# sentence_sep_regex <- paste0(emoji_regex, "|\\s")
# sentence_sep_regex <- paste0("\\s")
#
# bench::mark(stringi::stri_extract_all(str = sentences$sentences_orig, regex = emoji_regex))
# bench::mark(stringi::stri_extract_all_regex(str = sentences$sentences_orig, pattern = emoji_regex))
# bench::mark(stringi::stri_split(str = sentences$sentences_orig, regex = emoji_regex))
# bench::mark(stringi::stri_split(str = sentences$sentences_orig, regex = sentence_sep_regex))
#
# emojis <- stringi::stri_extract_all_regex(str = sentences$sentences_orig, pattern = emoji_regex, omit_no_match = TRUE)
#
# sents <- stringi::stri_split_regex(str = sentences$sentences_orig, pattern = emoji_regex) %>%
#   stringi::stri_split_regex(pattern = sentence_sep_regex)
#
# bench::mark(code = {sents <- stringi::stri_split_regex(str = sentences$sentences_orig, pattern = emoji_regex) %>%
#   stringi::stri_split_regex(pattern = sentence_sep_regex)
#
# })
#
# purrr::map(sentences$sentences_orig, stringi::stri_split_regex, pattern = sentence_sep_regex)
#
# emoji_lengths <- purrr::map(emojis, length)
# sent_lengths <- purrr::map(sents, length)
#
# output <-
#
#
# sentences <- tidyr::unnest(sentences, sentence)
