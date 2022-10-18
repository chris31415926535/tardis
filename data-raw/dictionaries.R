## Set up dictionaries
library(magrittr)

# VADER dict
dict_vader <- tidyvader::get_vader_dictionaries()$dictionary[[1]] %>%
  dplyr::rename(token = word, score = sentiment) %>%
  readr::write_csv("data-raw/dict_vader.csv")

dict_vader <- readr::read_csv("data-raw/dict_vader_mod.csv")

usethis::use_data(dict_vader, overwrite = TRUE)

# Tidytext dictionary
# +1 for positive, -1 for negative
#Minqing Hu and Bing Liu, “Mining and summarizing customer reviews.”, Proceedings of the ACM SIGKDD International Conference on Knowledge Discovery & Data Mining (KDD-2004), Seattle, Washington, USA, Aug 22-25, 2004.
dict_liu <- tidytext::sentiments %>%
  dplyr::mutate(score = dplyr::if_else(sentiment == "positive", 1, -1)) %>%
  dplyr::rename(token = word) %>%
  readr::write_csv("data-raw/dict_liu.csv")

usethis::use_data(dict_liu, overwrite = TRUE)


## UTF8 EMOJI DICT
# from a published paper that looked at emoji usage and sentiment
# NB they recorded the sentiment of the tweets the emojis occurred in, then used
# the distribution of tweet sentiments to create an emoji sentiment:
#    >> An emoji is assigned a sentiment from all the tweets in which it occurs.
#    >> First, for each emoji, we form a discrete probability distribution
#    >> (p−, p0, p+). The sentiment score  of the emoji is then computed as the
#    >> mean of the distribution. (Novak et al. 2015)
# We follow this methodology here.
# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0144296
# Kralj Novak P, Smailović J, Sluban B, Mozetič I (2015) Sentiment of Emojis. PLoS ONE 10(12): e0144296. https://doi.org/10.1371/journal.pone.0144296
# https://www.clarin.si/repository/xmlui/bitstream/handle/11356/1048/Emoji_Sentiment_Data_v1.0.csv?sequence=8&isAllowed=y
dict_emoji_raw <- readr::read_csv("https://www.clarin.si/repository/xmlui/bitstream/handle/11356/1048/Emoji_Sentiment_Data_v1.0.csv?sequence=8&isAllowed=y") %>%
  readr::write_csv("data-raw/dict_emoji_raw.csv")

dict_emoji <- dict_emoji_raw %>%
  dplyr::mutate(total = Positive + Negative + Neutral,
                score = (Positive - Negative) / total) %>%
  dplyr::select(token = Emoji, score) %>%
  readr::write_csv("data-raw/dict_emoji.csv")

usethis::use_data(dict_emoji, overwrite = TRUE)

# and an emoji regex from the package emo
emoji_regex_internal <- emo::ji_rx

usethis::use_data(emoji_regex_internal, internal = TRUE, overwrite = TRUE)

# create TARDIS dictionary combining Vader and emoji dicts
# we comptute emoji sentiment sa per Novak et al 2015, then we
# "normalize" to vader's scale of -3.9 to +3.4 (weirdly)
# we also add a row with a red heart with the same score as the other two
# hearts in the dictionary
dict_emoji_temp <- dict_emoji_raw %>%
  dplyr::filter(Occurrences > 50) %>%
  dplyr::mutate(total = Positive + Negative + Neutral,
                score = (Positive - Negative) / total) %>%
  dplyr::mutate(score = dplyr::if_else(score > 0, score * 3.4, score * 3.9)) %>%
  dplyr::select(token = Emoji, score) %>%
  dplyr::add_row(token = utf8::as_utf8("❤️"), score = 2.54, .before = 2)



dict_tardis_sentiment <- dplyr::bind_rows(dict_vader, dict_emoji_temp) %>%
  dplyr::filter(!stringr::str_detect(token, "ggas"))

readr::write_csv(dict_tardis_sentiment, "data-raw/dict_tardis.csv")

usethis::use_data(dict_tardis_sentiment, overwrite = TRUE)


###### MODIFIERS

dict_vader_modifiers <- tidyvader::get_vader_dictionaries()$dictionary[[4]] %>%
  dplyr::rename(token = word) %>%
  readr::write_csv("data-raw/dict_vader_modifiers.csv")

dict_modifiers <- readr::read_csv("data-raw/dict_modifiers.csv")

usethis::use_data(dict_modifiers, overwrite = TRUE)


### NEGATIONS

dict_vader_negations <- dplyr::tibble(token = tidyvader::get_vader_dictionaries()$dictionary[[3]])

usethis::use_data(dict_vader_negations, overwrite = TRUE)
