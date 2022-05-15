# # testing data.table
#
# library(magrittr)
# dict_sentiments <- tidyvader::get_vader_dictionaries()$dictionary[[1]] %>%
#   dplyr::add_row(word = "❤️", sentiment = 2.7)
#
#
# df_dplyr <- dplyr::tibble(word = c("happy", "the", "cow", "sad", "❤️"))
#
#
# bench::mark(dplyr::left_join(df_dplyr, dict_sentiments, by = "word"))
#
# bench::mark(dict_sentiments %>%
#   dplyr::filter(word %in% df_dplyr$word) %>%
#   dplyr::left_join(df_dplyr, by = "word"))
#
#
# df_dt <- data.table::data.table(df_dplyr)
# dict_dt <- data.table::data.table(dict_sentiments)
#
# data.table::merge.data.table(df_dt, dict_dt, by = "word")
#
# bench::mark(data.table::merge.data.table(df_dt, dict_dt, by = "word"))
#
# df_dt[dict_dt, on = "word"]
#
# dict_dt[df_dt, on="word"]
#
# bench::mark(dict_dt[df_dt, on="word"])
