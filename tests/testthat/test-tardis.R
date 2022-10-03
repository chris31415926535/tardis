
testthat::test_that("Emojis are recognized",{
  testthat::expect_gt(tardis("❤️")$sentiment_mean , 0)
  testthat::expect_lt(tardis("😭")$sentiment_mean, 0)
  testthat::expect_gt(tardis(":)")$sentiment_mean , 0)
  testthat::expect_lt(tardis(":(")$sentiment_mean , 0)
})

testthat::test_that("Leading and trailing punctuation isn't considered",{
  testthat::expect_equal(tardis("(happy)")$sentiment_mean , tardis("happy")$sentiment_mean)
  testthat::expect_equal(tardis("'sad' is how i feel")$sentiment_mean , tardis("sad is how i feel")$sentiment_mean)
})

testthat::test_that("Capitalization amplifies sentiment",{
  testthat::expect_gt(tardis("HAPPY")$sentiment_mean , tardis("happy")$sentiment_mean)
  testthat::expect_lt(tardis("SAD")$sentiment_mean , tardis("sad")$sentiment_mean)
})

testthat::test_that("Punctuation amplifies sentiment",{
  testthat::expect_gt(tardis("happy!")$sentiment_mean , tardis("happy")$sentiment_mean)
  testthat::expect_lt(tardis("sad!")$sentiment_mean , tardis("sad")$sentiment_mean)
})

testthat::test_that("Punctuation and capitalization together amplifies sentiment",{
  testthat::expect_gt(tardis("HAPPY!")$sentiment_mean , tardis("happy!")$sentiment_mean)
  testthat::expect_lt(tardis("SAD!")$sentiment_mean , tardis("sad!")$sentiment_mean)
})

testthat::test_that("Cpp11 function to split sentences works properly", {
  temp_dict_sentiments <- dplyr::tibble(word = "happy")
  temp_dict_emojis <- ""
  test1 <- dplyr::tibble(sentences_orig = "hi! you!")
  test2 <- dplyr::tibble(sentences_orig = "HI!!!! you??!!! wow")
  test3 <- dplyr::tibble(sentences_orig = "hi.. there...?? you!!!!!")
  testthat::expect_equal(split_text_into_sentences_cpp11(test1 , temp_dict_emojis, temp_dict_sentiments )$sentence,
                         c("hi!", " you!"))
  testthat::expect_equal(split_text_into_sentences_cpp11(test2 , temp_dict_emojis, temp_dict_sentiments )$sentence,
                         c("HI!!!!", " you??!!!"," wow"))
  testthat::expect_equal(split_text_into_sentences_cpp11(test3 , temp_dict_emojis, temp_dict_sentiments )$sentence,
                         c("hi..", " there...??", " you!!!!!"))
})


testthat::test_that("Cpp11 function to take pairwise nonzero value from two vectors", {
  testthat::expect_equal(get_nonzero_value_cpp11(c(1,0,1),
                                                 c(0,2,2)),
                         c(1,2,1))
})


testthat::test_that("Custom dictionaries with no emojis work properly", {
  custom_dict <- dplyr::tribble(~word, ~sentiment,
                                "happy", 5,
                                "sad", -5)
  testthat::expect_gt(tardis("happy", dict_sentiments = custom_dict)$sentiment_mean, 0)
  testthat::expect_equal(tardis("jumpin' jehosephat")$sentiment_mean, 0)
  testthat::expect_lt(tardis("sad", dict_sentiments = custom_dict)$sentiment_mean, 0)

})

testthat::test_that("Custom dictionaries that are ONLY emojis work properly", {
  custom_dict <- dplyr::tribble(~word, ~sentiment,
                                "❤️", 5,
                                "😭", -5)
  testthat::expect_gt(tardis("❤️", dict_sentiments = custom_dict)$sentiment_mean, 0)
  testthat::expect_equal(tardis("jumpin' anacondas")$sentiment_mean, 0)
  testthat::expect_lt(tardis("😭", dict_sentiments = custom_dict)$sentiment_mean, 0)

})

testthat::test_that("Custom dictionaries with text and emojis work properly", {
  custom_dict <- dplyr::tribble(~word, ~sentiment,
                                "❤️", 5,
                                "sadness", -5,
                                ":D", 5)
  testthat::expect_gt(tardis("❤️", dict_sentiments = custom_dict)$sentiment_mean, 0)
  testthat::expect_gt(tardis("time for lunch :D", dict_sentiments = custom_dict)$sentiment_mean, 0)
  testthat::expect_equal(tardis("jumpin' jehosephat")$sentiment_mean, 0)
  testthat::expect_lt(tardis("sadness", dict_sentiments = custom_dict)$sentiment_mean, 0)

})

testthat::test_that("Custom dictionaries with multi-word tokens work properly",{
  custom_dict <- dict_tardis_sentiment %>%
    dplyr::add_row(word = "supreme court", sentiment = 0) %>%
    dplyr::add_row(word = "happy sad", sentiment = 0) %>%
    dplyr::add_row(word = "oh dear", sentiment = -3)


  # if multi-word tokens  have sentiment-bearing sub-components, the subcomponents still work fine
  testthat::expect_gt(tardis("supreme", dict_sentiments = custom_dict)$sentiment_mean, 0)
  testthat::expect_equal(tardis("supreme court", dict_sentiments = custom_dict)$sentiment_mean, 0)

  testthat::expect_gt(tardis("happy", dict_sentiments = custom_dict)$sentiment_mean, 0)
  testthat::expect_equal(tardis("happy sad", dict_sentiments = custom_dict)$sentiment_mean, 0)
  testthat::expect_lt(tardis("sad", dict_sentiments = custom_dict)$sentiment_mean, 0)

  # multi-word tokens work when next to punctuation
  testthat::expect_lt(tardis("oh dear", dict_sentiments = custom_dict)$sentiment_mean, 0)
  testthat::expect_lt(tardis("oh dear!", dict_sentiments = custom_dict)$sentiment_mean,
                      tardis("oh dear", dict_sentiments = custom_dict)$sentiment_mean)
  testthat::expect_lt(tardis("oh dear!", dict_sentiments = custom_dict)$sentiment_mean, 0)
  })
