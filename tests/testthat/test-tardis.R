
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


testthat::test_that("Punctuation can be disabled",{
  testthat::expect_equal(tardis("happy!", use_punctuation = FALSE)$sentiment_mean , tardis("happy")$sentiment_mean)
  testthat::expect_equal(tardis("sad!!?!?!", use_punctuation = FALSE)$sentiment_mean , tardis("sad")$sentiment_mean)
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


testthat::test_that("Modifiers work as expected", {
  # modifiers amplify direction
  testthat::expect_gt(tardis("very happy")$sentiment_mean, tardis("happy")$sentiment_mean)
  testthat::expect_lt(tardis("very sad")$sentiment_mean, tardis("sad")$sentiment_mean)

  # modifiers work up to 3 steps back but no more
  testthat::expect_gt(tardis("very very happy")$sentiment_mean, tardis("very happy")$sentiment_mean)
  testthat::expect_gt(tardis("very very very happy")$sentiment_mean, tardis("very very happy")$sentiment_mean)
  testthat::expect_equal(tardis("very very very very happy")$sentiment_mean, tardis("very very very happy")$sentiment_mean)

  # multi-word modifiers work okay
  custom_modifiers <- dplyr::tibble(word = c("very", "gosh darn"), booster_value = 0.293, booster_sign=1)
  testthat::expect_gt(tardis("very happy", dict_modifiers = custom_modifiers)$sentiment_mean, tardis("happy", dict_modifiers = custom_modifiers)$sentiment_mean)
  testthat::expect_gt(tardis("gosh darn happy", dict_modifiers = custom_modifiers)$sentiment_mean, tardis("happy", dict_modifiers = custom_modifiers)$sentiment_mean)
  testthat::expect_lt(tardis("gosh darn sad", dict_modifiers = custom_modifiers)$sentiment_mean, tardis("sad", dict_modifiers = custom_modifiers)$sentiment_mean)
  testthat::expect_equal(tardis("gosh darn happy", dict_modifiers = custom_modifiers)$sentiment_mean, tardis("very happy", dict_modifiers = custom_modifiers)$sentiment_mean)

  # modifiers can be disabled
  testthat::expect_equal(tardis("very happy", dict_modifiers = "none")$sentiment_mean, tardis("happy")$sentiment_mean)
  testthat::expect_equal(tardis("very sad", dict_modifiers = "none")$sentiment_mean, tardis("sad")$sentiment_mean)
})

testthat::test_that("Negations work as expected",{

  # negations flip direction
  testthat::expect_lt(tardis("not happy")$sentiment_mean, tardis("happy")$sentiment_mean)
  testthat::expect_gt(tardis("not sad")$sentiment_mean, tardis("sad")$sentiment_mean)

  # negations damp effect sizes
  testthat::expect_lt(abs(tardis("not happy")$sentiment_mean), abs(tardis("happy")$sentiment_mean))
  testthat::expect_lt(abs(tardis("not sad")$sentiment_mean), abs(tardis("sad")$sentiment_mean))

  # negations work up to 3 steps back but no more
  testthat::expect_lt(abs(tardis("not not happy")$sentiment_mean), abs(tardis("not happy")$sentiment_mean))
  testthat::expect_lt(abs(tardis("not not not happy")$sentiment_mean), abs(tardis("not not happy")$sentiment_mean))
  testthat::expect_equal(tardis("not not not not happy")$sentiment_mean, tardis("not not not happy")$sentiment_mean)

  # negations can be disabled
  testthat::expect_equal(tardis("not happy", dict_negations = "none")$sentiment_mean, tardis("happy")$sentiment_mean)
  testthat::expect_equal(tardis("not sad", dict_negations = "none")$sentiment_mean, tardis("sad")$sentiment_mean)

})


testthat::test_that("Parameter simple_count works as expected", {
  testthat::expect_equal(tardis(stringr::sentences,
                                sigmoid_factor = NA, allcaps_factor = 1, dict_modifiers = "none", dict_negations = "none", use_punctuation = FALSE),
                         suppressWarnings(tardis(stringr::sentences, simple_count = TRUE)))
})
