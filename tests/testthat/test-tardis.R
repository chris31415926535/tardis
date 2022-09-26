
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
