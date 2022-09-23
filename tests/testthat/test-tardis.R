
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
