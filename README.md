
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Text Analysis with Rules and Dictionaries for Inferring Sentiment (TARDIS)

<!-- badges: start -->
<!-- badges: end -->

TARDIS uses simple rules and dictionaries to analyze text. By default it
uses built-in dictionaries to measure sentiment, i.e. how happy or sad
text is. It handles negations, so it knows “not happy” means “sad”, and
it handles modifiers, so it knows that “very happy” is more happy than
“happy”. TARDIS also supports unicode emojis and multi-word tokens (so
you can tell it that “supreme court” is neutral, instead of a
combination of “supreme” (positive) and “court” (neutral). TARDIS also
supports user-defined dictionaries and can be used to analyze other
constructs beyond sentiment.

## Installation

You can install the development version of tardis like so:

``` r
devtools::install_github("chris31415926535/tardis")
```

## Example

Let’s find the sentiment of a few sentences:

``` r
library(tardis)

text <- c("I am happy.",
          "I am really happy.",
          "I am really happy!")

tardis::tardis(text) %>%
  dplyr::select(text, sentiment_mean) %>%
  knitr::kable()
```

| text               | sentiment_mean |
|:-------------------|---------------:|
| I am happy.        |      0.5718850 |
| I am really happy. |      0.6695383 |
| I am really happy! |      0.6987571 |

Tardis also handles blocks of text differently from other
sentiment-analysis algorithms, most of which treat blocks of text as
single sentences. Instead, Tardis breaks each text into individual
sentences, finds their sentiment, and then returns the text’s mean,
standard deviation, and range. This can be helpful for finding large
swings in sentiment that could indicate irony or conflict in texts that
may be close to neutral overall.

``` r
text <- "This sentence is neutral. This one is really happy! This one is absolutely miserable."

tardis::tardis(text) %>%
  dplyr::select(text, sentiment_mean, sentiment_sd, sentiment_range) %>%
  knitr::kable()
```

| text                                                                                  | sentiment_mean | sentiment_sd | sentiment_range |
|:--------------------------------------------------------------------------------------|---------------:|-------------:|----------------:|
| This sentence is neutral. This one is really happy! This one is absolutely miserable. |      0.0613416 |    0.6455055 |        1.290718 |

## Known issues

-   ASCII emojis like :) and :( aren’t supported yet.
-   Bug: Sentences with more than one punctuation mark aren’t handled
    properly.
