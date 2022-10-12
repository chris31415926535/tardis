
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Text Analysis with Rules and Dictionaries for Inferring Sentiment (TARDIS)

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/tardis)](https://CRAN.R-project.org/package=tardis)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/chris31415926535/tardis/branch/main/graph/badge.svg)](https://app.codecov.io/gh/chris31415926535/tardis?branch=main)
[![R-CMD-check](https://github.com/chris31415926535/tardis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/chris31415926535/tardis/actions/workflows/R-CMD-check.yaml)
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

## Features

-   Handles ASCII and UTF-8 emojis :) 👍
-   Based on simple surveyable rules
-   Highly customizable
-   Pretty fast, uses cpp11

## Installation

The latest stable CRAN version can be installed as follows:

``` r
install.packages("tardis")
```

You can install the latest development version of tardis from GitHub
like so:

``` r
devtools::install_github("chris31415926535/tardis")
```

## Example

Let’s find the sentiment of a few sentences:

``` r
library(tardis)

text <- c("I am happy.",
          "I am really happy.",
          "I am really happy!",
          "I am really not happy!")

tardis::tardis(text) %>%
  dplyr::select(sentences_orig, score) %>%
  knitr::kable()
```

| sentences_orig         |      score |
|:-----------------------|-----------:|
| I am happy.            |  0.5718850 |
| I am really happy.     |  0.6695383 |
| I am really happy!     |  0.6987571 |
| I am really not happy! | -0.5968026 |

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
  dplyr::select(sentences_orig, score, score_sd, score_range) %>%
  knitr::kable()
```

| sentences_orig                                                                        |     score |  score_sd | score_range |
|:--------------------------------------------------------------------------------------|----------:|----------:|------------:|
| This sentence is neutral. This one is really happy! This one is absolutely miserable. | 0.0613416 | 0.6455055 |    1.290718 |

Or even passive-aggressive hostility:

``` r
text <- "Die in a fire 😘" 

tardis::tardis(text) %>%
  dplyr::select(sentences_orig, score, score_sd, score_range) %>%
  knitr::kable()
```

| sentences_orig  |      score |  score_sd | score_range |
|:----------------|-----------:|----------:|------------:|
| Die in a fire 😘 | -0.0664554 | 0.9568319 |    1.353165 |

## The algorithm

-   Split text into sentences.
-   Count the number of exclamation points and question marks in the
    sentence.
-   Split each sentence into tokens. These will usually be words, but
    can also be multi-word strings (e.g. “supreme court”) or emojis.
    -   Strip each token’s leading/trailing whitespace and check each
        token against the dictionary. This captures emojis like “:)”.
    -   Strip each token’s leading/trailing punctuation.
    -   Check to see if each token is capitalized.
    -   Check to see if each token is a modifier (e.g. “very”,
        “somewhat”).
    -   Check to see if each token is a negator (e.g. “never”, “not”).
    -   Make each token lowercase and check it against the dictionary.
        -   Note this is the second time we check the dictionary: the
            first time uses original tokens so will match things like
            “:)” but not like “Happy!” and this time we’ve removed
            punctuation and made it lowercase so it will match “happy”.
-   Now we have token-level information: its raw dictionary sentiment,
    if it was capitalized, if it is a modifier, or if it is a negator.
    -   We compute each token’s *modified* sentiment, which is a
        function of its own raw sentiment (if applicable), whether it
        was all-caps, and the three preceding tokens.
    -   For each negator in the preceding 3 tokens, we flip the current
        token’s valence and multiply by a value less than 1. The default
        is -0.75.
        -   In other words, sentiment changes direction and becomes more
            muted. “Not bad” is not “bad,” but not as good as “Good.”
    -   For each modifier in the preceding 3 tokens, we multiply the
        current token’s score by the appropriate value to scale
        sentiment up or down. Modifiers are attenuated the farther back
        they are.
    -   If the token was ALL CAPS, we scale its sentiment up. The
        default scale factor is 0.25.
-   We now have our modified token values, which we combine into raw
    sentence scores.
    -   We sum all of the modified token scores.
    -   We add or subtract any sentence-level punctuation score from
        exclamation points and question marks.
-   We then scale our raw sentence scores to be between -1 and 1 using
    the sigmoid function $x / \sqrt(x^2 + 15)$. *Note that this should
    be a user-supplied parameter.*
-   We then compute text-level scores by finding the mean, sd, and range
    of sentence scores within each text.

## Benchmarking

The major bottlenecks have been addressed using `cpp11` so the function
is reasonably fast, handling roughly 13000 sentences/second using test
data from `stringr::sentences`:

<img src="man/figures/README-benchmark_plot-1.png" width="100%" />

## Known issues / Possible future directions

-   Providing more fine-grained control over the algorithm
    (e.g. disabling modifiers or negators, disabling the final sigmoid
    function).
-   Testing and generalizing with other dictionaries/semantic
    constructs.
-   Adding a convenience function to analyze text with more than one
    dictionary at once.

## Similar projects and packages

-   Tardis was directly inspired by
    [VADER](https://github.com/cjhutto/vaderSentiment), which has an R
    implementation on CRAN in the package
    [vader](https://cran.r-project.org/package=vader), and an
    implementation I wrote that’s not on cran called
    [tidyvader](https://github.com/chris31415926535/tidyvader). Tardis
    also incorporates sentiment data from the VADER project.
-   [Tidytext](https://github.com/juliasilge/tidytext) is a wonderful
    package for text mining in R. Tardis incorporates some sentiment
    data from Tidytext.

## References

Hutto, C.J. & Gilbert, E.E. (2014). VADER: A Parsimonious Rule-based
Model for Sentiment Analysis of Social Media Text. Eighth International
Conference on Weblogs and Social Media (ICWSM-14). Ann Arbor, MI, June
2014.

Kralj Novak P, Smailović J, Sluban B, Mozetič I (2015) Sentiment of
Emojis. PLoS ONE 10(12): e0144296.
<https://doi.org/10.1371/journal.pone.0144296>

Minqing Hu and Bing Liu, “Mining and summarizing customer reviews.”,
Proceedings of the ACM SIGKDD International Conference on Knowledge
Discovery & Data Mining (KDD-2004), Seattle, Washington, USA, Aug 22-25,
2004.
