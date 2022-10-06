# tardis 0.1.4

* Bugfix: multi-word tokens recognized properly when next to punctuation.
* Sigmoid function is now optional, enabling simpler word counting.
* New parameter: negation_factor, multiplier for damping sentiment after negation.
* New parameter: allcaps_factor, multiplier for ALL CAPS sentiment increases.
* Added parameters and tests to enable disabling modifiers and negations.
* Added parameter to disable punctuation analysis.
* Added unit tests for all new features.
* Internal code cleanup.

# tardis 0.1.3

* Updated CRAN release.
* Fix cpp code to use logical operators, not bitwise operators, for clang14 build.

# tardis 0.1.2

* First CRAN release.
* Added a `NEWS.md` file to track changes to the package.
* Migrated internally to use cpp11 for roughly 6x speed increase.


# tardis 0.1.1.9000

* Initial working version on GitHub.
