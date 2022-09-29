## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.


## 2022-09-27: Changes at Benjamin Altmann's request for resubmission

* In file DESCRIPTION, all package names, software names, and API names have been removed from all fields. Previously some were included without single quotes.
* In file DESCRIPTION, the single Author/Creator/Copyright Holder is now declared using the Author@R field and the function person().
* In file DESCRIPTION, no changes have been made to references, because the methods in the package are new and are not described in any published works. Some of the package documentation includes references to data sources and other relevant work, but none of these sources describe the package methods.

## 2022-09-29: Changes at Prof Brian Ripley's request for resubmission

* In file src/code.cpp, changed bitwise operators '|' and '&' to logical operators '||' and '&&' to resolve warnings in build under r-devel-linux-x86_64-fedora-clang. 
  * Package builds clean locally and passes all unit tests.
  * Clean results from rhub::check(platforms = "fedora-clang-devel").
