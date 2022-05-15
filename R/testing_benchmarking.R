# # testing, benchmarking, assorted tomfoolery
#
# testvec <- tardis1()
# testdplyr <- tardis2()
#
#
# bench::mark(tardis1())
# bench::mark(tardis2())
# input_text <- test$text[[5]]
#
# input_text <- "die in a fire ❤️❤️❤️❤️"
#
# tardis1("die in a fire")
# tardis1(input_text)
#
# test <- tidyvader::get_vader_dictionaries()$dictionary[[6]]
#
# purrr::map_dfr(test$text, tardis1)
#
#
#
#
# ########
# library(cpp11)
# library(Rcpp)
#
# # SORT AND KEEP TRACK OF INDICES! needed to align word and value vectors
# # https://stackoverflow.com/questions/1577475/c-sorting-and-keeping-track-of-indexes
# Rcpp::cppFunction("
# std::vector< int > cpp_str_sort3( std::vector< std::string > A) {
#
#   int N = A.size();
#
#  //Assume A is a given vector with N elements
#  std::vector<int> V(N);
#
#  std::iota(V.begin(),V.end(),0); //Initializing
#
#  sort( V.begin(),V.end(), [&](int i,int j){return A[i]<A[j];} );
#
#  return (V);
# }
# ")
#
# cpp11::cpp_source(code = '
# #include "cpp11/integers.hpp"
# #include "cpp11/strings.hpp"
# #include <numeric>
# [[cpp11::register]]
# cpp11::integers cpp_str_sort3_cpp11( cpp11::strings A) {
#
#   int N = A.size();
#
#  //Assume A is a given vector with N elements
#  cpp11::writable::integers V(N);
#
#  std::iota(V.begin(),V.end(),0); //Initializing
#
#  std::sort( V.begin(),V.end(), [&](int i,int j){return A[i]<A[j];} );
#
#  return (V);
# }
# ')
#
# cpp11::cpp_function('
#  cpp11::strings cpp_str_sort( cpp11::writable::strings A ) {
#
#   int len = A.size();
#
#   for( int i=0; i < len; i++ ) {
#     std::sort( A.begin(), A.end() );
#   }
#
#   return A;
# }')
#
#
# cpp11::cpp_source(code = '
# #include "cpp11/strings.hpp"
# #include <numeric>
# #include <vector>
# [[cpp11::register]]
#  std::vector <std::string> cpp_str_sort4_cpp( std::vector <std::string> A ) {
#
#   int len = A.size();
#
#   for( int i=0; i < len; i++ ) {
#     std::sort( A.begin(), A.end() );
#   }
#
#   return A;
# }')
#
# cpp_source(
#   code = '#include "cpp11/integers.hpp"
#
#   [[cpp11::register]]
#   int num_odd(cpp11::integers x) {
#     int total = 0;
#     for (int val : x) {
#       if ((val % 2) == 1) {
#         ++total;
#       }
#     }
#     return total;
#   }
#   ')
#
#
#
# # // This is really just a binary search. It takes as inputs a text vector and a sorted dictionary.
# # // It outputs an integer vector of  the same length as the input text vector.
# # // It loops through each word in the input vector and finds its index in the dictionary (if applicable).
# # // Words that aren't in the  dictionary get NA, which is what we want so we can use the results to index a vector in R.
# # // This is way faster than just doing the indexing in R, which was a bottleneck previously.
# cpp11::cpp_function("
# cpp11::doubles get_index_cpp11(cpp11::strings text, cpp11::strings sorted_dictionary) {
#
#   int n = text.size();
#   cpp11::writable::doubles out(n);
#
#   int dict_length = sorted_dictionary.size();
#
#
#   for (int i = 0; i < n; ++i){
#     int l = 0;
#     int r = dict_length - 1;
#     std::string t(text[i]);
#
#     // if no result, return NA. We want this (instead of, e.g., 0) because if you index an R vector with NA, you get
#     // NA in response: this means that the length of the indexed vector is the same as that of our input index. If
#     // you use 0 it seems to drop nonexistent entries, which causes problems.
#     out[i] = NA_INTEGER;
#
#     while (l <= r) {
#
#       int m = l + (r-l) / 2;
#
#       std::string comparator(sorted_dictionary[m]);
#
#       // check if x is present at mid. if so, return m+1 (r is not 0-indexed)
#       if (comparator == t){
#         out[i] = m+1;
#         break;
#       }
#
#       // if x greater, ignore left half
#       if (t > comparator)
#         l = m + 1;
#
#       // if x less, ignore right half
#       if (t < comparator)
#         r = m - 1;
#     }
#
#   }
#
#  // return dict_length;
#
#   return out;
# }
# ")
#
# Rcpp::cppFunction("
#                   IntegerVector get_index_rcpp(CharacterVector text, CharacterVector sorted_dictionary) {
#   int n = text.length();
#   IntegerVector out(n);
#
#   int dict_length = sorted_dictionary.length();
#
#   for (int i = 0; i < n; ++i){
#     int l = 0;
#     int r = dict_length - 1;
#     String t(text[i]);
#
#     // if no result, return NA. We want this (instead of, e.g., 0) because if you index an R vector with NA, you get
#     // NA in response: this means that the length of the indexed vector is the same as that of our input index. If
#     // you use 0 it seems to drop nonexistent entries, which causes problems.
#     out[i] = NA_INTEGER;
#
#     while (l <= r) {
#
#       int m = l + (r-l) / 2;
#
#       String comparator(sorted_dictionary[m]);
#
#       // check if x is present at mid. if so, return m+1 (r is not 0-indexed)
#       if (comparator == t){
#         out[i] = m+1;
#         break;
#       }
#
#       // if x greater, ignore left half
#       if (t > comparator)
#         l = m + 1;
#
#       // if x less, ignore right half
#       if (t < comparator)
#         r = m - 1;
#     }
#
#   }
#
#   return out;
# }")
#
# get_index_cpp11(rev(letters), letters)
# get_index_rcpp(rev(letters), letters)
#
# bench::mark(get_index_cpp11(rev(letters), letters))
# bench::mark(get_index_rcpp(rev(letters), letters))
#
# cpp_str_sort(c("a", "z","c"))
# cpp_str_sort2(c("a", "z","c"))
# cpp_str_sort3(c("a", "z","c"))
# cpp_str_sort4_cpp(c("a", "z","❤️", "c"))
# bench::mark(cpp_str_sort4_cpp(c("a", "z","❤️", "c")) )
#
#
#
# bench::mark(cpp_str_sort(c("a", "z","c")) )
# bench::mark(cpp_str_sort2(c("a", "z","c")) )
# bench::mark(cpp_str_sort3(c("a", "z","❤️", "c")) )
#
# bench::mark(cpp_str_sort(c("a", "z","❤️", "c")) )
#
# bench::mark(cpp_str_sort3(dict_sentiments$word))
# bench::mark(cpp_str_sort4_cpp(dict_sentiments$word))
#
# # SORT SENTIMENTS DICTIONARY AND REORDER --
# # Rcpp, ~ 1049 itr/sec, 914us tims
# bench::mark(dict_sentiments[cpp_str_sort3(dict_sentiments$word),])
#
# # cpp11,
# bench::mark(dict_sentiments[cpp_str_sort4_cpp(dict_sentiments$word),])
#
#
# ############# try ustom dict
# dict_sentiments <- tidyvader::get_vader_dictionaries()$dictionary[[1]] %>%
#   dplyr::add_row(word = "❤️", sentiment = 2.7) %>%
#   dplyr::add_row(word = "🤣", sentiment = 2.7)
#
# dict_sentiments <- dplyr::tibble(word = c("happy", "sad", "❤️"),
#                                  sentiment = c(2.7, -2.7, 4))
#
# dict_sentiments_sorted <- dict_sentiments[cpp_str_sort3(dict_sentiments$word),]
#
# test_sentence <- "I love to go to the happy beach ❤️ " %>%
#   stringr::str_split(pattern = " ") %>%
#   unlist() %>%
#   tolower()
#
# get_index_cpp11(test_sentence, dict_sentiments_sorted$word)
#
# # testing using the cpp functions NO IT DOESN'T SORT UNICODE EMOJIS IN RCPP ARRRG
# input_data <- dplyr::tibble(word = test_sentence)
#
# bench::mark(get_index_cpp11(input_data$word, dict_sentiments_sorted$word))
# bench::mark(get_index_rcpp(input_data$word, dict_sentiments_sorted$word))
#
# # get the sentiment for the word
# input_data$sentiment <- dict_sentiments_sorted$sentiment[get_index_rcpp(input_data$word, dict_sentiments_sorted$word)]
#
# bench::mark(input_data$sentiment <- dict_sentiments_sorted$sentiment[get_index_rcpp(input_data$word, dict_sentiments_sorted$word)])
#
# bench::mark(dplyr::left_join(input_data, dict_sentiments_sorted, by = "word"))
#
#
# # WHAT ABOUT JUST R
#
# bench::mark(tardis1(stringr::sentences))
# bench::mark(tardis2(stringr::sentences))
#
# # profiling
#
# input_text <- dplyr::tibble(sentences = c("hey lovely baby! you are awesome! ", "what's going on! ", "i love you "))
# input_text <- "hey lovely baby! what's going on! i love you "
# input_text <- c("hey lovely baby! you are awesome! ", "what's going on! ", "i love you ")
# input_text <- rep(stringr::sentences, 10)
# input_text <- dplyr::tibble(sentences = rep(stringr::sentences, 10))
#
# test_vader <- tardis1(input_text, "sentences")
# text_afinn <- tardis1(input_text, "sentences", dict_sentiments = dict_afinn)
#
#
# bench::mark(tardis1(input_text, "sentences"))
# bench::mark(tardis1(input_text, "sentences", dict_sentiments = dict_sentiments, dict_modifiers = dict_modifiers, dict_negations = dict_negations))
#
# tardis1(input_text = "the shit")
#
# profile <- profvis::profvis(tardis1(input_text, "sentences"))
# profile
#
# reddit_input <- pushshiftR::get_reddit_comments(q = 'volkswagen', size = 1000, batch_pause = 1)
# test <- tardis1(reddit_input, "body")
# bench::mark(tardis1(reddit_input, "body"))
#
#
# for (i in 1:20){
#   message(i)
#   bench_input <- rep(stringr::sentences, i)
#
#   if (i == 1) bench_results <- bench::mark(tardis1( bench_input))
#
#   if (i>1)   bench_results <- dplyr::bind_rows(bench_results,
#                                                bench::mark(tardis1( bench_input)))
# }
#
# bench_results$length = length(stringr::sentences) * (1:nrow(bench_results))
#
# library(ggplot2)
# ggplot(bench_results) +
#   geom_line(aes(x=length, y=median))
#
#
#
# # comparing different sentiment dictionaries
# input_text <- stringr::sentences
#
# dict_afinn <- tidytext::sentiments %>% dplyr::mutate(sentiment = dplyr::if_else(sentiment == "negative", -1, 1))
# test_afinn <- tardis1(input_text, dict_sentiments = dict_afinn ) %>%
#   dplyr::mutate(dictionary = "AFINN") %>%
#   dplyr::rename(sentiment_afinn = sentiment_mean)
#
# test_vader <- tardis1(input_text) %>%
#   dplyr::mutate(dictionary = "VADER") %>%
#   dplyr::rename(sentiment_vader = sentiment_mean)
#
# sentiment_all <- dplyr::left_join(test_afinn, test_vader, by = "text")
#
# sentiment_all %>%
#   ggplot() +
#   geom_point(aes(x=sentiment_vader, y=sentiment_afinn)) +
#   geom_smooth(aes(x=sentiment_vader, y=sentiment_afinn))

#dplyr::tibble(x =  seq(-20, 20, .1), y =  x / sqrt(x^2 + 20 )) %>% ggplot2::ggplot() + ggplot2::geom_line(ggplot2::aes(x=x,y=y))


## TESTING DICTIONARY WITH MULTI-NGRAM SENTIMENTS


dict_sentiments <- dplyr::tibble(word = c("good", "supreme court", "bad"), sentiment = c(2.7, 0, -2.7))
tardis1("the supreme court is a bad institution", dict_sentiments = dict_sentiments)
tardis1("the supreme court is a bad institution")
bench::mark(tardis1(rep(stringr::sentences, times = 10)))
