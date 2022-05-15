# Rcpp::cppFunction("
# std::vector <std::string> do_nothing_rcpp (std::vector <std::string> X){
#   return X;
# }
# ")
#
#
# Rcpp::cppFunction("
# std::vector <std::string> basic_sort_rcpp (std::vector <std::string> X){
#
#   std::sort(X.begin(), X.end());
#
#   return X;
# }
# ")
#
# Rcpp::cppFunction('
# void count_to_x_times_y (int x, int y){
#
#   std::string A;
#   A = "happy";
#   int z;
#
#   for (int i; i < y; i++){
#     for (int j; j < x; j++){
#       z = 5*5; // do something
#       A = "sad";
#     }
#
#   }
#
#   return;
# }
# ')
#
# Rcpp::cppFunction('
# std::vector <double> get_value_from_string (std::vector <std::string> input_words, std::vector <std::string> dict_words, std::vector <double> dict_values){
#
#
#
#   int input_length = input_words.size();
#   int dict_length = dict_words.size();
#
#   std::vector <double> output_values(input_length);
#
#   // for each word in the input
#   for (int i = 0; i < input_length; i++){
#
#     // check each dictionary word
#     for (int j = 0; j < dict_length; j++){
#       if (input_words[i] == dict_words[j]){
#         output_values[i] = dict_values[j];
#         continue;
#
#       }
#
#     }
#
#   }
#
# }
# ')
#
# get_value_from_string(letters, LETTERS, c(1,2,3.3))
# get_value_from_string(c("happy", "fuck"), dict_sentiments$word, dict_sentiments$sentiment)
#
# test_text <- c("happy", "fuck", "😐", "❤️")
# get_value_from_string(test_text, dict_sentiments$word, dict_sentiments$sentiment)
# bench::mark(get_value_from_string(test_text, dict_sentiments$word, dict_sentiments$sentiment))
#
# dict_sentiments_vec[test_text]
# bench::mark(dict_sentiments_vec[test_text])
# # doing it using the basic unsorted cpp routine IS faster than doing it with vectors in R...
#
#
#
# do_nothing_rcpp("sadf")
# do_nothing_rcpp("😁") == "😁"
#
# do_nothing_rcpp(c("😁", "x", "😐") )
# sort_rcpp(c("😁", "x", "😐") )
#
# bench::mark(sort_rcpp(dict_sentiments$word))
#
# bench::mark(count_to_x_times_y(x = 8000, y = 1000000))
