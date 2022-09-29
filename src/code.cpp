#include "cpp11.hpp"
using namespace cpp11;
[[cpp11::register]]
cpp11::strings split_string_after_punctuation_cpp11(std::string string_to_split) {

  writable::strings out;

  int string_chars = string_to_split.length();

  int found_punct = 0;
  int last_sentence_start = 0;

  for (int i = 0; i < string_chars; i++){

    if (found_punct == 0){

      if( (string_to_split[i] == '!') || (string_to_split[i] == '?') || (string_to_split[i] == '.') ) {
       // Rcout << "found new end of sentence at character" << i << "\n";
        found_punct = 1;
      }

    } else if (found_punct == 1){
      if (((string_to_split[i] != '!') && (string_to_split[i] != '?') && (string_to_split[i] != '.')) ) {
        out.push_back(string_to_split.substr(last_sentence_start, (i - last_sentence_start)));
        found_punct = 0;
        last_sentence_start = i;

      }

    } // end of found_punct == 1

    if (i == (string_chars-1)){
     // Rcout << "found  end of text at character" << i << "\n";
      out.push_back(string_to_split.substr(last_sentence_start, (i+1 - last_sentence_start)));
    }


  } // end for loop


  return out;
}


[[cpp11::register]]
cpp11::doubles get_nonzero_value_cpp11(cpp11::doubles x, cpp11::doubles y) {
  int n = x.size();

  writable::doubles out(n);

  for (int i = 0; i < n; i++){
    if (x[i] == y[i]) out[i] = x[i];
    if ((x[i] != 0) && (y[i] == 0)) out[i] = x[i];
    if ((x[i] == 0) && (y[i] != 0)) out[i] = y[i];
    if ((x[i] != 0) && (y[i] != 0)) out[i] = x[i]; // choose x arbitrarily
  }

  return out;
}
