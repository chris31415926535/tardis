#include <Rcpp.h>
using namespace Rcpp;
//[[Rcpp::export]]
Rcpp::CharacterVector split_string_after_punctuation(std::string string_to_split) {

  Rcpp::CharacterVector out;

  int string_chars = string_to_split.length();

  int found_punct = 0;
  int last_sentence_start = 0;

  for (int i = 0; i < string_chars; i++){

    if (found_punct == 0){

      if(string_to_split[i] == '!' | string_to_split[i] == '?' | string_to_split[i] == '.') {
       // Rcout << "found new end of sentence at character" << i << "\n";
        found_punct = 1;
      }

    } else if (found_punct == 1){
      if ((string_to_split[i] != '!' & string_to_split[i] != '?' & string_to_split[i] != '.') | i == (string_chars-1)) {
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

  //Rcpp::String out_utf8 = out;
  //out.set_encoding(CE_UTF8);
  //return out_utf8;
  return out;
}



Rcpp::NumericVector get_nonzero_value_rcpp (Rcpp::NumericVector x, Rcpp::NumericVector y) {
  int n = x.size();
  Rcpp::NumericVector out(n);

  for (int i = 0; i < n; i++){
    if (x[i] == y[i]) out[i] = x[i];
    if (x[i] != 0 & y[i] == 0) out[i] = x[i];
    if (x[i] == 0 & y[i] != 0) out[i] = y[i];
    if (x[i] != 0 & y[i] != 0) out[i] = x[i]; // choose x arbitrarily
  }

  return out;
}
