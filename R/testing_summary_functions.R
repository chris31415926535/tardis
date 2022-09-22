
# Testing custom functions for the sentence summaries, which is the new bottleneck
#
# bench::mark({
#   result_sentences <- result %>%
#     dplyr::group_by(text_id, sentence_id) %>%
#     dplyr::summarise(sentence_sum = sum(sentiment_word, na.rm = TRUE),
#                      sentence_punct = min(punct_exclamation, 4) * 0.292 + min(punct_question * 0.18, 0.96)
#                      #,sentence_swing = max(sentiment_word) - min(sentiment_word)
#                      , .groups = "drop_last"
#     ) %>%
#     #dplyr::mutate(sentence_score = dplyr::if_else(sentence_sum > 0, sentence_sum + sentence_punct, sentence_sum - sentence_punct)) %>%
#     # vectorized add punctuation in the signed direction, only if not zero. (otherwise subtracted when it shouldn't)
#     dplyr::mutate(sentence_score = dplyr::if_else(abs(sentence_sum) > 0, sentence_sum + sentence_punct* (sentence_sum/abs(sentence_sum)), sentence_sum)) %>%
#     dplyr::mutate(sentence_score = sentence_score / sqrt((sentence_score * sentence_score) + 15)) %>%
#     dplyr::select(-sentence_sum, -sentence_punct)
#   #manual_summary(result)
#
# }) %>% dplyr::pull(median)
#
# bench::mark({
#   dplyr::tibble(sentence_score = manual_summary_rcpp(sentence_id = result$sentence_id, sentiment_word = result$sentiment_word, punct_exclamation = result$punct_exclamation, punct_question = result$punct_question))
# }) %>% dplyr::pull(median)
#
# bench::mark({
#   manual_summary_rcppdf2(result)
# }) %>% dplyr::pull(median)
#
#
# # DOING VECTORIZED SUMMARY
# # This custom Rcpp function is roughly 200-400x faster than the following dplyr code
# # The dplyr was previously a bottleneck! Bottleneck eliminated!
# # result %>%
# #   dplyr::group_by(text_id, sentence_id) %>%
# #   dplyr::summarise(sentence_sum = sum(sentiment_word, na.rm = TRUE),
# #                    sentence_punct = min(punct_exclamation, 4) * 0.292 + min(punct_question * 0.18, 0.96)
# #                    #,sentence_swing = max(sentiment_word) - min(sentiment_word)
# #                    , .groups = "drop_last"
# #   ) %>%
# #   # vectorized add punctuation in the signed direction, only if not zero. (otherwise subtracted when it shouldn't)
# #   dplyr::mutate(sentence_score = dplyr::if_else(abs(sentence_sum) > 0, sentence_sum + sentence_punct* (sentence_sum/abs(sentence_sum)), sentence_sum)) %>%
# #   dplyr::mutate(sentence_score = sentence_score / sqrt((sentence_score * sentence_score) + 15))
# Rcpp::cppFunction('
# std::vector <double> manual_summary_rcpp(std::vector <int> sentence_id, std::vector <double> sentiment_word, std::vector <double> punct_exclamation, std::vector <double> punct_question) {
#
#   int sentence_index = 1;
#   int first_index = 0;
#   int num_words = sentence_id.size();
#
#   // need * because max_element returns an iterator: https://stackoverflow.com/questions/9874802/how-can-i-get-the-maximum-or-minimum-value-in-a-vector
#   int num_sentences = *max_element(std::begin(sentence_id), std::end(sentence_id));
#
#   //printf("num words: %i\\n ", num_words);
#   //printf("num sentences: %i \\n", num_sentences);
#
#
#   std::vector <double> sentence_sums(num_sentences);
#   std::vector <double> sentence_puncts(num_sentences);
#   std::vector <double> sentence_scores(num_sentences);
#
#   // loop through all words
#   for (int i = 0; i <= num_words; i++) {
#
#     // if new sentence
#     if ((sentence_id[i] != sentence_index) & i > 0){
#
#       //printf("\\nNew sentence found");
#       // sum the word sentiments in the range we identified
#       for (int j = first_index; j <= i-1; j++){
#
#         if (!Rcpp::NumericVector::is_na(sentiment_word[j])) {
#           sentence_sums[sentence_index-1] += sentiment_word[j];
#         }
#
#       }
#
#       //Do punctuation
#
#       int num_exclamation = punct_exclamation[i-1];
#       int num_question    = punct_question[i-1];
#       sentence_puncts[sentence_index-1] = std::min(num_exclamation, 4) * 0.292 + std::min((num_question * 0.18), 0.96);
#
#       // set new index
#       first_index = i;
#       sentence_index = sentence_id[i];
#
#     } // end if new sentence
#
#   } // end for loop i through words
#
#
#   // now loop through sentences again to get summmary scores
#
#   for (int i = 0; i < num_sentences; i++){
#
#     // punctuation scores go in the same direction as overall word scores
#     // NB they are additive, not multiplicative
#     if (sentence_sums[i] > 0){
#       sentence_scores[i] = sentence_sums[i] + sentence_puncts[i];
#     }
#
#     if (sentence_sums[i] < 0){
#       sentence_scores[i] = sentence_sums[i] - sentence_puncts[i];
#     }
#
#     // compute final score using logit function to compress within (-1, 1)
#     // FIXME TODO make this a parameter / optional
#     sentence_scores[i] = sentence_scores[i] / std::sqrt(sentence_scores[i] * sentence_scores[i] + 15);
#   }
#
# return sentence_scores;
#
# }')
#
#
# ### CUSTOM SUMMARY FUNCTUON RETURNING DATAFRAME
# Rcpp::cppFunction('
# DataFrame manual_summary_rcppdf(std::vector <int> sentence_id, std::vector <double> sentiment_word, std::vector <double> punct_exclamation, std::vector <double> punct_question) {
#
#   int sentence_index = 1;
#   int first_index = 0;
#   int num_words = sentence_id.size();
#
#   // need * because max_element returns an iterator: https://stackoverflow.com/questions/9874802/how-can-i-get-the-maximum-or-minimum-value-in-a-vector
#   int num_sentences = *max_element(std::begin(sentence_id), std::end(sentence_id));
#
#   //printf("num words: %i\\n ", num_words);
#   //printf("num sentences: %i \\n", num_sentences);
#
#
#   std::vector <double> sentence_sums(num_sentences);
#   std::vector <double> sentence_puncts(num_sentences);
#   std::vector <double> sentence_scores(num_sentences);
#   std::vector <int>    sentence_ids_output(num_sentences);
#
#   // loop through all words
#   for (int i = 0; i <= num_words; i++) {
#
#     // if new sentence
#     if ((sentence_id[i] != sentence_index) & i > 0){
#
#       //printf("\\nNew sentence found");
#       // sum the word sentiments in the range we identified
#       for (int j = first_index; j <= i-1; j++){
#
#         if (!Rcpp::NumericVector::is_na(sentiment_word[j])) {
#           sentence_sums[sentence_index-1] += sentiment_word[j];
#         }
#
#       }
#
#       //Do punctuation
#
#       int num_exclamation = punct_exclamation[i-1];
#       int num_question    = punct_question[i-1];
#       sentence_puncts[sentence_index-1] = std::min(num_exclamation, 4) * 0.292 + std::min((num_question * 0.18), 0.96);
#
#       // set new index
#       first_index = i;
#       sentence_index = sentence_id[i];
#
#     } // end if new sentence
#
#   } // end for loop i through words
#
#
#   // now loop through sentences again to get summmary scores
#
#   for (int i = 0; i < num_sentences; i++){
#
#     // punctuation scores go in the same direction as overall word scores
#     // NB they are additive, not multiplicative
#     if (sentence_sums[i] > 0){
#       sentence_scores[i] = sentence_sums[i] + sentence_puncts[i];
#     }
#
#     if (sentence_sums[i] < 0){
#       sentence_scores[i] = sentence_sums[i] - sentence_puncts[i];
#     }
#
#     // compute final score using logit function to compress within (-1, 1)
#     // FIXME TODO make this a parameter / optional
#     sentence_scores[i] = sentence_scores[i] / std::sqrt(sentence_scores[i] * sentence_scores[i] + 15);
#   }
#
#   // create vector of sentence ids
#   for (int i = 0; i < num_sentences; i++) {
#    sentence_ids_output[i] = i + 1;
#   }
#
#
#   DataFrame result = DataFrame::create( Named("sentence_score") = sentence_scores,
#                                       Named("sentence_id") = sentence_ids_output);
#
#   return result;
#
# }')
#
#
# ### CUSTOM SUMMARY FUNCTION ACCEPTING AND RETURNING DATAFRAME
# Rcpp::cppFunction('
# DataFrame manual_summary_rcppdf2(DataFrame input_df ){ //std::vector <int> sentence_id, std::vector <double> sentiment_word, std::vector <double> punct_exclamation, std::vector <double> punct_question) {
#
#   // pull vectors we need from the input dataframe
#   std::vector <int> text_id = input_df["text_id"];
#   std::vector <int> sentence_id = input_df["sentence_id"];
#   std::vector <double> sentiment_word = input_df["sentiment_word"];
#   std::vector <double> punct_exclamation = input_df["punct_exclamation"];
#   std::vector <double> punct_question = input_df["punct_question"];
#   //std::vector <std::string> sentence_text = input_df["sentence"];
#
#   //set up some values
#   int sentence_index = 1;
#   int first_index = 0;
#   int num_words = sentence_id.size();
#
#   // need * because max_element returns an iterator: https://stackoverflow.com/questions/9874802/how-can-i-get-the-maximum-or-minimum-value-in-a-vector
#   int num_sentences = *max_element(std::begin(sentence_id), std::end(sentence_id));
#
#   //printf("num words: %i\\n ", num_words);
#   //printf("num sentences: %i \\n", num_sentences);
#
#   // set up our output vectors
#   std::vector <double> sentence_sums(num_sentences);
#   std::vector <double> sentence_puncts(num_sentences);
#   std::vector <double> sentence_scores(num_sentences);
#   std::vector <int>    sentence_ids_output(num_sentences);
#   std::vector <int>    text_ids_output(num_sentences);
#   //std::vector <std::string> sentence_text_output(num_sentences);
#
#   // loop through all words
#   for (int i = 0; i <= num_words; i++) {
#
#     // if new sentence
#     if ((sentence_id[i] != sentence_index) & i > 0){
#
#       //printf("\\nNew sentence found");
#       // sum the word sentiments in the range we identified
#       for (int j = first_index; j <= i-1; j++){
#
#         if (!Rcpp::NumericVector::is_na(sentiment_word[j])) {
#           sentence_sums[sentence_index-1] += sentiment_word[j];
#         }
#
#       }
#
#       //Do punctuation
#
#       int num_exclamation = punct_exclamation[i-1];
#       int num_question    = punct_question[i-1];
#       sentence_puncts[sentence_index-1] = std::min(num_exclamation, 4) * 0.292 + std::min((num_question * 0.18), 0.96);
#
#       // set text id FIXME and maybe sentence id?
#       text_ids_output[sentence_index-1] = text_id[i-1];
#     //  sentence_text_output[sentence_index-1] = sentence_text[i-1];
#
#       // set new index
#       first_index = i;
#       sentence_index = sentence_id[i];
#
#
#
#     } // end if new sentence
#
#   } // end for loop i through words
#
#
#   // now loop through sentences again to get summmary scores
#
#   for (int i = 0; i < num_sentences; i++){
#
#     // punctuation scores go in the same direction as overall word scores
#     // NB they are additive, not multiplicative
#     if (sentence_sums[i] > 0){
#       sentence_scores[i] = sentence_sums[i] + sentence_puncts[i];
#     }
#
#     if (sentence_sums[i] < 0){
#       sentence_scores[i] = sentence_sums[i] - sentence_puncts[i];
#     }
#
#     // compute final score using logit function to compress within (-1, 1)
#     // FIXME TODO make this a parameter / optional
#     sentence_scores[i] = sentence_scores[i] / std::sqrt(sentence_scores[i] * sentence_scores[i] + 15);
#   }
#
#   // create vectors of text ids and sentence ids
#   for (int i = 0; i < num_sentences; i++) {
#    sentence_ids_output[i] = i + 1;
#
#   }
#
#
#   DataFrame result = DataFrame::create( Named ("text_id") = text_ids_output,
#                                         Named("sentence_id") = sentence_ids_output,
#                                        // Named("sentence") = sentence_text_output,
#                                         Named("sentence_score") = sentence_scores);
#
#   return result;
#
# }')
#
#
# manual_summary_rcpp(sentence_id = result$sentence_id, sentiment_word = result$sentiment_word, punct_exclamation = result$punct_exclamation, punct_question = result$punct_question)
# test <- manual_summary_rcppdf(sentence_id = result$sentence_id, sentiment_word = result$sentiment_word, punct_exclamation = result$punct_exclamation, punct_question = result$punct_question)
# manual_summary_rcppdf2(result)
# manual_summary(result)
# bench::mark(manual_summary_rcppdf2(result))
# bench::mark(manual_summary_rcppdf(sentence_id = result$sentence_id, sentiment_word = result$sentiment_word, punct_exclamation = result$punct_exclamation, punct_question = result$punct_question)) %>% dplyr::pull(median)
# bench::mark(manual_summary(result)) %>% dplyr::pull(median)
#
#
#
# bench::mark(manual_summary_rcpp(sentence_id = result$sentence_id, sentiment_word = result$sentiment_word, punct_exclamation = result$punct_exclamation, punct_question = result$punct_question)) %>% dplyr::pull(median)
#
# manual_summary <- function(result) {
#   sentence_index <- 1
#   num_sentences <- max(result$sentence_id)
#   sentence_sums <- vector(mode = "numeric", length = num_sentences)
#   sentence_puncts <- vector(mode = "numeric", length = num_sentences)
#   first_index <- 1
#
#   for (i in 1:(nrow(result))){
#     # if new sentence
#     #  message(sprintf("i=%s    ; sentence_index=%s", i, sentence_index))
#     if (((result$sentence_id[[i]] != sentence_index) & (i > 1)) ){
#       # update last sentence mean
#       sentence_sums[[sentence_index]] <- sum(result$sentiment_word[first_index:(i-1)], na.rm = TRUE)
#       sentence_puncts[[sentence_index]] <- min(result$punct_exclamation[i-1], 4) * 0.292 + min(result$punct_question[i-1] * 0.18, 0.96)
#       # set new index
#       first_index <- i
#       sentence_index <- result$sentence_id[[i]]
#     }
#
#     # handle last word in last sentence
#     # lsat word in single-word sentence, update sentence index
#     if (result$sentence_id[[i]] != sentence_index) {
#       first_index <- i
#       sentence_index <- result$sentence_id[[i]]
#     }
#
#   }
#   sentence_sums[[sentence_index]] <- sum(result$sentiment_word[first_index:i], na.rm = TRUE)
#   sentence_puncts[[sentence_index]] <- min(result$punct_exclamation[i], 4) * 0.292 + min(result$punct_question[i] * 0.18, 0.96)
#
#
#   sentence_sums
#
#   sentence_scores <- dplyr::if_else(abs(sentence_sums) > 0, sentence_sums + sentence_puncts* (sentence_sums/abs(sentence_sums)), sentence_sums)
#   sentence_scores <- sentence_scores / sqrt((sentence_scores * sentence_scores) + 15)
#
#   return (sentence_scores)
# }
#
#
#
#
# #### BENCH
# bench::mark(tardis1 ( input_text))
# bench::mark(tardis1(input_text, use_rcpp = TRUE))
