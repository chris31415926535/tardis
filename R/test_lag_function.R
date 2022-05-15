#
#
# test_lag <- function(sentence_id, vec_to_lag) {
#   sentence_id <- c(1,1,1,2,3,3,3,4,4)
#   vec_to_lag  <- c(1,0,0,0,0,1,0,1,0)
#
#   vec_length <- length(sentence_id)
#   vec_lagged1 <- rep(0, vec_length)
#   vec_lagged2 <- rep(0, vec_length)
#   vec_lagged3 <- rep(0, vec_length)
#
#   last_id <- 0
#   num_lags <- 0
#
#   for (i in 1:vec_length){
#     #message(i)
#     # new sentence id
#     if ((sentence_id[[i]] != last_id)  ) {
#       last_id <- sentence_id[[i]]
#       vec_lagged1[[i]] <- 0
#       num_lags <- 1
#     } else if ((sentence_id[[i]] == last_id) & (num_lags == 1)){
#       # first lag
#       num_lags <- num_lags + 1
#       vec_lagged1[[i]] <- vec_to_lag[[i-1]]
#     } else if ((sentence_id[[i]] == last_id) & (num_lags == 2)){
#       num_lags <- num_lags + 1
#       vec_lagged1[[i]] <- vec_to_lag[[i-1]]
#       vec_lagged2[[i]] <- vec_to_lag[[i-2]]
#     } else if ((sentence_id[[i]] == last_id) & (num_lags >= 3)){
#       num_lags <- num_lags + 1
#       vec_lagged1[[i]] <- vec_to_lag[[i-1]]
#       vec_lagged1[[2]] <- vec_to_lag[[i-2]]
#       vec_lagged1[[3]] <- vec_to_lag[[i-3]]
#
#
#
#   }
#
#
#   }
#
#   name1 <- "modifier1"
#   name2 <- "modifier2"
#   name3 <- "modifier3"
#   dplyr::tibble(sentence_id, vec_to_lag, vec_lagged1, vec_lagged2, vec_lagged3) %>%
#     dplyr::rename({{name1}} := vec_lagged1,
#                   {{name2}} := vec_lagged2 ,
#                   {{name3}} := vec_lagged3 )
# }
#
#
# # custom function to lag a vector based on the values in an index vector
# # essentially a custom version of dplyr::group() %>% dplyr::lag() but MUCH faster.
# # vector_index is a vector of monotonically increasing integers indicating groups
# # vec_to_lag is the vector to be lagged
# # NOTE this seems 50x faster than the dplyr functions, which were the bottleneck
# # this could be easily converted to Rcpp if necessary / worth it
# lag1_indexed_vector <- function(vector_index, vec_to_lag) {
#
#   vec_length <- length(vector_index)
#   vec_lagged1 <- rep(0, vec_length)
#   last_id <- 0
#
#   for (i in 1:vec_length){
#     #message(i)
#     # new sentence id
#     if ((vector_index[[i]] != last_id)  ) {
#       last_id <- vector_index[[i]]
#       vec_lagged1[[i]] <- 0
#     } else {
#       vec_lagged1[[i]] <- vec_to_lag[[i-1]]
#     }
#
#   }
#
#   return (vec_lagged1)
# }
#
# test_lag()
#
# bench::mark(test_lag())
#
# lag1_indexed_vector(vector_index = c(1,1,1,2,3,3,3,4,4),
#                     vec_to_lag =   c(1,0,1,1,0,1,0,1,0))
#
# lag1_indexed_vector(result$sentence_id, result$negation)
#
# bench::mark(test_lag())
# bench::mark(lag1_indexed_vector(result$sentence_id, result$negation))
