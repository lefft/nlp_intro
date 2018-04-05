

tokenize_trigrams <- function(txt){
  txt_words <- strsplit(txt, split = " ")[[1]]
  n_trigrams <- length(txt_words) - 2
  dplyr::data_frame(idx = seq_len(n_trigrams),
                    w1 = txt_words[1:(n_trigrams)],
                    w2 = txt_words[2:(n_trigrams+1)],
                    w3 = txt_words[3:(n_trigrams+2)])
}


word_count_context <- function(word, txt){
  trigrams <- tokenize_trigrams(txt)
  out <- mutate(trigrams, w2="")
  out[[word]] <- NA
  for (idx in seq_len(nrow(out))){
    tg <- c(out$w1[idx], "", out$w3[idx])
    tg[2] <- word
    out[[word]][idx] <- sum(
      trigrams$w1==tg[1] & trigrams$w2==tg[2] & trigrams$w3==tg[3])
  }
  return(out)
}




# use closure for faster iteration thru the trigrams 
cooc_count_closure <- function(trigrams){
  function(w1, w2){
    coocs <- 0
    for (idx in 1:nrow(trigrams)){
      if (w1 %in% trigrams[idx, ] & w2 %in% trigrams[idx, ])
        coocs <- coocs+1
    }
    return(coocs)
  }
}


trigrams_to_contexts <- function(trigrams){
  trigram_matrix <- as.matrix(trigrams[, c("w1","w2","w3")], ncol=3)
  contexts <- lapply(1:nrow(trigrams), function(row_idx){
    c(trigram_matrix[row_idx, "w1"], "", trigram_matrix[row_idx, "w3"])
  })
  return(contexts)
}



# 
# 
# # NO NEED(?)
# 
# # get the contexts 
# get_contexts <- function(tokens, window_size=3){
#   contexts <- vector("list", length=length(tokens)-window_size-1)
#   for (x in 1:(length(tokens)-window_size-1)){
#     l_idx <- x
#     r_idx <- window_size + l_idx - 1
#     window <- tokens[l_idx:r_idx]
#     window[median(seq_along(window))] <- ""
#     contexts[[x]] <- window
#   }
#   return(contexts)
# }
