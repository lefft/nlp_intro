lefftpack::lazy_setup()
source("itsy_bitsy_functions.r")

# get a "corpus" as a single string 
txt <- readLines("itsy_bitsy.txt") %>% paste(collapse=" ") %>% 
  gsub("[^[:alpha:]] ", "", .) %>% tolower()

txt

# get the unique words
vocab <- txt %>% strsplit(split=" ") %>% `[[`(1) %>% unique()

# tokenize the corpus into trigrams 
trigrams <- tokenize_trigrams(txt)

# also get the "contexts", as a list 
trigram_contexts <- trigrams_to_contexts(trigrams)



### 1. context-term matrix ----------------------------------------------------
context_term_matrix <- sapply(vocab, function(w){
  word_count_context(w,txt)[[w]]
})

rownames(context_term_matrix) <- sapply(trigram_contexts, 
  function(tg) paste0(tg[1], "_", tg[3]))

context_term_matrix



### 2. term-term co-occurrence matrix (trigram windows) -----------------------

# create function to count term co-occurrences in a corpus 
cooc_count <- cooc_count_closure(trigrams)


# make an empty term-term matrix 
tt_cooc_matrix <- matrix(NA, nrow=length(vocab), ncol=length(vocab), 
                         dimnames=list(vocab, vocab))

# for each cell of the matrix, count trigram-cooccurrences of the row-col words
for (w1 in vocab){
  for (w2 in vocab){
    tt_cooc_matrix[w1, w2] <- cooc_count(w1, w2)
  }
}







### SCRATCH 
# # make an empty df holding all combos of words
# cooc <- as_data_frame(t(combn(vocab, m=2)))
# 
# # count co-occurrences for each combo of words (*ignore linear order*)
# cooc$cooc_count <- sapply(1:nrow(cooc), function(idx){
#   cooc_count(w1=cooc[idx,1], w2=cooc[idx,2])
# })
# 
# tt_cooc_matrix <- matrix(cooc$cooc_count, 
#                          nrow=length(cooc$V1), ncol=length(cooc$V2), byrow=TRUE,
#                          dimnames=list(cooc$V1, cooc$V2))
# # ,dimnames=list(cooc$V1, cooc$V2)) %>% View
# 
# rownames(mm) <- cooc$V1
# colnames(mm) <- cooc$V2


