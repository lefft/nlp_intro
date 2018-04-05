### 1. setup, load data, etc. -------------------------------------------------
lefftpack::lazy_setup()  # devtools::install_github("lefft/lefftpack")

# location to read from + write to
data_path <- function(...){ 
  file.path("/Users/timothyleffel/Google Drive/work NORC/", 
            "_nlp_pres/boosh/demos/word_embeddings/", "data", ...) 
}

# files we'll read and write (respectively)
files <- list(
  w2v_data_compressed = data_path("pruned.word2vec-words_as_cols.rda"),
  vecsamp_outfile = data_path("word2vec_words-as-cols_sample.csv"))

# loads `dat`, a subset of the 100d word2vec embeddings trained on english wiki
load(files$w2v_data_compressed)


# each column holds an embedding for a word (given by the colname)
# each row is a feature (rownames give feature indices)
message("\nfirst few elements of first few words: ")
print(dat[1:10, 1:5])
message("\ndata dimensions: ", list(dim(dat))) # (300 x 43981)




### 2. get 5000 most frequent english words (freqs from COCA) -----------------
samp_words <- lefftpack::dataset_word_freq %>% 
  arrange(Rank) %>% mutate(Word = tolower(Word)) %>% 
  filter(Word %in% colnames(dat)) %>% `[[`("Word")




### 3. carve out a sample of the column-wise word vectors ---------------------
dat_samp <- dat[, samp_words]
message("\ndimensions of w2v sample: ", list(dim(dat_samp))) # (300 x 4852)


### save the dataset for use in demo ------------------------------------------
message("\nwriting ~5k word2vec embeddings to file:\n  >> `", 
        gsub(data_path(), "data", files$vecsamp_outfile), "`")
write.csv(dat_samp, files$vecsamp_outfile, row.names=TRUE)


