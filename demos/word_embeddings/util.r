



### MEASURING DISTANCES BETWEEN VECTORS ---------------------------------------

# refer to a word's vector with a string ("key")
wv <- function(word) as.numeric(dat[,word])

# A,B length n: sum from 1 to n of Ai*Bi over prod of root sum of squares 
euc_dist <- function(v1, v2) sum(v1*v2) / ((sqrt(sum(v1^2)))*(sqrt(sum(v2^2))))

# get distance between vecs by passing just the word keys 
vec_distance <- function(w1, w2, f=euc_dist, ...) f(wv(w1), wv(w2), ...)


# find closest vector to a word (can find closest in sample for quicker dev)
closest_word <- function(word, samples=NULL, exclude=NULL){
  vec <- wv(word)
  if (is.null(samples)){
    comp_words <- colnames(dat) 
  } else {
    comp_words <- sample(colnames(dat), size=samples)
  }
  comp_words <- comp_words[comp_words!=word]
  if (!is.null(exclude)){
    comp_words <- comp_words[!comp_words %in% exclude]
  }
  closest <- 0
  closest_word <- ""
  for (w in comp_words){
    w_dist <- vec_distance(word, w)
    if (w_dist > closest){
      closest <- w_dist
      closest_word <- w
      message("new closest: ", sprintf("%05f", w_dist), " (",closest_word,")")
    }
  }
  return(setNames(closest, closest_word))
}



# find closest word to a vector (for e.g. passing w1+w2)
closest_word_vec <- function(vec, samples=NULL, exclude=NULL){
  if (is.null(samples)){
    comp_words <- colnames(dat) 
  } else {
    comp_words <- sample(colnames(dat), size=samples)
  }
  if (!is.null(exclude)){
    comp_words <- comp_words[!comp_words %in% exclude]
  }
  closest <- 0
  closest_word <- ""
  for (w in comp_words){
    w_dist <- euc_dist(vec, wv(w))
    if (w_dist > closest){
      closest <- w_dist
      closest_word <- w
      message("new closest: ", sprintf("%05f", w_dist), " (",closest_word,")")
    }
  }
  return(setNames(closest, closest_word))
}

# find closest n words to a word 
closest_n_words <- function(word, n, vec_grid){
  vec_grid <- vec_grid[vec_grid$V1==word | vec_grid$V2==word, ]
  vec_grid <- vec_grid[order(vec_grid$distance, decreasing=TRUE), ]
  vec_grid$word <- word
  vec_grid$close_word <- ifelse(vec_grid$V1==word, vec_grid$V2, vec_grid$V1)
  # TODO -- GET RID OF DUPES WHEN CREATING THE GRID!!! 
  unique(vec_grid[1:min(n, nrow(vec_grid)), c("word","close_word","distance")])
}



### UTILITIES FOR VISUALIZING PRE-TRAINED `word2vec` VECTORS ------------------

# NOTE: scaling column-wise seems to do nothing (i.e. `... apply(2, scale)`)
vector_heatmap <- function(dat, words, splitvar=NULL, interpolate=FALSE){
  if (!sum(words %in% colnames(dat)) == length(words)){
    message("some words are not available, subsetting to available words...")
    words <- words[words %in% colnames(dat)]
  }
  
  plot_dat <- dat[, words] %>% 
    as_data_frame() %>% mutate(idx=seq_len(nrow(.))) %>% 
    reshape2::melt(id.vars="idx") %>% 
    mutate(variable = factor(variable, levels=rev(words))) %>% as_data_frame
    # mutate(variable = as.character(variable)) 
  
  
  heatmap_helper <- function(data){
    data %>% ggplot(aes_string(x="idx", y="variable", fill="value")) + 
      geom_raster(show.legend=FALSE, alpha=.85, interpolate=interpolate) +
      # or: geom_tile(); and: scale_fill_continuous(low="white",high="black")
      scale_x_discrete(expand=c(0,0)) + 
      labs(x="", y="") + theme(panel.grid=element_blank())
  }
  
  if (is.null(splitvar)){
    return(heatmap_helper(plot_dat))
  }
  
  if (!is.null(splitvar)){
    message("faceting of heatmaps not yet supported!")
    return(null)
    # only want two levels of splitvar 
    stopifnot(length(unique(splitvar)) == 2)
    
    # mutate(variable = factor(variable, levels=rev(words))))
    # variable=factor(words, levels=rev(words))
    split_df <- data_frame(variable=words, splitvar=splitvar)
    
    plot_dat <- left_join(plot_dat, split_df, by="variable")
    
    out1 <- plot_dat %>% 
      filter(splitvar==unique(splitvar)[1]) %>% 
      plot_helper
    
    out2 <- plot_dat %>% 
      filter(splitvar==unique(splitvar)[2]) %>% 
      plot_helper
    
    gridExtra::grid.arrange(out1, out2, ncol=2, nrow=1)
  }
}






# reduce vectors to 2d-space, plot them with color encoded by `splitvar`
# TODO -- generalize coloring (max 4 colors as-is...)
# TODO -- allow `splitvar` to be a boolean function on strings 
# TODO -- smarter handling of `perplexity`
tsne_vector_plot <- function(dat, words, splitvar, perplexity='auto',
                             seed=NULL, scale_colors=NULL){
  # NOTE: need to be careful about perplexity
  # if (nrow(X) - 1 < 3 * perplexity) { stop("Perplexity is too large.")}
  if (!sum(words %in% colnames(dat)) == length(words)){
    message("some words are not available, subsetting to available words...")
    words <- words[words %in% colnames(dat)]
  }
  
  tsne_data <- t(dat[, words])
  
  if (perplexity=='auto'){
    perplexity <- (nrow(tsne_data)-1) / 3
  }
  
  if (!is.null(seed)) set.seed(seed) # for tsne rng 
  
  tsne_data <- Rtsne::Rtsne(tsne_data, dims=2, perplexity=perplexity)$Y
  
  tsne_df <- tsne_data %>% as_data_frame %>% 
    rename(x=V1, y=V2) %>% mutate(word = words) %>% mutate(splitvar = splitvar)
  
  if (is.null(scale_colors))
    scale_colors <- c("green4","steelblue","orange","black")
  
  # requires `ggrepel::` package to be attached! 
  tsne_df %>% 
    ggplot(aes(x=x, y=y, label=word, color=splitvar)) + 
    scale_color_manual(values=scale_colors) + 
    geom_point(show.legend=FALSE) + 
    geom_label_repel(show.legend=FALSE, min.segment.length=0, box.padding=.5) +
    coord_cartesian() + # this can be useful sometimes... coord_equal(ratio=1)
    labs(x="", y="")
}




### UTILITIES FOR QUERYING LARGE DATA FRAMES WITH VECS OR DISTANCES -----------

word_check_closure <- function(vecs_df) function(w) w %in% colnames(vecs_df) 
pair_check_closure <- function(dists_df){ function(w1, w2){ 
  sum(dists_df$V1==w1 & dists_df$V2==w2) > 0 | 
    sum(dists_df$V2==w1 & dists_df$V1==w2) > 0
}}


### UTILITIES FOR MANIPULATING PRE-TRAINED `word2vec` VECTORS -----------------

# get a file's size, in mb
file_size <- function(fname, mb_char=TRUE){ 
  n_bytes <- file.info(fname)$size
  if (mb_char) return(paste0(round(n_bytes/1e6, 1), "mb")) else return(n_bytes)
}


# flip `fname` vecs s.t. words are cols, index is rowname, all vals numeric 
flip_vecs <- function(fname, write_to){
  
  message("\n(beware -- `flip_vecs()` only set up for word2vec format...)")
  
  in_dims <- as.numeric(strsplit(readLines(fname, n=1), split=" ")[[1]])
  
  message("\nreading ", file_size(fname), " ", paste(in_dims, collapse="x"), 
          " table, to be flipped:\n  >> `", fname, "`")
  
  word_vecs <- read.table(fname, 
                          nrows=in_dims[1], header=FALSE, skip=1, row.names=1, 
                          colClasses=c("character", rep("numeric", in_dims[2])),
                          col.names=c("word", sprintf("v%03d", 1:in_dims[2])))
  
  word_vecs_flipped <- t(word_vecs)
  rownames(word_vecs_flipped) <- NULL
  out_dims <- dim(word_vecs_flipped)
  
  write.csv(word_vecs_flipped, write_to, row.names=FALSE)
  
  message("\nwrote flipped ", paste(out_dims, collapse="x"), ", ", 
          file_size(write_to), " table to:\n  >> `", write_to, "`")
}






# efficiently read a flipped (words as cols) vectors file as a matrix 
read_vectors_table <- function(fname, vector_length=300){
  
  message("\nreading input file (size: ", file_size(fname), ")...\n", 
          "assuming column-vectors with word-names in first row...")
  
  words <- scan(file=fname, 
                what=character(), sep=",", quote="\"", nlines=1)
  
  vectors <- scan(file=fname, 
                  what=double(), sep=",", skip=1)
  
  feature_names <- sprintf("f%03d", 1:vector_length)
  
  word_vectors_matrix <- matrix(vectors, ncol=length(words), byrow=TRUE, 
                                dimnames=list(feature_names, words))
  
  message("returning ", paste(dim(word_vectors_matrix), collapse="x")," matrix")
  
  return(word_vectors_matrix)
}


