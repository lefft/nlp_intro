# NOTE: REQUIRES FUNCTION `wv()` TO BE AVAILABLE (SEE `words_demo.r`)


### to refer to a word's vector -----------------------------------------------

# we will instantiate this with `wv()` in `words_demo.r`
wv_closure <- function(columnwise_word_vectors_matrix){
  function(word) as.numeric(columnwise_word_vectors_matrix[, word])
}




### to compute word similarities ----------------------------------------------

# cosine similarity: dot product over product of sums of squares 
cos_sim <- function(v1,v2) sum(v1*v2) / ((sqrt(sum(v1^2)))*(sqrt(sum(v2^2))))

# euclidean distance: sqrt of sum of squared pointwise diffs
euc_dist <- function(v1,v2) sqrt(sum((v1-v2)^2))

# useful if you want to try out different distance metrics 
# vec_distance <- function(w1, w2, f=cos_sim, ...) f(wv(w1), wv(w2), ...)




### to find similar words to a given word -------------------------------------

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
    w_dist <- cos_sim(wv(word), wv(w)) 
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
    w_dist <- cos_sim(vec, wv(w))
    if (w_dist > closest){
      closest <- w_dist
      closest_word <- w
      message("new closest: ", sprintf("%05f", w_dist), " (",closest_word,")")
    }
  }
  return(setNames(closest, closest_word))
}




### to visualize word embeddings ----------------------------------------------

# make a heatmap of a set of words' embeddings (try setting `interpolate=TRUE`)
vector_heatmap <- function(dat, words, splitvar=NULL, interpolate=FALSE){
  if (!sum(words %in% colnames(dat)) == length(words)){
    message("some words are not available, subsetting to available words...")
    words <- words[words %in% colnames(dat)]
  }
  
  plot_dat <- dat[, words] %>% 
    as_data_frame() %>% mutate(idx=seq_len(nrow(.))) %>% 
    reshape2::melt(id.vars="idx") %>% 
    mutate(variable = factor(variable, levels=rev(words))) %>% as_data_frame
  
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
tsne_vector_plot <- function(dat, words, splitvar, perplexity='auto',
                             seed=NULL, scale_colors=NULL){
  # TODO -- generalize coloring (max 4 colors as-is...)
  # TODO -- allow `splitvar` to be a boolean function on strings 
  # TODO -- smarter handling of `perplexity`
  
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



