## introduction to natural language processing 
###### materials for march/2018 monthly DS meeting 




### pres outline first pass 

1. housekeeping -- you can dl/clone the repo, follow along in nb/whatev it ends up being (5min)
2. motivating example -- smthg cool but simple (5min)
3. high-level map of field (5min)
4. concrete examples of NLP-related stuff happening at NORC (5min)
5. tooling considerations (10min)
6. demos of some cool stuff (30min)





### notes on each section of pres 

##### 1. housekeeping (5min)
- need to make data/code available before pres
- send out email w info day before if possible 
- keep it lite on the dependencies 

##### 2. *insert hook for motivating example* (5min)
##### 3. hi-level map of field (5min)
##### 4. nlp at norc (5min)
##### 5. tooling for nlp (10min)
##### 6. some fun demos (30min)








### notes to self, draft outlines, etc. 

##### 3. hi-level map of field[^1] [^2] 
<hr>

[^1]: **NB**: here we focus only on the analysis of *text*, which (from a linguist's perspective) is a crude approximation of language qua cognitive system. this means we won't go into the (very cool!) field of speech processing or any of its main tasks (performance on which has been rapidly improving in the last couple of years!). 

[^2]: **NB**: note that lots of NLP tasks are internally complex and require some NLU subtasks and other NLG subtasks (e.g. machine translation and document summarization)

##### Natural language generation (NLG)

*informal definition*: using computers to produce utterances or prose that is intended to be heard or read by humans (check out [narrative science](https://narrativescience.com/) for some cool applications)

I haven't encountered any clear use-cases for NLG in my work at NORC, although I wouldn't be surprised to hear that some exist. 

* examples of nlg systems in action: chatbots, alexa/siri, document and event summarization (e.g. AI-written sports summaries and obits in some newspapers -- many more on the horizon i suspect)

##### Natural Language Understanding (NLU)

*informal definition*: using computers to extract high-level (human-graspable) information from bodies of text -- the information can be highly structured and concrete (e.g. a dataset of word-frequency ratios), or highly abstract (e.g. these authors talk about pet ownership a lot). NLU is often what people mean when they say "NLP." 

* examples of NLU tasks: 
    - information retrieval 
    - document clustering (e.g. topic modeling)
    - sentiment analysis 
    - ... (many more)
* some sub-domains/fields of NLU:
    - lexical analysis -- stuff at the word and sub-word level, e.g. tokenization, chunking, stemming, lemmatization
    - parsing -- e.g. pos tagging, grammar construction, dependency detection, many kinds of structure building 
    - interpretation -- 
        - lexical semantics: meaning at the word level (e.g. word-sense disambiguation); 
        - compositional semantics: meaning at the phrase/sentence level (e.g. for dialogue systems)




###### stuff to demo 

- basic data structures (dtm, tcm, etc. )
- sentiment analysis 
- LDA topic modeling(?)
- 



### current status of proj folder (mar20/2018, yikes...)


```
rstudio@ip-172-31-18-238:~/BOOSH/nlp_intro$ tree
.
├── __outline
├── readme.html
├── readme.md
└── wv
    ├── app_draft
    │   └── vex-dev.r
    ├── dev
    │   ├── _demos
    │   ├── funcs_compute.r
    │   ├── funcs_train.r
    │   ├── _sample_datasets
    │   │   ├── data_readme.md
    │   │   ├── dataset_info.csv
    │   │   ├── samp_1k.csv
    │   │   ├── word2vec
    │   │   │   ├── pruned.word2vec.txt
    │   │   │   ├── pruned.word2vec.txt.zip
    │   │   │   ├── pruned.word2vec-words_as_cols.csv
    │   │   │   └── pruned.word2vec-words_as_cols.rda
    │   │   ├── word2vec_pair-dists_sample.csv
    │   │   └── word2vec_words-as-cols_sample.csv
    │   ├── _slides
    │   ├── train_explore_vecs.r
    │   ├── util.r
    │   ├── word_vex.html
    │   └── word_vex.rmd
    └── needs_migrated
        ├── 2018-02-28_hmc-demo-pres
        │   ├── itsy_bitsy_functions.r
        │   ├── itsy_bitsy.r
        │   ├── itsy_bitsy.txt
        │   ├── pres.html
        │   ├── pres.rmd
        │   └── wv-notes.md
        ├── pairwise_similarity.r
        ├── prep_glove-ascii_only.r
        ├── prep_glove.py
        ├── prep_glove.r
        ├── scraps.rmd
        ├── system_funcs.r
        ├── tweeter_vexxxe.r
        ├── util.r
        ├── word2vec_fiddle.r
        └── word2vec_prep.r

9 directories, 35 files
```



### embeddings for 100 common words as sample data
```r
### setup, load data, etc. ----------------------------------------------------
suppressPackageStartupMessages(library(dplyr))

data_path <- function(...) file.path(
  "/home/rstudio/BOOSH/nlp_intro/wv-stuff_to_integrate/datasets", ...)
  
files <- list(
  vecsamp=data_path("sample_data", "word2vec_words-as-cols_sample.csv"),
  pairdistsamp=data_path("sample_data", "word2vec_pair-dists_sample.csv"),
  w2v_data_compressed=data_path("word2vec","pruned.word2vec-words_as_cols.rda"))

load(files$w2v_data_compressed)



### define distance/similarity measures ---------------------------------------
# euclidean distance: sqrt of sum of squared pointwise diffs
euc_dist <- function(v1,v2) sqrt(sum((v1-v2)^2))

# cosine similarity: dot product over product of sums of squares 
cos_sim <- function(v1,v2) sum(v1*v2) / ((sqrt(sum(v1^2)))*(sqrt(sum(v2^2))))



### snag 100 words we'll use for demos ----------------------------------------
# TODO -- ELIMINATE DEPENDENCY, JUST READ FROM URL 
samp_words <- as_data_frame(lefftpack::dataset_word_freq) %>% arrange(Rank) %>%
  `[`(1:106, "Word") %>% filter(Word %in% colnames(dat)) %>% `[[`("Word")

dat_samp <- dat[, samp_words]



### for those 100 words, get all the pairwise dists/sims ----------------------
# create a grid of all pairs of elements from `samp_words` (to fill it up)
vec_dists <- as_data_frame(t(combn(samp_words, m=2, simplify=TRUE)))

# NB: the bigger `samp_words` is, the longer this will take
# (clocked against for-loop and sequence of sapply, this approach wins)
ctr <- 0
message("starting at ", Sys.time(), "...")

# compute both metrics for each pair, while looping over rows only once 
vec_dists[, c("dist","sim")] <- t(sapply(1:nrow(vec_dists), function(idx){
  ctr <<- ctr+1
  if (ctr %% 1e3 == 0) message("on row ", ctr, " of ", nrow(vec_dists), "...")
  c(euc_dist(dat_samp[, vec_dists$V1[idx]], dat_samp[, vec_dists$V2[idx]]), 
    cos_sim(dat_samp[, vec_dists$V1[idx]], dat_samp[, vec_dists$V2[idx]]))
}))

message("done at ", Sys.time(), " <333")

# note: distance and similarity are basically perfectly negatively correlated 
plot(vec_dists$sim, vec_dists$dist, main=paste0(
  "correlation btwn cos sim and euc dist: ", 
  round(cor(vec_dists$sim, vec_dists$dist), 3)))



### save each one as a csv, to be included in repo ----------------------------
write.csv(dat_samp, files$vecsamp, row.names=TRUE)
write.csv(vec_dists, files$pairdistsamp, row.names=FALSE)
```






## notes, links, etc. 


printed some paypes mar27, also peep these: 
  - http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.80.3862&rep=rep1&type=pdf
  - http://papers.nips.cc/paper/5021-distributed-representations-of-words-and-phrases-and-their-compositionality.pdf
  - http://www.aclweb.org/anthology/N13-1090
  - http://www.aclweb.org/anthology/P11-1015
  - http://www.aclweb.org/anthology/D14-1162
  - https://arxiv.org/pdf/1411.4166.pdf


1. 'snaut' -- similar to what i have been trying to build:
  - [source](https://github.com/pmandera/semspaces)
  - [homepage](http://meshugga.ugent.be/snaut/)
  - [app](http://meshugga.ugent.be/snaut-english/)
  - [associated paper](http://crr.ugent.be/papers/Mandera_et_al_JML_2016.pdf) (published attached)



##### a motivating example {.flexbox .vcenter}

ugh the flexbox doesnt seem to work...

nor do the fonts methinks...

css customization kinda a mess

[css quick reference](https://www.w3schools.com/cssref/css_selectors.asp)

