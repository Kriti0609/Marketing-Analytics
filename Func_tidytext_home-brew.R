try(require(dplyr) || install.packages("dplyr"))
library(dplyr)

try(require(tidytext) || install.packages("tidytext"))
library(tidytext)

try(require(tidyr) || install.packages("tidyr"))
library(tidyr)
try(require(wordcloud) || install.packages("wordcloud"))
library(wordcloud)
try(require(ggplot2) || install.packages("ggplot2"))
library(ggplot2)

library(text2vec)

require(tibble)
require(stringr) 
require(Matrix)	

## Text input comes from readLines(file.choose())
## stopwords to be input from readline readLines(file.choose())
# Defining Function to clean corpus
clean_corpus<-function(text,user_stopwords){
  text=gsub("\\d+"," ",text)
  text=gsub("\\n"," ",text)
  text=gsub("\\%"," ",text)
  text=gsub("<.*?>"," ",text)
  text=gsub("\\s+|\\s+?"," ",text)
  std_stopwords<-c(stop_words$word)
  all_words<-append(std_stopwords,user_stopwords)
  words<-unique(all_words)
  stopword_df=data.frame(words)
  text_df=data_frame(text=text)
  text_df_token=text_df %>% unnest_tokens(words,text) 
  textdf_final= anti_join(text_df_token,stopword_df,by="words")
  
  return (textdf_final)
  
}
############################

Build_DTM<-function(dataframe){
  dataframe = dataframe %>% mutate(doc = seq(1:nrow(dataframe))) %>% group_by(doc)
  dataframe = dataframe %>% count(words, sort = FALSE) %>% rename(count = n)
  dtm<- dataframe %>% cast_dtm(doc, words, count)
  regular_dtm<-as.matrix(dtm)
  final_tfidf_matrix=bind_tf_idf(dataframe, words, doc, count)
  return (regular_dtm)
}

#############################
Build_WordCloud_Chart_COG<-function(dtm){
temp<-dtm
count = colSums(temp)
freq_mat=data.frame(count)
freq_mat <- freq_mat[order(freq_mat$count, decreasing = TRUE),,drop = FALSE]
freq_mat=rownames_to_column(freq_mat,var = "words")
wordcloud(freq_mat$words,freq_mat$count,max.words = 300)

#plot barchart for top tokens
bar_plot_frame = freq_mat[freq_mat[, "count"] >=30, ]
print(ggplot(bar_plot_frame, aes(x=words, y=count)) + geom_bar(stat="identity"))
}

#############################3
text.clean = function(x)                    # text data
{ require("tm")
  x  =  gsub("<.*?>", " ", x)               # regex for removing HTML tags
  x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
  x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric 
  x  =  tolower(x)                          # convert to lower case characters
  x  =  removeNumbers(x)                    # removing numbers
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space

# Read Stopwords list
stpw1 = readLines('https://raw.githubusercontent.com/sudhir-voleti/basic-text-analysis-shinyapp/master/data/stopwords.txt')# stopwords list
stpw2 = tm::stopwords('english')      # tm package stop word list; tokenizer package has the same name function, hence 'tm::'
comn  = unique(c(stpw1, stpw2))         # Union of two list
stopwords = unique(gsub("'"," ",comn))  # final stop word lsit after removing punctuation

  x  =  removeWords(x,stopwords)            # removing stopwords created above
  x  =  stripWhitespace(x)                  # removing white space
#  x  =  stemDocument(x)                   # can stem doc if needed.

  return(x)
}

===================================================================================

build_tcm <- function(x){   # x is cleaned corpus
require(text2vec)
tok_fun = word_tokenizer  # using word & not space tokenizers
it_0 = itoken( x,
                  #preprocessor = text.clean,
                  tokenizer = tok_fun,
                  ids = data$id,
                  progressbar = T)

vocab = create_vocabulary(it_0,    #  func collects unique terms & corresponding statistics
                          ngram = c(2L, 2L))

pruned_vocab = prune_vocabulary(vocab,  # filters input vocab & throws out v frequent & v infrequent terms
                                term_count_min = 1)

vectorizer = vocab_vectorizer(pruned_vocab) #  creates a text vectorizer func used in constructing a dtm/tcm/corpus

dtm_0  = create_dtm(it_0, vectorizer) # high-level function for creating a document-term matrix

# Sort bi-gram with decreasing order of freq
 tsum = as.matrix(t(slam::rollup(dtm_0, 1, na.rm=TRUE, FUN = sum))) # find sum of freq for each term
 tsum = tsum[order(tsum, decreasing = T),]       # terms in decreasing order of freq

#-------------------------------------------------------
# Code bi-grams as unigram in clean text corpus
#-------------------------------------------------------

text2 = x
text2 = paste("",text2,"")

pb <- txtProgressBar(min = 1, max = (length(tsum)), style = 3) ; 

i = 0
for (term in names(tsum)){
  i = i + 1
  focal.term = gsub("_", " ",term)        # in case dot was word-separator
  replacement.term = term
  text2 = gsub(paste("",focal.term,""),paste("",replacement.term,""), text2)
  setTxtProgressBar(pb, i)
 }


it_m = itoken(text2,     # function creates iterators over input objects to vocabularies, corpora, DTM & TCM matrices
              # preprocessor = text.clean,
              tokenizer = tok_fun,
              ids = data$id,
              progressbar = T)

vocab = create_vocabulary(it_m)     # vocab func collects unique terms and corresponding statistics
pruned_vocab = prune_vocabulary(vocab,
                                term_count_min = 1)


vectorizer = vocab_vectorizer(pruned_vocab,    # start with the pruned vocab
                              grow_dtm = FALSE,    # doesn;t play well in R due to memory & over-writing issues
                              skip_grams_window = 5L)   # window size = no. of terms to left & right of focal term

tcm = create_tcm(it_m, vectorizer) # create_tcm() func to build a TCM


return(tcm)  

    } # build_tcm func ends
###################################################################################################################3

distill.cog = function(mat1, # input TCM ADJ MAT
                       title, # title for the graph
                       s,    # no. of central nodes
                       k1){  # max no. of connections  
  library(igraph)
  a = colSums(mat1) # collect colsums into a vector obj a
  b = order(-a)     # nice syntax for ordering vector in decr order  
  
  mat2 = mat1[b, b]     # order both rows and columns along vector b
  
  diag(mat2) =  0
  
  ## +++ go row by row and find top k adjacencies +++ ##

  wc = NULL
  
  for (i1 in 1:s){ 
    thresh1 = mat2[i1,][order(-mat2[i1, ])[k1]]
    mat2[i1, mat2[i1,] < thresh1] = 0   # neat. didn't need 2 use () in the subset here.
    mat2[i1, mat2[i1,] > 0 ] = 1
    word = names(mat2[i1, mat2[i1,] > 0])
    mat2[(i1+1):nrow(mat2), match(word,colnames(mat2))] = 0
    wc = c(wc,word)
  } # i1 loop ends
  
  
  mat3 = mat2[match(wc, colnames(mat2)), match(wc, colnames(mat2))]
  ord = colnames(mat2)[which(!is.na(match(colnames(mat2), colnames(mat3))))]  # removed any NAs from the list
  mat4 = mat3[match(ord, colnames(mat3)), match(ord, colnames(mat3))]
  graph <- graph.adjacency(mat4, mode = "undirected", weighted=T)    # Create Network object
  graph = simplify(graph) 
  V(graph)$color[1:s] = "green"
  V(graph)$color[(s+1):length(V(graph))] = "pink"

  graph = delete.vertices(graph, V(graph)[ degree(graph) == 0 ]) # delete singletons?
  
  plot(graph, 
       layout = layout.kamada.kawai, 
       main = title)

  } # distill.cog func ends


