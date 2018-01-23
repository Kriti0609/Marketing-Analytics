try(require(dplyr) || install.packages("dplyr"))
library(dplyr)

require(tidytext) || install.packages("tidytext")
library(tidytext)

try(require(tidyr) || install.packages("tidyr"))
library(tidyr)

require(tibble)
require(stringr)
require(text2vec)   
require(Matrix)	
## Text input comes from readLines(file.choose())
## stopwords to be input from readline readLines(file.choose())
#++++++++++++ Defining Function to clean corpus
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
  textdf_doc = text_df %>% mutate(doc = seq(1:nrow(text_df))) %>% group_by(doc)
  textdf_doc1=textdf_doc %>% unnest_tokens(words,text) %>% count(words, sort = FALSE) %>% rename(count = n)
  textdf_final= anti_join(textdf_doc1,stopword_df,by="words")
  
  return (textdf_final)
  
}
##########
### how many words in each document?
textdf_word = textdf_doc %>% 
              unnest_tokens(word, text) %>% 
              mutate(word1 = 1) %>% 
              select(doc, word1) %>%
                  group_by(doc, word1) %>% 
              summarise(words_doc = sum(word1)) %>% 
              select(doc, words_doc)



###############
####++++++++++++++++++++++++++

Build_DTM<-function(dataframe){
  dtm<- dataframe %>% cast_dtm(doc, words, count)
  regular_matrix<-dataframe %>% cast_sparse(doc, words, count)
  final_tfidf_matrix=bind_tf_idf(dataframe, words, doc, count)
  return (final_tfidf_matrix)
}

####++++++++++++++++++++++++++++++


