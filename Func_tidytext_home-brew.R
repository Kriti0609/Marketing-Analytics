try(require(dplyr) || install.packages("dplyr"))
library(dplyr)

require(tidytext) || install.packages("tidytext")
library(tidytext)

try(require(tidyr) || install.packages("tidyr"))
library(tidyr)
try(require(wordcloud) || install.packages("wordcloud"))
library(wordcloud)
try(require(ggplot2) || install.packages("ggplot2"))
library(ggplot2)

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

