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


#############################################################################################################

sentiment_analysis<-function(String,emo){
textdf = data_frame(text = String)
textdf_line = textdf%>%mutate(linenumber=row_number())
senti.nrc = textdf_line%>%
ungroup() %>%
unnest_tokens(word,text) %>%
inner_join(get_sentiments("nrc")) %>% 
count(sentiment,index=linenumber %/% 1, sort = FALSE) %>%
mutate(method="nrc")

df_emo1=data.frame()
df_emo2=data.frame()
df_emo3=data.frame()

# Create dataframe for top 3 documents with emotion 1:
senti.nrc %>% filter(sentiment == emo[1])
ans1=arrange(senti.nrc %>% filter(sentiment == emo[1]),desc(n) )   
ans2= ans1 %>% slice(1:3)
for(i in 1:3){
ind = ans2$index[i]
df_emo1=rbind((textdf_line%>%filter(textdf_line$linenumber==ind)),df_emo1)
	}
df_emo1=cbind(df_emo1,emotions=emo[1])


# Create dataframe for top 3 documents with emotion 2:
senti.nrc %>% filter(sentiment == emo[2])
ans1=arrange(senti.nrc %>% filter(sentiment == emo[2]),desc(n) )   
ans2= ans1 %>% slice(1:3)
for(i in 1:3){
ind = ans2$index[i]
df_emo2=rbind((textdf_line%>%filter(textdf_line$linenumber==ind)),df_emo2)
}
df_emo2=cbind(df_emo2,emotions=emo[2])



# Create dataframe for top 3 documents with emotion 3:
senti.nrc %>% filter(sentiment == emo[3])
ans1=arrange(senti.nrc %>% filter(sentiment == emo[3]),desc(n) )   
ans2= ans1 %>% slice(1:3)
for(i in 1:3){
ind = ans2$index[i]
df_emo3=rbind((textdf_line%>%filter(textdf_line$linenumber==ind)),df_emo3)
}
df_emo3=cbind(df_emo3,emotions= emo[3])

df_emo=c(df_emo1, df_emo2, df_emo3)
return (df_emo)
	
}


