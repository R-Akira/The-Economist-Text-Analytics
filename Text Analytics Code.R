####### Libraries #######
library(pdftools) 
library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
library(scales)
library(wordcloud)
library(igraph)
library(ggraph)
library(reshape2)
library(textdata)

####### Importing Necessary External Data #######

# Stopwords
data(stop_words)

# Custom Stopwords

# The custom stop words are used to remove tokens with high frequency, 
# yet brings no insights
# These custom stop words can be improved further

junk_stop <- data_frame( word = c('ing','time','people', 'countries',
                         '0','1','2','3','4','5','6',
                         '7','8','9','10','15','20','30',
                         '40','50','2018','2019','2016','december',
                         'january','de','day','first','day','leader',
                         'government','money','run','party','find','nation',
                         'nations','home','risk','lot','left','ms',
                         'economist','china','american','america','world','firms',
                         'pro','al','con','called','7th','city','nov','set',
                         'recent','called','low','find','ten','win','local',
                         'data','power','business','first','market','trade',
                         'country','national','change','q3','political','foreign',
                         'president','south','2017','firms'),
                    lexicon = rep("junk",each=79))


# Sentiments Data

data(sentiments)

bing <- get_sentiments('bing')

################################## Part 1 ##################################
################################## Tokenizing ##################################


###### Importing all PDF files from December 2019 ###### 

# Setting working directory
setwd("/Users/user/Desktop/R/Mod B/A3/Data/2019 December")

# Listing all Files
files <- list.files(pattern = "pdf$")

# Making a List
data <-lapply(files,pdf_text)

# Converting to Matrix
data_mat <- do.call(cbind,data)


# Converting to Dataframe
a <- 105 #how many observations to you have
b <- 3   #how many variables do you have
my_df <- as.data.frame(matrix(nrow=a, ncol=b))

for(z in 1:b){
  for(i in 1:a){
    my_df[i,z]<- data_mat[i*b+z-b]
  }#closing z loop
}#closing i loop

# Converting each Variable
my_txt <- my_df$V1
my_txt <- substr(my_txt, start=1 , stop = 100000000)

one <- data_frame(line=1:a, text=my_txt)

my_txt <- my_df$V2
my_txt <- substr(my_txt, start=1 , stop = 100000000)

two <- data_frame(line=1:a, text=my_txt)

my_txt <- my_df$V3
my_txt <- substr(my_txt, start=1 , stop = 100000000)

three <- data_frame(line=1:a, text=my_txt)

# Combining into 1 DataFrame
dec2019 <- rbind(one,two,three)

dec2019_token <- dec2019 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  anti_join(junk_stop) 


###### Importing all PDF files from January 2019 ###### 

# Setting working directory
setwd("/Users/user/Desktop/R/Mod B/A3/Data/2019 January")

# Listing all Files
files <- list.files(pattern = "pdf$")

# Making a List
data <-lapply(files,pdf_text)

# Converting to Matrix
data_mat <- do.call(cbind,data)


# Converting to Dataframe
a <- 72 #how many observations to you have
b <- 4   #how many variables do you have
my_df <- as.data.frame(matrix(nrow=a, ncol=b))

for(z in 1:b){
  for(i in 1:a){
    my_df[i,z]<- data_mat[i*b+z-b]
  }#closing z loop
}#closing i loop

# Converting each Variable
my_txt <- my_df$V1
my_txt <- substr(my_txt, start=1 , stop = 100000000)

one <- data_frame(line=1:a, text=my_txt)

my_txt <- my_df$V2
my_txt <- substr(my_txt, start=1 , stop = 100000000)

two <- data_frame(line=1:a, text=my_txt)

my_txt <- my_df$V3
my_txt <- substr(my_txt, start=1 , stop = 100000000)

three <- data_frame(line=1:a, text=my_txt)

my_txt <- my_df$V4
my_txt <- substr(my_txt, start=1 , stop = 100000000)

four <- data_frame(line=1:a, text=my_txt)

# Combining into 1 DataFrame
jan2019 <- rbind(one,two,three,four)

jan2019_token <- jan2019 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  anti_join(junk_stop) 


###### Importing all PDF files from January 2020 ###### 

# Setting working directory
setwd("/Users/user/Desktop/R/Mod B/A3/Data/2020 January")

# Listing all Files
files <- list.files(pattern = "pdf$")

# Making a List
data <-lapply(files,pdf_text)

# Converting to Matrix
data_mat <- do.call(cbind,data)


# Converting to Dataframe
a <- 81 #how many observations to you have
b <- 4   #how many variables do you have
my_df <- as.data.frame(matrix(nrow=a, ncol=b))

for(z in 1:b){
  for(i in 1:a){
    my_df[i,z]<- data_mat[i*b+z-b]
  }#closing z loop
}#closing i loop

# Converting each Variable
my_txt <- my_df$V1
my_txt <- substr(my_txt, start=1 , stop = 100000000)

one <- data_frame(line=1:a, text=my_txt)

my_txt <- my_df$V2
my_txt <- substr(my_txt, start=1 , stop = 100000000)

two <- data_frame(line=1:a, text=my_txt)

my_txt <- my_df$V3
my_txt <- substr(my_txt, start=1 , stop = 100000000)

three <- data_frame(line=1:a, text=my_txt)

my_txt <- my_df$V4
my_txt <- substr(my_txt, start=1 , stop = 100000000)

four <- data_frame(line=1:a, text=my_txt)

# Combining into 1 DataFrame
jan2020 <- rbind(one,two,three,four)

jan2020_token <- jan2020 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  anti_join(junk_stop)


################################## Part 2 ##################################
################################## Correlogram ##################################

# The correlogram is used to find the topics discussed in different periods

economist <- bind_rows(mutate(jan2020_token, period="January 2020"),
                       mutate(dec2019_token, period="December 2019"),
                       mutate(jan2019_token, period="January 2019")
) %>%
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(period, word) %>%
  group_by(period) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(period, proportion) %>%
  gather(period, proportion, `January 2020`, `December 2019`)


# Plotting Correlogram

ggplot(data = economist, aes(x=proportion, y=`January 2019`, 
                             color = abs(`January 2019`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~period, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "January 2019", x=NULL)


######## Second Correlogram ########

economist2 <- bind_rows(mutate(jan2020_token, period="January 2020"),
                       mutate(jan2019_token, period="January 2019"),
                       mutate(dec2019_token, period="December 2019")
) %>%
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(period, word) %>%
  group_by(period) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(period, proportion) %>%
  gather(period, proportion, `January 2020`, `January 2019`)


# Plotting Correlogram
ggplot(data = economist2, aes(x=proportion, y=`December 2019`, 
                             color = abs(`December 2019`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~period, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "December 2019", x=NULL)

################################## Part 3 ##################################
################################## Word Cloud ##################################

# The wordcloud is used to support the iteration of the custom stopwords

# Token Count for Wordcloud

jan20token_count <- jan2020_token %>% count(word, sort=TRUE)
jan19token_count <- jan2019_token %>% count(word, sort=TRUE)
dec19token_count <- dec2019_token %>% count(word, sort=TRUE)

set.seed(1234)

# Wordclouds

wordcloud(words = jan20token_count$word, freq = jan20token_count$n, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = jan19token_count$word, freq = jan19token_count$n, min.freq = 1,
                      max.words=100, random.order=FALSE, rot.per=0.35, 
                      colors=brewer.pal(8, "Dark2"))

wordcloud(words = dec19token_count$word, freq = dec19token_count$n, min.freq = 1,
                      max.words=200, random.order=FALSE, rot.per=0.35, 
                      colors=brewer.pal(8, "Dark2"))


################################## Part 4 ##################################
################################## BiGrams ##################################

# Bigram is used to analyze the token context even further

# Dec 2019
dec2019_bigram <- dec2019 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% junk_stop$word) %>%
  filter(!word2 %in% junk_stop$word) %>%
  count(word1, word2, sort = TRUE)

# Setting Bigram Graph

dec2019bigram_graph <- dec2019_bigram %>%
  filter(n>20) %>%
  graph_from_data_frame()


# Plot Bigram

ggraph(dec2019bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)


# Jan 2019

jan2019_bigram <- jan2019 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% junk_stop$word) %>%
  filter(!word2 %in% junk_stop$word) %>%
  count(word1, word2, sort = TRUE)

# Setting Bigram Graph

jan2019bigram_graph <- jan2019_bigram %>%
  filter(n>20) %>%
  graph_from_data_frame()


# Plot Bigram

ggraph(jan2019bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)


# Jan 2020
jan2020_bigram <- jan2020 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% junk_stop$word) %>%
  filter(!word2 %in% junk_stop$word) %>%
  count(word1, word2, sort = TRUE)

# Setting Bigram Graph

jan2020bigram_graph <- jan2020_bigram %>%
  filter(n>20) %>%
  graph_from_data_frame()


# Plot Bigram

ggraph(jan2020bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)



################################## Part 5 ##################################
################################## QuadroGrams ##################################

# Quadrogram is used to analyze the token context even further

# Dec 2019 Quadrogram

# Building Quadrogram

dec2019_quadrogram <- dec2019 %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) %>%
  filter(!word1 %in% junk_stop$word) %>%
  filter(!word2 %in% junk_stop$word) %>%
  filter(!word3 %in% junk_stop$word) %>%
  filter(!word4 %in% junk_stop$word) %>%
  count(word1, word2, word3, word4, sort = TRUE)

# Setting Quadrogram

dec2019quad_graph <- dec2019_quadrogram %>%
  filter(n>2) %>%
  graph_from_data_frame()

# Plot Quadrogram

ggraph(dec2019quad_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)


# Jan 2019 Quadrogram

# Building Quadrogram

jan2019_quadrogram <- jan2019 %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) %>%
  filter(!word1 %in% junk_stop$word) %>%
  filter(!word2 %in% junk_stop$word) %>%
  filter(!word3 %in% junk_stop$word) %>%
  filter(!word4 %in% junk_stop$word) %>%
  count(word1, word2, word3, word4, sort = TRUE)

# Setting Quadrogram

jan2019quad_graph <- jan2019_quadrogram %>%
  filter(n>2) %>%
  graph_from_data_frame()

# Plot Quadrogram

ggraph(jan2019quad_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

# Jan 2020 Quadrogram


# Building Quadrogram

jan2020_quadrogram <- jan2020 %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) %>%
  filter(!word1 %in% junk_stop$word) %>%
  filter(!word2 %in% junk_stop$word) %>%
  filter(!word3 %in% junk_stop$word) %>%
  filter(!word4 %in% junk_stop$word) %>%
  count(word1, word2, word3, word4, sort = TRUE)

# Setting Quadrogram

jan2020quad_graph <- jan2020_quadrogram %>%
  filter(n>3) %>%
  graph_from_data_frame()

# Plot Quadrogram

ggraph(jan2020quad_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)


################################## Part 6 ##################################
################################## Sentiment Wordcloud ##################################

# Sentiment Wordcloud is used to know what kind of "bad" or "good" news happened in a period

# Dec 2019

dec2019_token %>% inner_join(bing) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("blue", "red"),
                   max.words=100)


# Jan 2019

jan2019_token %>% inner_join(bing) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("blue", "red"),
                   max.words=100)

# Jan 2020

jan2020_token %>% inner_join(bing) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("blue", "red"),
                   max.words=100)


