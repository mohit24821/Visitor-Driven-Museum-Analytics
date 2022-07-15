#Sentiment Analysis on TripAdvisor data
#Setting up the working directory
setwd("/Users/mohit/Desktop/Trinity College/BA- Dissertation/AI enabled Museum Operations/Visitor Data")
getwd()

#loading the libraries
library(readxl)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidytext)
library(tidyverse)
library(stringr)
library(tidyr)
library(scales)
library(broom)
library(purrr)
library(widyr)
library(igraph)
library(ggraph)
library(SnowballC)
library(wordcloud)
library(reshape2)
theme_set(theme_minimal())
library(dplyr)
library(anytime)
library(plyr)
library(wordcloud)


#Downloaded all the data available on TripAdvisor
data <- read_excel("Reviews.xlsx")

head(data)
#Dimension of data
dim(data); 
#Total number of reviews available is 250 (10 reviews per page, it's about 10 pages of data); due to the limitation on scraping the data from the website for free

min(data$dateCreated); 
#oldest data is 29th April

max(data$dateCreated);
#latest date is 24th June

#_____________________________________________
df <- data
df <- df[complete.cases(df), ]
df$dateCreated <- anytime(df$dateCreated)
View(df)

df %>%
  count(Week = round_date(dateCreated, "week")) %>%
  ggplot(aes(Week, n)) +
  geom_line() + 
  ggtitle('The Number of Reviews Per Week')

#_____________________________________________

#Text mining of reviews data
View(df)
df <- tibble::rowid_to_column(df, "ID")
df <- df %>%
  mutate(dateCreated = as.POSIXct(dateCreated, origin = "2022-04-29"),month = round_date(dateCreated, "month"))

review_words <- df %>%
  distinct(reviewBody, .keep_all = TRUE) %>%
  unnest_tokens(word, reviewBody, drop = FALSE) %>%
  distinct(ID, word, .keep_all = TRUE) %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_detect(word, "[^\\d]")) %>%
  group_by(word) %>%
  mutate(word_total = n()) %>%
  ungroup()
word_counts <- review_words %>%
  count(word, sort = TRUE)


word_counts %>%
  head(25) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "lightblue") +
  scale_y_continuous(labels = comma_format()) +
  coord_flip() +
  labs(title = "Most common words in review text",
       subtitle = "Among 250 reviews; stop words removed",
       y = "# of uses")


#Bigram (2 words coming together)

review_bigrams <- df %>%
  unnest_tokens(bigram, reviewBody, token = "ngrams", n = 2)
bigrams_separated <- review_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united %>%
  count(bigram, sort = TRUE)

#We can visualize bigrams in word networks:

review_subject <- df %>% 
  unnest_tokens(word, reviewBody) %>% 
  anti_join(stop_words)
my_stopwords <- data_frame(word = c(as.character(1:10)))
review_subject <- review_subject %>% 
  anti_join(my_stopwords)
title_word_pairs <- review_subject %>% 
  pairwise_count(word, ID, sort = TRUE, upper = FALSE)
set.seed(1234)
title_word_pairs %>%
  filter(n >= 10) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  ggtitle('Word network in TripAdvisor reviews')
theme_void()
#The above visualizes the common bigrams in TripAdvisor reviews, showing those that occurred at least 10 times and where neither word was a stop-word.

#TriDiagrams

#Bigrams sometimes are not enough, let's see what are the most common trigrams in Hilton Hawaiian Village's TripAdvisor reviews?

review_trigrams <- df %>%
  unnest_tokens(trigram, reviewBody, token = "ngrams", n = 3)
trigrams_separated <- review_trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")
trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)
trigram_counts <- trigrams_filtered %>% 
  count(word1, word2, word3, sort = TRUE)
trigrams_united <- trigrams_filtered %>%
  unite(trigram, word1, word2, word3, sep = " ")
trigrams_united %>%
  count(trigram, sort = TRUE)

##Sentiment Analysis

reviews <- df %>% 
  filter(!is.na(reviewBody)) %>% 
  select(ID, reviewBody) %>% 
  group_by(row_number()) %>% 
  ungroup()
tidy_reviews <- reviews %>%
  unnest_tokens(word, reviewBody)
tidy_reviews <- tidy_reviews %>%
  anti_join(stop_words)
bing_word_counts <- tidy_reviews %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip() + 
  ggtitle('Words that contribute to positive and negative sentiment in the reviews')


##Let's try another sentiment library to see whether the results are the same.

contributions <- tidy_reviews %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(occurences = n(),
            contribution = sum(score))
contributions %>%
  top_n(25, abs(contribution)) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, contribution, fill = contribution > 0)) +
  ggtitle('Words with the greatest contributions to positive/negative 
          sentiment in reviews') +
  geom_col(show.legend = FALSE) +
  coord_flip()

set.seed(1234)
palet  = brewer.pal(8, 'Dark2')
wordcloud(df$reviewBody, min.freq = 20, scale = c(4, 0.2) , random.order = TRUE, col = palet)



