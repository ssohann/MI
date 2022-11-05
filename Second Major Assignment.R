In this second major assignment to be graded, you will text-analyze tweets collected around a keyword, COVID-19 vaccines. For doing so, you are provided with a tweet data set, cv_tweets_eng-1.RData, which contains only English-written tweets with geo-location information. Analyzing the tweets, you are required to set the seed to reproduce the random number generator by using your ID, set.seed(####) and submit the following things.
  
  First, you create a graph showing the time-series trend of tweets posted over time (i.e., the number of tweets posted on each day).
  
  Second, you create a map showing the geo-location of positive and negative tweets in the United States, classified by lexicon-based sentiment analysis using the bing lexicon. Or you can create a graph tracing the rhythm of expressing overall sentiments around COVID-19 vaccines on Twitter in the U.S.
  
  Third, you identify and visualize the top 10 bigrams in terms of TF-IDF in the United States, India, United Kingdom, and Canada, and find any similarity or difference in the bigrams among the four countries.
  
  Fourth, you create a semantic network graph of word co-occurrences in the tweets from each of the four countries, and discuss some topics (word or hashtag clusters) identified by analyzing the four semantic networks in the tweets.
  
  Requirement 1: You should set the seed to reproduce the random number generator by using your ID. For instance, if your ID # is 20201234, then you set the seed by entering set.seed(20201234) before creating a semantic network of word occurrences in the tweet dataset.
  
  Requirement 2: The words to be counted should NOT contain stop words, punctuation marks, non-ASCII codes, HTML tags, and URLs.
  
  Requirement 3: Word co-occurrence is defined as an instance of any pair of words or hashtags used together in the same tweet.
  
  Requirement 4: By 11:59 PM on June 15th (Tuesday), you will upload the following things to the Assignments section on our e-class page.
  
  1. Your R codes used in the workflow of text processing, tokenization, analysis, and visualization (20 points).
  
  2. The graph showing the time-series trend of tweets posted over time and your written interpretation of the result (15 points).
  
  3. The graph showing either the geo-location map or the time-series trend of tweet sentiments in the U.S. and your written interpretation of the result (15 points).
  
  4. The tables to compare the most prominent bigrams in terms of TF-IDF among the tweets from the U.S., India, the U.K., and Canada, and your discussion on the similarity or difference in the bigrams among the four countries (15 points).
  
  5. Four semantic networks of word co-occurrences in the tweets from the four countries and your interpretation and discussion on some salient topics emerging from the networks (15 points).
  
  6. Your short written responses (less than 10 sentences) to the following questions (10 point each)
  
  6.1) What do you think the most prominent two or three advantages and disadvantages of lexicon-based sentiment analysis?
  
  6.2) What do you think any meaningful differences between analyzing bigrams and word co-occurrences? Which method do you find more helpful to understand public opinions about COVID-19 vaccines in the four countries?

#0. TOKENIZING
library(tidyverse)
library(tidytext)
library(textclean)
library(stopwords)
library(widyr)
library(igraph)
library(ggraph)
library(ggthemes)
library(tibble)
library(tidyr)
library(stringr)
library(textdata)
library(ggplot2)
library(lubridate)
load("cv_tweets_eng-1.RData")
set.seed(20182943)

covid_tweets %>%
  select(text) %>%
  View()

#1. time-series trend of tweets posted over time 

covid_tweets_days  <- cv_tweets_eng %>% 
  mutate(day = floor_date(created_at, unit="day")) %>% 
  count(day)


covid_tweets_days %>%
  ggplot(aes(x=day, y=n)) +
  geom_line() +
  theme_bw() + #blank&white
  labs(x = NULL, y = "Daily Sum",
       title = "Time Series Trend Of Tweets Over Time",
       subtitle = "Tweets were aggregated in day intervals.")

#2.  a graph tracing the rhythm of expressing overall sentiments around COVID-19 vaccines on Twitter in the U.S.

covid_tweets_tidy <- cv_tweets_eng %>% 
  filter(lang == "en") %>% 
  filter(country == "United States") %>%
  mutate(hour = floor_date(created_at, unit="hour")) %>% 
  mutate(text = sapply(text, replace_contraction)) %>% 
  mutate(text = str_replace_all(text, "[#@]?[^[:ascii:]]+", " ")) %>% 
  mutate(text = str_replace_all(text, "&amp;|&lt;|&gt;|&quot;|RT", " ")) %>% 
  unnest_tweets(word, text) %>% 
  filter(!word %in% stopwords()) %>%
  filter(str_detect(word, "[a-z]"))

bing_lexicon <- lexicon_bing()

covid_tweets_tidy %>% 
  inner_join(bing_lexicon) %>% 
  count(hour, sentiment)

names(covid_tweets_tidy) 

cv_text = covid_tweets_tidy %>%
  select(hour, word)

#check for words out
cv_text %>% 
  inner_join(bing_lexicon) %>% 
  #filter(!word %in% words_out) %>%
  count(sentiment, word, sort=TRUE) %>% 
  filter(sentiment == "positive")%>% 
  View()
  
words_out <- c("like","virus","trump","breakthrough")

cv_text %>% 
  inner_join(bing_lexicon) %>% 
  filter(!word %in% words_out) %>% 
  count(hour, sentiment) %>% 
  arrange(hour) %>% 
  ggplot(aes(x=hour, y=n, colour=sentiment)) +
  geom_line() +
  theme_bw() +
  labs(x = NULL, y = "Hourly Sum",
       title = "The rhythm of the sentiment on Twitter",
       subtitle = "The Bing Lexicon was used to measure sentiment in tweets")

#3. the top 10 bigrams in terms of TF-IDF (US, INDIA, UK, CANADA)
covid_bigrams <- cv_tweets_eng %>% 
  filter(lang=="en") %>% 
  filter(country %in% c("United States","India","United Kingdom","Canada")) %>% 
  mutate(text = str_replace_all(text, "RT", " ")) %>% 
  mutate(text = sapply(text, replace_non_ascii)) %>% 
  mutate(text = sapply(text, replace_contraction)) %>% 
  mutate(text = sapply(text, replace_html)) %>% 
  mutate(text = sapply(text, replace_url)) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  arrange(created_at)

bigrams_separated <- covid_bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_separated%>%
  filter(word1 == "breakthrough")%>%
  select(word1,word2)

bigram_tf_idf <- bigrams_separated %>% 
  filter(!word1 %in% stopwords()) %>% 
  filter(!word2 %in% stopwords()) %>% 
  filter(!str_detect(word2, "[^[:alpha:]]")) %>% 
  unite(bigram, word1, word2, sep = " ") %>%  
  count(country, bigram) %>%
  bind_tf_idf(bigram, country, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf %>% 
  arrange(desc(tf_idf)) %>% 
  group_by(country) %>% 
  top_n(10, tf_idf) %>% 
  ungroup %>% 
  mutate(bigram = reorder_within(bigram, tf_idf, country)) %>% 
  ggplot(aes(bigram, tf_idf, fill = country)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() + 
  labs(x = NULL, y = "TF-IDF") +
  facet_wrap(~country, ncol=2, scales="free") +
  coord_flip()

#4. a semantic network graph of word co-occurrences in the tweets from each of the four countries,
covid_tweets_words <- cv_tweets_eng %>% 
  filter(lang=="en") %>% 
  filter(country %in% c("United States","India","United Kingdom","Canada")) %>% 
  mutate(text = str_replace_all(text, "RT", " ")) %>% 
  mutate(text = sapply(text, replace_non_ascii)) %>% 
  mutate(text = sapply(text, replace_contraction)) %>% 
  mutate(text = sapply(text, replace_html)) %>% 
  mutate(text = sapply(text, replace_url)) %>% 
  unnest_tokens(word, text, token=stringr::str_split, pattern="[^#@[:word:]]") %>%
  filter(!word %in% stopwords()) %>% 
  filter(!str_detect(word, "[^[:word:]#@]")) %>% 
  dplyr::select(country, status_id, word)

covid_tweets_words %>% 
  count(word, sort=T)


word_cors <- covid_tweets_words %>% 
  group_by(word) %>% 
  filter(n() >= 10) %>% 
  pairwise_cor(word, status_id, sort = TRUE)

set.seed(20182943)

word_cors %>% 
  filter(correlation > .4) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) + 
  geom_node_point(color = "plum4", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) + 
  theme_void() 