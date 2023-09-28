# Script name:      comparative_textAnalysis_2_latinx_printedPress.R
# Created on:       September_23_2023
# Author:           Dr. Martin Calvino
# Purpose:          Compare texts, sentiments, bigrams and correlated words among news articles mentioning the term "latinx" across 6 newspapers
# Version:          v1.09.23.2023

# Corpus:               text extracted from 6 newspapers containing the term latinx
# The New York Times:   103 news articles up to April-22-2022
# Houston Chronicle:    101 news articles up to May-04-2022
# Los Angeles Times:    101 news articles up to May-06-2022
# Miami Herald:         78 news articles up to May-13-2022
# The Washington Post:  20 news articles up to May-17-2022
# Chicago Tribune:      31 news articles up to May-20-2022


# load libraries
library(tidyverse)
library(tidytext)
library(MetBrewer)
library(scales)
library(igraph)
library(ggraph)


# write function to show directory path for each newspaper .txt document
show_files <- function(directory_path, pattern = "\\.txt$") {
  file_name_v <- dir(directory_path, pattern, full.names = TRUE)
  for (i in seq_along(file_name_v)) {
    cat(i, file_name_v[i], "\n", sep = " ")
  }
}

# directory path to folder (latinx2022) containing .txt documents (data)
my_folder <- "/home/calviot/Desktop/latinx2022/data"

# show directory paths
show_files(my_folder)

# load .txt file containing news articles from Chicago Tribune
ct_text <- scan("/home/calviot/Desktop/latinx2022/data/ChicagoTribune.txt", what = "character", sep = "\n")
length(ct_text)
ct_text
# convert to lower case
ct_text <- tolower(ct_text)
ct_text
# create tibble
ct_text_df <- tibble(line = 1:length(ct_text), text = ct_text)
View(ct_text_df)

# access dataset with stop words from the tidytext package
data("stop_words", package = "tidytext")

# Chicago Tribune as tidytext as "ct_tt"
ct_tt <- ct_text_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

View(ct_tt) # is in tidytext format (one token per row)


# create a function to do all the above, and apply it to the other 5 newspapers' .txt documents
tidy_newspaper_text <- function(directory_path) {
  newspaper_text <- scan(directory_path, what = "character", sep = "\n")
  newspaper_text <- tolower(newspaper_text)
  newspaper_text <- tibble(line = 1:length(newspaper_text), text = newspaper_text)
  newspaper_tidy_text <- newspaper_text %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
  return(newspaper_tidy_text)
}

# call the tidy_newspaper_text() function on other newspapers' .txt files

# Houston Chronicle as tidytext as "hc_tt"
hc_tt <- tidy_newspaper_text("/home/calviot/Desktop/latinx2022/data/HoustonChronicle.txt")
View(hc_tt)
# Los Angeles Times as tidytext as "lat_tt"
lat_tt <- tidy_newspaper_text("/home/calviot/Desktop/latinx2022/data/LATimes.txt")
View(lat_tt)
# Miami Herald as tidytext as "mh_tt"
mi_tt <- tidy_newspaper_text("/home/calviot/Desktop/latinx2022/data/MiamiHerald.txt")
View(mi_tt)
# The New York Times as tidytext as "nyt_tt"
nyt_tt <- tidy_newspaper_text("/home/calviot/Desktop/latinx2022/data/NYTimes.txt")
View(nyt_tt)
# The Washington Post as tidytext as "wp_tt"
wp_tt <- tidy_newspaper_text("/home/calviot/Desktop/latinx2022/data/WashingtonPost.txt")
View(wp_tt)


################################################################################
### Text Comparison based on word frequency usage

# calculate frequency for each word for each newspaper and place it into a single data frame
word_frequency.1 <- bind_rows(mutate(mi_tt, author = "Miami Herald"),
                            mutate(lat_tt, author = "Los Angeles Times"),
                            mutate(ct_tt, author = "Chicago Tribune"),
                            mutate(hc_tt, author = "Houston Chronicle"),
                            mutate(wp_tt, author = "The Washington Post"),
                            mutate(nyt_tt, author = "The New York Times"))

View(word_frequency.1) # 170,657 rows
sum(is.na(word_frequency.1))

word_frequency.1 <- mutate(word_frequency.1, word = str_extract(word, "[^.,\\d]+")) # remove periods, commas and digits from the "word" column
View(word_frequency.1)
sum(is.na(word_frequency.1)) # 4,591 NAs

word_frequency.2 <- word_frequency.1 %>%
  count(author, word)
View(word_frequency.2)

word_frequency.3 <- word_frequency.2 %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n)) %>%
  select(-n)
View(word_frequency.3)

word_frequency.4 <- word_frequency.3 %>%
  spread(author, proportion) %>%
  select(word, `Miami Herald`, everything())
View(word_frequency.4)

word_frequency.5 <- word_frequency.4 %>%
  gather(`Chicago Tribune`:`The Washington Post`, key = author, value = proportion)
View(word_frequency.5)

# plot results
ggplot(word_frequency.5, aes(x = proportion, y = `Miami Herald`, color = abs(`Miami Herald` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 5) +
  theme(legend.position = "none") +
  labs(y = "Miami Herald", x = NULL) +
  theme_bw()

# save plot as .svg and modify it in Adobe Illustrator for better visualization
ggsave("comparative_word_frequency.svg", width = 5000, height = 3000, units = "px")

# how correlated are the word frequencies between Miami Herald's and the rest of the newspapers
cor.test(data = word_frequency.5[word_frequency.5$author == "Chicago Tribune",], ~ proportion + `Miami Herald`) # 0.867
cor.test(data = word_frequency.5[word_frequency.5$author == "Houston Chronicle",], ~ proportion + `Miami Herald`) # 0.881
cor.test(data = word_frequency.5[word_frequency.5$author == "Los Angeles Times",], ~ proportion + `Miami Herald`) # 0.845
cor.test(data = word_frequency.5[word_frequency.5$author == "The New York Times",], ~ proportion + `Miami Herald`) # 0.814
cor.test(data = word_frequency.5[word_frequency.5$author == "The Washington Post",], ~ proportion + `Miami Herald`) # 0.733


################################################################################
# How to quantify what a newspaper text is about?
# Term frequency - inverse document frequency tf-idf

# commonly used words
newspaper_words <- word_frequency.1 %>%
  count(author, word, sort = TRUE) %>%
  ungroup()

View(newspaper_words)

# total words per newspaper
newspaper_total_words <- newspaper_words %>%
  group_by(author) %>%
  summarize(total = sum(n))

View(newspaper_total_words)

# inspect the distribution of n/total newspaper: the number of times a word appears in a newspaper divided by the total
# number of words in that newspaper
news_words <- left_join(newspaper_words, newspaper_total_words)
View(news_words)

ggplot(news_words, aes(n/total, fill = author)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.004) +
  facet_wrap(~author, ncol = 3, scales = "free_y") +
  theme_bw()

# calculate tf-idf
news_words_tf_idf <- news_words %>%
  bind_tf_idf(word, author, n)

View(news_words_tf_idf) # words that appear in all newspapers' documents have idf = 0

# visualize words with high tf-idf values for each newspaper
news_words_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(author) %>%
  top_n(25) %>%
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf_idf") +
  facet_wrap(~author, ncol = 3, scales = "free") +
  coord_flip() +
  theme_bw()


################################################################################
### Sentiment Analysis

# access the sentiments dataset from the tidytext package
sentiments # this is the Bing lexicon
# access the AFINN and NRC lexicon
get_sentiments("afinn")
get_sentiments("nrc")

# inspect negative words from the Bing lexicon
bing_negative <- get_sentiments("bing") %>%
  filter(sentiment == "negative")

# concrete example: inspect negative words on Miami Herald
mi_negative_words <- mi_tt %>%
  inner_join(bing_negative) %>%
  count(word, sort = TRUE)
View(mi_negative_words)

# calculate sentiment score per 5 lines of text for each newspaper
bing_sentiment <- word_frequency.1 %>%
  inner_join(get_sentiments("bing")) %>%
  count(author, index = line %/% 5, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
View(bing_sentiment)

# plot sentiment scores across narrative time for each newspaper
ggplot(bing_sentiment, aes(index, sentiment, fill = author)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = met.brewer("Troy", 6)) +
  facet_wrap(~author, ncol = 2, scales = "free_x") +
  theme_bw()

# save plot as .svg and modify it in Adobe Illustrator for better visualization
ggsave("comparative_sentiment.svg", width = 4000, height = 3000, units = "px")

# identify most common positive and negative words for Miami Herald (A)
mi_word_counts <- mi_tt %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
View(mi_word_counts)

# plot the top 25 words contributing to each sentiment (negative vs. positive) in Miami Heralds' news articles (B)
mi_word_counts %>%
  group_by(sentiment) %>%
  top_n(25) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = met.brewer("Renoir", 2)) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "contribution to sentiment", x = NULL) +
  coord_flip() +
  theme_bw()

# save plot as .svg and modify it in Adobe Illustrator for better visualization (C)
ggsave("MiamiHerald_negative_positive_words.svg", width = 2000, height = 1500, units = "px")

# create a function that combines code from sections A, B, C
# top words contributing to sentiment as "top_words_to_sentiment"
top_words_to_sentiment <- function(newspaper_tidyText, my_plot_file_name) {
  # section A
  newspaper_tidyText_word_counts <- newspaper_tidyText %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()
  # section B
  my_plot <- newspaper_tidyText_word_counts %>%
    group_by(sentiment) %>%
    top_n(25) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    scale_fill_manual(values = met.brewer("Renoir", 2)) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "contribution to sentiment", x = NULL) +
    coord_flip() +
    theme_bw()
  return(my_plot)
  # section C
  return(ggsave(my_plot_file_name, width = 2000, height = 1500, units = "px"))
}

# top words contributing to sentiment in Chicago Tribune
top_words_to_sentiment(ct_tt, "ChicagoTribune_negative_positive_words.svg")
# top words contributing to sentiment in Houston Chronicle
top_words_to_sentiment(hc_tt, "HoustonChronicle_negative_positive_words.svg")
# top words contributing to sentiment in Los Angeles Times
top_words_to_sentiment(lat_tt, "LosAngelesTimes_negative_positive_words.svg")
# top words contributing to sentiment in The New York Times
top_words_to_sentiment(nyt_tt, "TheNewYorkTimes_negative_positive_words.svg")
# top words contributing to sentiment in The Washington Post
top_words_to_sentiment(wp_tt, "TheWashingtonPost_negative_positive_words.svg")

## import the above six .svg files into Adobe Illustrator to create a composite plot for visualizing results


################################################################################
### extract bi-grams

# tokenize text files by n-grams (bigrams)
bigrams <- function(directory_path) {
  newspaper_text <- scan(directory_path, what = "character", sep = "\n")
  newspaper_text <- tolower(newspaper_text)
  newspaper_text <- tibble(line = 1:length(newspaper_text), text = newspaper_text)
  newspaper_tidy_text <- newspaper_text %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  return(newspaper_tidy_text)
}

#3 extract bigrams from The New York Times
bigrams_nyt <- bigrams("/home/calviot/Desktop/latinx2022/data/NYTimes.txt")
bigrams_nyt

# count bigrams present in The New York Times
bigrams_nyt %>%
  count(bigram, sort = TRUE) # "of the" is the most common bigram

# separate bigrams into two columns: word1 and word2 in order to remove stop words from them
bigrams_nyt_separated <- bigrams_nyt %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_nyt_separated

# now filter out stop words
bigrams_nyt_filtered <- bigrams_nyt_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigrams_nyt_filtered

# now inspect most common bigrams (without stop words) in The New York Times
bigram_nyt_counts <- bigrams_nyt_filtered %>%
  count(word1, word2, sort = TRUE)

bigram_nyt_counts # "latin american" is the most common bigram occurring 48 times

# inspect bigrams in which word1 == latinx
latinx_bigrams <- bigrams_nyt_filtered %>%
  filter(word1 == "latinx") %>%
  count(word1, word2, sort = TRUE)

View(latinx_bigrams)

# inspect bigrams in which word1 == ms
# this is useful to identify female persons mentioned (a proxy for name entity recognition)
female_latinx_bigrams <- bigrams_nyt_filtered %>%
  filter(word1 == "ms") %>%
  count(word1, word2, sort = TRUE)

View(female_latinx_bigrams)

# visualize network of bigrams (occurring 3 or more times) for The New York Times
# create an igraph object with graph_from_data_frame() function
bigram_nyt_graph <- bigram_nyt_counts %>%
  filter(n >= 3) %>%
  graph_from_data_frame()

bigram_nyt_graph

# visualize network with ggraph()
set.seed(2023)
ggraph(bigram_nyt_graph, layout = "fr") +
  geom_edge_link(edge_width = 1) +
  geom_node_point(color = "orange", size = 4.5) +
  geom_node_text(aes(label = name)) +
  theme_void()

ggsave("nyt_bigram_plot.svg", width = 1000, height = 1000, units = "px")


## create a function that extract bigrams in which word1 == latinx
word1_latinx_bigrams <- function(dataset) {
  bigrams_newspaper <- bigrams(dataset) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
    filter(word1 == "latinx") %>%
    count(word1, word2, sort = TRUE)
  return(bigrams_newspaper)
}

# The New York Times latinx bigrams
word1_latinx_bigrams("/home/calviot/Desktop/latinx2022/data/NYTimes.txt")
# Chicago Tribune latinx bigrams
word1_latinx_bigrams("/home/calviot/Desktop/latinx2022/data/ChicagoTribune.txt")
# Houston Chronicle latinx bigrams
word1_latinx_bigrams("/home/calviot/Desktop/latinx2022/data/HoustonChronicle.txt")
# Los Angeles Times latinx bigrams
word1_latinx_bigrams("/home/calviot/Desktop/latinx2022/data/LATimes.txt")
# Miami Herald latinx bigrams
word1_latinx_bigrams("/home/calviot/Desktop/latinx2022/data/MiamiHerald.txt")
# The Washington Post latinx bigrams
word1_latinx_bigrams("/home/calviot/Desktop/latinx2022/data/WashingtonPost.txt")


## create a function that visualizes bigrams
visualize_bigrams <- function(dataset) {
  bigrams_newspaper <- bigrams(dataset) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE) %>%
    filter(n >= 3) %>%
    graph_from_data_frame()
  
  my_bigram_network_plot <- ggraph(bigrams_newspaper, layout = "fr") +
    geom_edge_link() +
    geom_node_point(color = "orange", size = 4.5) +
    geom_node_text(aes(label = name)) +
    theme_void()
  
  return(my_bigram_network_plot)
}

# The New York Times bigrams' visualization
visualize_bigrams("/home/calviot/Desktop/latinx2022/data/NYTimes.txt")
# Chicago Tribune bigrams' visualization
visualize_bigrams("/home/calviot/Desktop/latinx2022/data/ChicagoTribune.txt")
# Houston Chronicle bigrams' visualization
visualize_bigrams("/home/calviot/Desktop/latinx2022/data/HoustonChronicle.txt")
# Los Angeles Times bigram's visualization
visualize_bigrams("/home/calviot/Desktop/latinx2022/data/LATimes.txt")
# Miami Herald bigrams' visualization
visualize_bigrams("/home/calviot/Desktop/latinx2022/data/MiamiHerald.txt")
# The Washington Post bigrams' visualization
visualize_bigrams("/home/calviot/Desktop/latinx2022/data/WashingtonPost.txt")


################################################################################  
## Correlating Pairs of Words across sections of text

install.packages("widyr", dependencies = TRUE)
library(widyr)

# what words tend to appear within 5-lines sections of text?
# let's focus on Chicago Tribune first as concrete example
section_words <- word_frequency.1 %>%
  mutate(section = row_number() %/% 5) %>%
  filter(author == "Chicago Tribune") %>%
  filter(section > 0) %>%
  filter(!word %in% stop_words$word)

View(section_words)

 # count words co-occurring within sections
word_pairs <- section_words %>%
  pairwise_count(word, section, sort = TRUE)

View(word_pairs)

# which words co-occur with "latinx" for every 5-lines section of text?
# "undocumented" was the word that contributed the most to negative sentiment in Chicago Tribune
word_pairs %>%
  filter(item1 == "undocumented") %>%
  print(n = 25)

# "affordable" was the workd that contributed the most to positive sentiment in Chicago Tribune
word_pairs %>%
  filter(item1 == "affordable") %>%
  print(n = 25)


# find correlated words
word_cors <- section_words %>%
  group_by(word) %>%
  filter(n() >= 3) %>%
  pairwise_cor(word, section, sort = TRUE)

View(word_cors)  


# what are the most correlated words with "undocumented" and "affordable" in Chicago Tribune? 
word_cors %>%
  filter(item1 %in% c("undocumented", "affordable")) %>%
  group_by(item1) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~item1, scales = "free") +
  coord_flip()

# visualize network of correlated words within 5-lines sections of text for Chicago Tribune
word_cors %>%
  na.omit() %>%
  filter(item1 %in% c("undocumented", "issues", "hard", "affordable", "support", "helped")) %>%
  filter(correlation > 0.01) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "orange", size = 4.5) +
  geom_node_text(aes(label = name)) +
  theme_void()


# create function to identify most correlated words within 5-lines of text, and apply it to newspapers
correlated_word_pairs <- function(newspaper_name, negative_term, positive_term) {
  section_words <- word_frequency.1 %>%
    mutate(section = row_number() %/% 5) %>%
    filter(author == newspaper_name) %>%
    filter(section > 0) %>%
    filter(!word %in% stop_words$word)
  
  word_cors <- section_words %>%
    group_by(word) %>%
    filter(n() >= 3) %>%
    pairwise_cor(word, section, sort = TRUE)
  
  word_cors %>%
    filter(item1 %in% c(negative_term, positive_term)) %>%
    group_by(item1) %>%
    top_n(15) %>%
    ungroup() %>%
    mutate(item2 = reorder(item2, correlation)) %>%
    ggplot(aes(item2, correlation)) +
    geom_bar(stat = "identity") +
    facet_wrap(~item1, scales = "free") +
    coord_flip()
}

# calculate correlated pairs of words and visualize them
correlated_word_pairs("Chicago Tribune", "undocumented", "affordable")
correlated_word_pairs("Houston Chronicle", "issues", "support")
correlated_word_pairs("Los Angeles Times", "queer", "support")
correlated_word_pairs("Miami Herald", "racism", "support")
correlated_word_pairs("The New York Times", "issues", "support")
correlated_word_pairs("The Washington Post", "hard", "support")


# create function to visualize network of correlated words associated with my words of choice
correlated_words_to_my_selected_words <- function(newspaper_name, vector_with_words_of_choice) {
  section_words <- word_frequency.1 %>%
    mutate(section = row_number() %/% 5) %>%
    filter(author == newspaper_name) %>%
    filter(section > 0) %>%
    filter(!word %in% stop_words$word)
  
  word_cors <- section_words %>%
    group_by(word) %>%
    filter(n() >= 3) %>%
    pairwise_cor(word, section, sort = TRUE)
  
  word_cors %>%
    na.omit() %>%
    filter(item1 %in% vector_with_words_of_choice) %>%
    filter(correlation >= 0.07) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link() +
    geom_node_point(color = "orange", size = 4.5) +
    geom_node_text(aes(label = name)) +
    theme_void()
}

# Chicago Tribune
ct_words <- c("undocumented", "issues", "hard", "affordable", "support", "helped")
correlated_words_to_my_selected_words("Chicago Tribune", ct_words)
# Houston Chronicle
hc_words <- c("issues", "racism", "hate", "support", "love", "free")
correlated_words_to_my_selected_words("Houston Chronicle", hc_words)
# Los Angeles Times
lat_words <- c("queer", "hard", "lack", "support", "love", "free")
correlated_words_to_my_selected_words("Los Angeles Times", lat_words)
# Miami Herald
mihe_words <- c("racism", "issues", "hate", "support", "love", "pride")
correlated_words_to_my_selected_words("Miami Herald", mihe_words)
# The New York Times
nyt_words <- c("issues", "dirt", "queer", "support", "love", "won")
correlated_words_to_my_selected_words("The New York Times", nyt_words)
# The Washington Post
wp_words <- c("hard", "risk", "issue", "support", "educated", "trust")
correlated_words_to_my_selected_words("The Washington Post", wp_words)

