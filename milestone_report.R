### load files

path <- "~/Projects/swiftkey-nlp/data/"
corpora_path <- "final/en_US/"

docs <- c("blogs", "news", "twitter")

filenames <- sapply(docs, (function(f) { paste0(path, corpora_path, "en_US.", f, ".txt") }))

data <- sapply(filenames, readLines)

### basic file statistics

library(stringi)

char_count <- sapply(data, (function(c) { sum(nchar(c)) }))
word_count <- sapply(data, (function(c) { sum(stri_stats_latex(c)[["Words"]]) }))
line_count <- sapply(data, length)
mean_line_chars <- char_count / line_count
mean_line_words <- word_count / line_count
mean_word_length <- char_count / word_count

file_stats <- data.frame(doc = docs, char_count, word_count, line_count, mean_line_chars, mean_line_words, mean_word_length, row.names = NULL)

### plot basic file statistics

library(xtable)

print(xtable(file_stats), comment=F)

library(ggplot2)
library(gridExtra)

p1 <- ggplot(file_stats) + aes(x = doc, y = char_count) + geom_bar(stat = "identity")
p2 <- ggplot(file_stats) + aes(x = doc, y = word_count) + geom_bar(stat = "identity")
p3 <- ggplot(file_stats) + aes(x = doc, y = line_count) + geom_bar(stat = "identity")
p4 <- ggplot(file_stats) + aes(x = doc, y = mean_line_chars) + geom_bar(stat = "identity")
p5 <- ggplot(file_stats) + aes(x = doc, y = mean_line_words) + geom_bar(stat = "identity")
p6 <- ggplot(file_stats) + aes(x = doc, y = mean_word_length) + geom_bar(stat = "identity")

grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3)

### sample data

set.seed(1234)

for (doc in docs) {
  filename <- paste0(path, "final/en_US/en_US.", doc, ".txt")
  sample_filename <- paste0(path, "sample_", doc, ".txt")
  
  data <- readLines(file(filename))
  keep <- as.logical(rbinom(data, 1, 0.01))
  sample <- data[keep]
  
  writeLines(sample, file(sample_filename))
}

### process sample data

library(readtext)
library(quanteda)
library(dplyr)

path <- "~/Projects/swiftkey-nlp/"
sample_data <- readtext(paste(path, "/data/*.txt", sep=""))

sample_corpus <- corpus(sample_data)

profanity <- read.csv(paste(path, "profanity.csv", sep=""), sep=";", header = FALSE, stringsAsFactors = FALSE)$V1

sample_tokens <- sample_corpus %>% 
  tokens(remove_punct=TRUE, remove_numbers=TRUE, remove_twitter=TRUE, remove_url=TRUE) %>%
  tokens_select(profanity, selection='remove') %>%
  tokens_tolower()

sample_dfm <- dfm(sample_tokens)

features_le4 <- textstat_frequency(sample_dfm) %>% filter(frequency <= 4) %>% .$feature

sample_tokens <- sample_tokens %>% tokens_select(features_le4, selection='remove')

sample_dfm <- dfm(sample_tokens)

### further exploratory analysis

sample_tokens_2gram <- tokens_ngrams(sample_tokens, 2)
sample_tokens_3gram <- tokens_ngrams(sample_tokens, 3)

sample_dfm_2gram <- dfm(sample_tokens_2gram)
sample_dfm_3gram <- dfm(sample_tokens_3gram)

freq_df <- textstat_frequency(sample_dfm) %>% mutate(feature = as.factor(feature))
freq_df_2gram <- textstat_frequency(sample_dfm_2gram) %>% mutate(feature = as.factor(feature))
freq_df_3gram <- textstat_frequency(sample_dfm_3gram) %>% mutate(feature = as.factor(feature))

tokens <- nfeat(sample_dfm)

textplot_wordcloud(sample_dfm, max_words = 100)

freq_top30 <- freq_df_ge3 %>% slice(1:30)
freq_ge10k <- freq_df_ge3 %>% filter(frequency >= 10000)

ggplot(freq_top30) + 
  aes(x = reorder(feature, frequency), y = frequency) + 
  ggtitle("Top 30 1-grams by frequency") +
  xlab("feature (1-gram)") +
  geom_col(fill='darkred') +
  coord_flip()

ggplot(freq_ge10k) + 
  aes(x = reorder(feature, frequency), y = frequency) + 
  ggtitle("1-grams appearing more than 10,000 times") +
  xlab("feature (1-gram)") +
  geom_col(fill='steelblue4') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

freq_2gram_top30 <- freq_df_2gram %>% slice(1:30)
freq_2gram_ge5c <- freq_df_2gram %>% filter(frequency >= 500)

ggplot(freq_2gram_top30) + 
  aes(x = reorder(feature, frequency), y = frequency) +
  ggtitle("Top 30 2-grams by frequency") +
  xlab("feature (2-gram)") +
  geom_col(fill='darkred') +
  coord_flip()

ggplot(freq_2gram_ge5c) + 
  aes(x = reorder(feature, frequency), y = frequency) +
  ggtitle("2-grams appearing more than 500 times") +
  xlab("feature (2-gram)") +
  geom_col(fill='steelblue4') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

freq_3gram_top30 <- freq_df_3gram %>% slice(1:30)
freq_3gram_ge50 <- freq_df_3gram %>% filter(frequency >= 50)

ggplot(freq_3gram_top30) + 
  aes(x = reorder(feature, frequency), y = frequency) + 
  ggtitle("Top 30 3-grams by frequency") +
  xlab("feature (3-gram)") +
  geom_col(fill='darkred') + 
  coord_flip()

ggplot(freq_3gram_ge50) + 
  aes(x = reorder(feature, frequency), y = frequency) + 
  ggtitle("3-grams appearing more than 50 times") +
  xlab("feature (3-gram)") +
  geom_col(fill='steelblue4') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

total_tokens <- sum(ntoken(sample_dfm))
total_features <- nfeat(sample_dfm)

features_needed <- function(required_coverage)
{
  current_feature = 0
  current_tokens = 0
  
  while (current_tokens < total_tokens * required_coverage) {
    current_feature = current_feature + 1
    current_tokens = current_tokens + freq_df[current_feature,]$frequency
  }
  
  return(current_feature)
}