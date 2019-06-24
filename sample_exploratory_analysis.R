require(quanteda)
require(dplyr)
require(ggplot2)

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

tokens_needed <- function(required_coverage)
{
  current_feature = 0
  current_tokens = 0
  
  while (current_tokens < total_tokens * required_coverage) {
    current_feature = current_feature + 1
    current_tokens = current_tokens + freq_df[current_feature,]$frequency
  }
  
  return(current_feature)
}