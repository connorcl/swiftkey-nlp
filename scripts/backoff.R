### stupid backoff langauge model

## sampling

set.seed(1234)

docs <- c("blogs", "news", "twitter")
path <- "~/Projects/swiftkey-nlp/data/"

for (doc in docs) {
  # get filename
  filename <- paste0(path, "final/en_US/en_US.", doc, ".txt")
  sample_filename <- paste0(path, "sample_", doc, ".txt")
  # read data
  data <- readLines(file(filename))
  # generate logical vector of random 1% of lines to keep
  keep <- as.logical(rbinom(data, 1, 1))
  # subset using this vector
  sample <- data[keep]
  # write samples to new files
  writeLines(sample, file(sample_filename))
}

## load data

library(readtext)
library(quanteda)
library(data.table)
library(dplyr)

# load sample data
path <- "~/Projects/swiftkey-nlp/"
sample_data <- readtext(paste(path, "/data/*.txt", sep=""))
# create corpus
sample_corpus <- corpus(sample_data)


# load profanity dictionary
profanity <- read.csv(paste(path, "profanity.csv", sep=""), sep=";", header = FALSE, stringsAsFactors = FALSE)$V1
# generate tokens object
sample_tokens <- sample_corpus %>% 
  tokens(remove_punct=TRUE, remove_numbers=TRUE, remove_twitter=TRUE, remove_url=TRUE) %>%
  tokens_select(profanity, selection='remove') %>%
  tokens_tolower()
# generate dfm object
sample_dfm <- dfm(sample_tokens)
# trim features occuring fewer than 5 times
sample_dfm <- dfm_trim(sample_dfm, min_termfreq = 5, termfreq_type="count")
# create dataframe
freq_df <- textstat_frequency(sample_dfm) %>% select(feature, frequency)

# generate 2-gram tokens objects
sample_tokens_2gram <- tokens_ngrams(sample_tokens, 2)
sample_dfm_2gram <- dfm(sample_tokens_2gram)
# trim features occuring fewer than 5 times
sample_dfm_2gram <- dfm_trim(sample_dfm_2gram, min_termfreq = 4, termfreq_type="count")
# create dataframe
freq_df_2gram <- textstat_frequency(sample_dfm_2gram) %>% select(feature, frequency)

# 3-grams
sample_tokens_3gram <- tokens_ngrams(sample_tokens, 3)
sample_dfm_3gram <- dfm(sample_tokens_3gram)
# trim features occuring fewer than 5 times
sample_dfm_3gram <- dfm_trim(sample_dfm_3gram, min_termfreq = 3, termfreq_type="count")
# create dataframe
freq_df_3gram <- textstat_frequency(sample_dfm_3gram) %>% select(feature, frequency)

# 4-grams
sample_tokens_4gram <- tokens_ngrams(sample_tokens, 4)
sample_dfm_4gram <- dfm(sample_tokens_4gram)
# trim features occuring fewer than 5 times
sample_dfm_4gram <- dfm_trim(sample_dfm_4gram, min_termfreq = 3, termfreq_type="count")
# create dataframe
freq_df_4gram <- textstat_frequency(sample_dfm_4gram) %>% select(feature, frequency)


# 5-grams
sample_tokens_5gram <- tokens_ngrams(sample_tokens, 5)
sample_dfm_5gram <- dfm(sample_tokens_5gram)
# trim features occuring fewer than 5 times
sample_dfm_5gram <- dfm_trim(sample_dfm_5gram, min_termfreq = 2, termfreq_type="count")
# create dataframe
freq_df_5gram <- textstat_frequency(sample_dfm_5gram) %>% select(feature, frequency)

















