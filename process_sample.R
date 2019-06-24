require(readtext)
require(quanteda)
require(dplyr)

path <- "~/Projects/swiftkey-nlp/"
sample_data <- readtext(paste(path, "/data/*.txt", sep=""))

sample_corpus <- corpus(sample_data)

profanity <- read.csv(paste(path, "profanity.csv", sep=""), sep=";", header = FALSE, stringsAsFactors = FALSE)$V1

sample_tokens <- sample_corpus %>% 
  tokens(remove_punct=TRUE, remove_numbers=TRUE, remove_twitter=TRUE, remove_url=TRUE) %>%
  tokens_select(profanity, selection='remove') %>%
  tokens_tolower()

sample_dfm <- dfm(sample_tokens)

features_le9 <- textstat_frequency(sample_dfm) %>% filter(frequency <= 9) %>% .$feature

sample_tokens <- sample_tokens %>% tokens_select(features_le9, selection='remove')

sample_dfm <- dfm(sample_tokens)