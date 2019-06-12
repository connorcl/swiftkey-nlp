require(readtext)
require(quanteda)
require(dplyr)

path <- "~/Projects/swiftkey-nlp/"
sample_data <- readtext(paste(path, "/data/*.txt", sep=""))

sample_corpus <- corpus(sample_data)

profanity <- read.csv(paste(path, "profanity.csv", sep=""), sep=";", header = FALSE, stringsAsFactors = FALSE)$V1

sample_tokens <- sample_corpus %>% 
  tokens(remove_punct=TRUE, remove_numbers=TRUE, remove_twitter=TRUE, remove_url=TRUE) %>%
  tokens_select(stopwords("english"), selection='remove') %>%
  tokens_select(profanity, selection='remove') %>%
  tokens_tolower()

sample_tokens_stem <- sample_tokens %>% tokens_wordstem()

sample_dfm <- dfm(sample_tokens)
sample_dfm_stem <- dfm(sample_tokens_stem)