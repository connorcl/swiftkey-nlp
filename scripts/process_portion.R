
args = commandArgs(trailingOnly = TRUE)

if(length(args) == 0) {
  stop("Please specify which portion to process (1-10)")
} else {
  portion = min(as.integer(args[1]), 10)
  portion = max(portion, 1)
}

#print(portion)

path <- "~/Projects/swiftkey-nlp/data/"
docs <- c("blogs", "news", "twitter")

setwd(path)

# create portion files
for (doc in docs) {
  #print(doc)
  # get filename
  filename <- paste0(path, "final/en_US/en_US.", doc, ".txt")
  portion_filename <- paste0(path, "portion_", as.character(portion), "_", doc, ".txt")
  # create doc file con
  doc_con <- file(filename)
  # read data
  data <- readLines(doc_con)
  close(doc_con)
  # split into portions
  len <- length(data)
  #print(paste("len", len))
  size <- len %/% 10
  #print(paste("size", size))
  start <- ((portion - 1) * size) + 1
  #print(paste("start", start))
  if (portion == 10) {
     size <- size + (len %% 10)
  }
  #print(paste("size", size))
  # keep relevant portion
  data <- data[start:(start+size-1)]
  # write result
  portion_con <- file(portion_filename)
  writeLines(data, portion_con)
  close(portion_con)
}

## load data

library(readtext)
library(quanteda)
library(dplyr)

freq_dfs <- NULL

# load portion
data <- readtext(paste0(path, "/portion_", portion, "_*.txt"))
# create corpus
portion_corpus <- corpus(data)
print(str(portion_corpus))
# load profanity dictionary
profanity <- read.csv(paste(path, "../profanity.csv", sep=""), sep=";", header = FALSE, stringsAsFactors = FALSE)$V1
# generate tokens object
portion_tokens <- portion_corpus %>% 
  tokens(remove_punct=TRUE, remove_numbers=TRUE, remove_twitter=TRUE, remove_url=TRUE) %>%
  tokens_select(profanity, selection='remove') %>%
  tokens_tolower()

# clean memory
rm(data)
rm(portion_corpus)
rm(profanity)
gc(full=TRUE)

for (i in 1:5) {
  
  print(i)
  
  if (i == 1) {
    portion_tokens_ng <- portion_tokens
  } else {
    portion_tokens_ng <- tokens_ngrams(portion_tokens, i)
  }
  
  portion_dfm <- dfm(portion_tokens_ng)
  
  rm(portion_tokens_ng)
  gc(full=TRUE)
  
  portion_dfm <- dfm_trim(portion_dfm, min_termfreq = 4, termfreq_type="count")
  
  gc(full=TRUE)
  
  portion_freq_df <- textstat_frequency(portion_dfm) %>% select(feature, frequency)
  
  rm(portion_dfm)
  gc(full=TRUE)
  
  filename <- paste0("portion_", portion, "_", i, "gram_freq.csv")
  
  write.csv(portion_freq_df, filename)
  
  rm(portion_freq_df)
  gc(full=TRUE)
}




