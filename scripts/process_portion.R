### process_portion.R - takes a portion number between 1 and 10 
### as a command line argument and processes this tenth of the data

## process command line arguments

args = commandArgs(trailingOnly = TRUE)

if(length(args) == 0) {
  stop("Please specify which portion to process (1-10)")
} else {
  portion = min(as.integer(args[1]), 10)
  portion = max(portion, 1)
}

print(paste("Processing portion", portion))

## create portion files

docs <- c("blogs", "news", "twitter")
path <- "~/Projects/swiftkey-nlp/data/"
setwd(path)

# initialize vector of portion filenames
portion_filenames <- NULL

# for each document (blogs, news, twitter)
for (doc in docs) {
  # generate filenames
  filename <- paste0(path, "final/en_US/en_US.", doc, ".txt")
  portion_filenames[doc] <- paste0(path, "portion_", as.character(portion), "_", doc, ".txt")
  # create document file connection
  doc_con <- file(filename)
  # read data
  data <- readLines(doc_con)
  close(doc_con)
  # split into portions
  data_length <- length(data)
  portion_size <- data_length %/% 10
  # calculate portion start based on size of first nine portions
  portion_start <- ((portion - 1) * portion_size) + 1
  # tenth portion may be slightly larger to cover entire document
  if (portion == 10) {
     portion_size <- portion_size + (data_length %% 10)
  }
  # only keep specified portion
  data <- data[portion_start:(portion_start+portion_size-1)]
  # write result to file
  portion_con <- file(portion_filenames[doc])
  writeLines(data, portion_con)
  close(portion_con)
}

## load data

# load required libraries
require(readtext)
require(quanteda)
require(dplyr)

# load portion files
data <- readtext(paste0(path, "/portion_", portion, "_*.txt"))
# create corpus
portion_corpus <- corpus(data)
# load profanity dictionary
profanity <- read.csv(paste0(path, "/profanity.csv"), sep=";", header = FALSE, stringsAsFactors = FALSE)$V1
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

# for each n for which ngrams will be generated
for (n in 1:5) {
  
  print(paste0("Generating ", n, "-grams"))
  
  # generate ngrams if n > 1
  if (n == 1) {
    portion_tokens_ng <- portion_tokens
  } else {
    portion_tokens_ng <- tokens_ngrams(portion_tokens, n)
  }
  
  # create document feature matrix
  portion_dfm <- dfm(portion_tokens_ng)
  # clean memory
  rm(portion_tokens_ng)
  gc(full=TRUE)
  
  # trim dfm, only keeping terms appearing at least twice
  portion_dfm <- dfm_trim(portion_dfm, min_termfreq = 2, termfreq_type="count")
  # clean memory
  gc(full=TRUE)
  
  # generate ngram frequency data frame
  portion_freq_df <- textstat_frequency(portion_dfm) %>% select(feature, frequency)
  # clean memory
  rm(portion_dfm)
  gc(full=TRUE)
  
  # generate output filename
  filename <- paste0("portion_", portion, "_", n, "gram_freq.csv")
  # write term frequency dataframe to output file
  write.csv(portion_freq_df, filename)
  # clean memory
  rm(portion_freq_df)
  gc(full=TRUE)
}

## remove portion files
for (f in portion_filenames) {
  file.remove(f)
}