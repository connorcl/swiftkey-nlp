
library(dplyr)

path <- "~/Projects/swiftkey-nlp/data/"
setwd(path)

generate_ngram_re <- function(n) {
  
  word_re <- "[a-z]+'{0,1}[a-z]*"
  
  re <- paste0("^", word_re)
  
  if (n > 1) {
    for (i in 1:(n-1)) {
      re <- paste0(re, "_", word_re)
    }
  }
  
  re <- paste0(re, "$")
  
  return(re)
  
}

for (n in 1:5) {
  
  freq_df <- NULL
  
  for (i in 1:10) {
    filename <- paste0("portion_", i, "_", n,"gram_freq.csv")
    portion_df <- read.csv(filename, stringsAsFactors = FALSE)
    freq_df <- rbind(freq_df, portion_df)
  }
  
  freq_df <- freq_df %>% 
    select(feature, frequency) %>%
    group_by(feature) %>%
    summarize_all(sum) %>%
    filter(grepl(generate_ngram_re(n), feature) & frequency >= 4)
  
  output_filename <- paste0("freq_df_", n, "gram.csv")
  write.csv(freq_df, output_filename, row.names = FALSE)
}