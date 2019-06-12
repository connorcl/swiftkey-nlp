set.seed(1234)

docs <- c("blogs", "news", "twitter")
path <- "~/Projects/swiftkey-nlp/data/"

for (doc in docs) {
  filename <- paste(path, "final/en_US/en_US.", doc, ".txt", sep="")
  sample_filename <- paste(path, "sample_", doc, ".txt", sep="")
  
  data <- readLines(file(filename))
  keep <- as.logical(rbinom(data, 1, 0.1))
  sample <- data[keep]
  
  writeLines(sample, file(sample_filename))
}