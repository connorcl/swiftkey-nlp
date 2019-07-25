blogs <- readLines(file('data/en_US.blogs.txt'))
news <- readLines(file('data/en_US.news.txt'))
twitter <- readLines(file('data/en_US.twitter.txt'))

process <- function(s) {
  s <- tolower(s)
  s <- gsub("\u2019", "'", s)
  s <- gsub("\u2010", "-", s)
  s <- gsub("[^-' a-z]", "", s)
  s <- gsub("\\s{2,}", " ", s)
  s <- strsplit(s, " ")[[1]]
  return(s)
}