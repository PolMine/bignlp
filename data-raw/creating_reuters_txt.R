library(tm)

reuters_files <- system.file("texts", "crude", package = "tm")
reuters_vcorpus <- VCorpus(DirSource(reuters_files), list(reader = readReut21578XMLasPlain))
reuters_txt <- unname(unlist(lapply(reuters_vcorpus, function(x) gsub("\\s+", " ", gsub("\\n", " ", x[["content"]])))))
writeLines(text = reuters_txt, con = "~/Lab/github/bignlp/inst/extdata/txt/reuters.txt")
