# This script requires that bignlp is installed and that it already includes the csv 
# version

library(bignlp)

reuters_files <- system.file("texts", "crude", package = "tm")
reuters_vcorpus <- VCorpus(DirSource(reuters_files), list(reader = readReut21578XMLasPlain))
reuters_txt <- unname(unlist(lapply(reuters_vcorpus, function(x) gsub("\\s+", " ", gsub("\\n", " ", x[["content"]])))))
reuters_dt <- data.table(
  doc_id = 1L:length(reuters_txt),
  text = reuters_txt
)

annoli <- AnnotationList$new(reuters_dt[["text"]])
Pipe <- AnnotationPipeline$new()
Pipe$annotate(annoli)
reuters_dt <- annoli$as.data.table()

save(
  reuters_dt,
  file = "~/Lab/github/bignlp/data/reuters.RData"
)