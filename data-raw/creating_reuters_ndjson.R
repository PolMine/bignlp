library(bignlp)
library(data.table)
reuters_dt <- data.table(
  text = readLines(system.file(package = "bignlp", "extdata", "txt", "reuters.txt"))
)
reuters_dt[["id"]] <- 1L:nrow(reuters_dt)

destfile <- tempfile()
corenlp_dir <- system.file(package = "cleanNLP", "extdata", "stanford-corenlp-full-2016-10-31")
properties_english_fast <- system.file(
  package = "cleanNLP", "extdata", "StanfordCoreNLP-english-fast.properties"
)
y <- bignlp:::corenlp_annotate(
  x = reuters_dt,
  destfile = NULL,
  properties_file = properties_english_fast,
  corenlp_dir = corenlp_dir,
  threads = 1L,
  progress = FALSE
  )
writeLines(text = y, con = "~/Lab/github/bignlp/inst/extdata/ndjson/reuters.ndjson")
