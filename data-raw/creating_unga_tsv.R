unga_xml_files <- list.files(system.file(package = "bignlp", "extdata", "xml"), full.names = TRUE)

library(cwbtools)
library(data.table)

CD <- CorpusData$new()
CD$import_xml(filenames = unga_xml_files)
data.table::fwrite(x = CD$chunktable, file = "~/Lab/github/bignlp/inst/extdata/tsv/unga.tsv", sep = "\t")
