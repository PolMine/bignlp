library(data.table)

testthat::context("results")

test_that(
  "results of methods are identical",
  {
    reuters_txt <- readLines(system.file(package = "bignlp", "extdata", "txt", "reuters.txt"))
    reuters_dt <- data.table(doc_id = 1L:length(reuters_txt), text = reuters_txt)
    
    props <- properties(corenlp_get_properties_file(lang = "en", fast = TRUE))
    
    Pipe <- StanfordCoreNLP$new(properties = props, output_format = "conll")
    Pipe$verbose(FALSE)
    segdirs <- segment(x = reuters_dt, dir = (nlpdir <- tempdir()), chunksize = 10L)
    conll_files <- lapply(segdirs, Pipe$process_files)
    Sys.sleep(0.2)
    dt1 <- corenlp_parse_conll(conll_files, progress = FALSE)
    
    dt2 <- corenlp_annotate(reuters_dt, pipe = props, progress = FALSE)
    
    expect_identical(dt1, dt2)
  }
)
