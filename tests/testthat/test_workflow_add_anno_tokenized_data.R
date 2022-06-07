library(data.table)

testthat::context("Adding annotation layer to tokenized data")

test_that(
  "Workflow #1 explained in the vignette",
  {
    ts_ws <- lapply(
      split(reuters_dt, f = reuters_dt[["doc_id"]]),
      function(tab) paste(tab[["word"]], collapse = " ")
    )
    
    properties_list <- list(
      "annotators" = "tokenize, ssplit",
      "tokenize.whitespace" = "true"
    )
    Pipe <- StanfordCoreNLP$new(
      properties = properties_list,
      output_format = "conll"
    )
    Pipe$verbose(FALSE) # ineffective, messages shown anyway
    
    annoli <- AnnotationList$new(ts_ws, purge = FALSE)
    
    # Do not purge because original text includes things that look like tags.
    Pipe$annotate(annoli, purge = FALSE)
    
    
    reuters_dt_v2 <- annoli$as.data.table()
    
    # Check that the number of tokens is identical
    expect_identical(nrow(reuters_dt), nrow(reuters_dt_v2))
    
    # Check that the tokens are identical
    expect_identical(reuters_dt[["word"]], reuters_dt_v2[["word"]])
  }
)
