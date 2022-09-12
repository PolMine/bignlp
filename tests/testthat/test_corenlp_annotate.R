test_that(
  "corenlp_annotate for xml_document", 
  {
    xml_dir <- system.file(package = "bignlp", "extdata", "xml")
    xml_files <- list.files(xml_dir, full.names = TRUE)
    xml_doc <- xml2::read_xml(x = xml_files[[1]])

    # first we run the exercise with conll output
    
    Pipe <- StanfordCoreNLP$new(
      output_format = "conll",
      properties = corenlp_get_properties_file(lang = "en", fast = TRUE)
    )

    corenlp_annotate(x = xml_doc, pipe = Pipe, cols = c("word", "word"), fmt = '<w word="%s">%s</w>\n', sentences = TRUE)
    tokens1 <- xml2::xml_text(xml2::xml_find_all(xml_doc, xpath = "//w"))

    
    # with XML outputter

    xml_doc2 <- xml2::read_xml(x = xml_files[[1]])
    
    Pipe <- StanfordCoreNLP$new(
      output_format = "xml",
      properties = corenlp_get_properties_file(lang = "en", fast = TRUE)
    )
    corenlp_annotate(x = xml_doc2, pipe = Pipe, xpath = "//p")
    tokens2 <- xml_text(xml2::xml_find_all(xml_doc2, xpath = "//word"))
    
    expect_identical(tokens1, tokens2)
  }
)

test_that(
  "corenlp_annotate for data.table (XML outputter)", 
  {
    library(data.table)
    library(xml2)
    
    fname <- system.file(package = "bignlp", "extdata", "txt", "reuters.txt")
    reuters_txt <- readLines(fname)
    reuters_dt <- data.table(
      doc_id = seq_along(reuters_txt),
      text = reuters_txt
    )

    props <- corenlp_get_properties_file(lang = "en", fast = "TRUE")
    conll_tokens <- corenlp_annotate(
      x = reuters_dt,
      pipe = props,
      progress = FALSE,
      threads = 1L
    )[["word"]]

    Pipe <- StanfordCoreNLP$new(output_format = "xml", properties = props)
    y <- corenlp_annotate(
      x = reuters_dt,
      pipe = Pipe,
      progress = FALSE,
      threads = 1L
    )
    
    xml_tokens <- unlist(lapply(
      y[["xml"]],
      function(doc) xml_text(xml_find_all(read_xml(doc), xpath = "//word"))
    ))
    
    expect_identical(xml_tokens, conll_tokens)
    
    y <- corenlp_annotate(
      x = reuters_dt,
      pipe = Pipe,
      progress = FALSE,
      inmemory = TRUE,
      threads = 2L
    )
    
    xml_tokens <- unlist(lapply(
      y[["xml"]],
      function(doc) xml_text(xml_find_all(read_xml(doc), xpath = "//word"))
    ))
    
    expect_identical(xml_tokens, conll_tokens)
    
    y <- corenlp_annotate(
      x = reuters_dt,
      pipe = Pipe,
      progress = FALSE,
      inmemory = FALSE,
      threads = 2L
    )
    
    xml_tokens <- unlist(lapply(
      y[["xml"]],
      function(doc) xml_text(xml_find_all(read_xml(doc), xpath = "//word"))
    ))
    
    expect_identical(xml_tokens, conll_tokens)
    
    
  }
)