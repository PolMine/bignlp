test_that(
  "corenlp_annotate for xml_document", 
  {
    xml_dir <- system.file(package = "bignlp", "extdata", "xml")
    xml_files <- list.files(xml_dir, full.names = TRUE)
    xml_doc <- xml2::read_xml(x = xml_files[[1]])

    Pipe <- StanfordCoreNLP$new(
      output_format = "conll",
      properties = corenlp_get_properties_file(lang = "en", fast = TRUE)
    )

    corenlp_annotate(x = xml_doc, pipe = Pipe, cols = "word", fmt = '<w word="%s"/>\n', sentences = TRUE)

    y <- tempfile(fileext = ".xml")
    xml2::write_xml(x = xml_doc, file = y, options = NULL)

  }
)