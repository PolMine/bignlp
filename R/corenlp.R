#' Annotate a string.
#' 
#' Use CoreNLP to annotate strings.
#' 
#' If argument `threads` is 1, the tagging  result is returned, if output is
#' NULL. If `threads` is higher than 1, `output` should be a directory where
#' tagging results will be stored as NDJSON files.
#' 
#' If `inmemory` is `FALSE`, temporary files are used for input and output to
#' the CoreNLP. If you choose inmemory processing (`TRUE`), you may see a
#' warning beginning with "An illegal reflective access operation has occurred".
#' There will be subsequen messages on the operation of threads. Somewhat
#' against intuition, setting `inmemory` as `FALSE` yields better performance
#' and by circumventing the bottleneck of passing data between R and the Java
#' Virtual Machine directly. Using the temporary files does not provoke a
#' warning and is assumed to be more robust.
#' 
#' @param x Either a `data.table` (required to have the columns 'doc_id' and
#'   'text'), or a character vector with input file(s), or a directory. If
#'   `input` is a directory, all files in the directory are processed. Files are
#'   assumed to be tsv files with two columns ('doc_id' and 'text').
#' @param pipe A Pipe object or a properties file to configure annotator.
#' @param threads An `integer` value.
#' @param corenlp_dir The directory where corenlp resides.
#' @param preclean Logical, whether to preprocess string.
#' @param purge A `logical` value, whether to preprocess input.
#' @param byline Logical, whether to process files in a line-by-line manner.
#' @param output_format The output generated, either "json" (default), "txt", or
#'   "xml".
#' @param inmemory If `TRUE`, documents are processed in-memory using
#'   `AnnotationPipeline$annotate()`, if `FALSE`, documents written to disk
#'   temporarily are used as input for `StanfordCoreNLP$process_files()`. See
#'   details on a warning you may see when opting for the inmemory option and on
#'   performance.
#' @param progress Logical, whether to show progress bar.
#' @param verbose Logical, whether to output messages.
#' @param ... Further arguments.
#' @return The target files will be returned, so that they can serve as input to
#'   `corenlp_parse_ndjson`.
#' @importFrom pbapply pblapply
#' @importFrom parallel mclapply
#' @importFrom data.table data.table rbindlist fread fwrite uniqueN is.data.table
#' @importFrom R6 R6Class
#' @importFrom stats setNames
#' @rdname corenlp_annotate
#' @examples 
#' library(data.table)
#' reuters_txt <- readLines(system.file(package = "bignlp", "extdata", "txt", "reuters.txt"))
#' reuters_dt <- data.table(doc_id = 1L:length(reuters_txt), text = reuters_txt)
#' 
#' props <- corenlp_get_properties_file(lang = "en", fast = "TRUE")
#' y <- corenlp_annotate(
#'   x = reuters_dt,
#'   pipe = props,
#'   corenlp_dir = corenlp_get_jar_dir(),
#'   progress = FALSE
#' )
#' 
#' y <- corenlp_annotate(
#'   x = reuters_dt,
#'   pipe = props,
#'   threads = TRUE,
#'   corenlp_dir = corenlp_get_jar_dir(),
#'   progress = FALSE
#' )
#' @export corenlp_annotate
#' @include bignlp-package.R
setGeneric("corenlp_annotate", function(x, ...) standardGeneric("corenlp_annotate"))


#' @importFrom data.table setnames
#' @rdname corenlp_annotate
setMethod("corenlp_annotate", "data.table", function(x, corenlp_dir = getOption("bignlp.corenlp_dir"), pipe, purge = TRUE, threads = 1L, inmemory = TRUE, progress = TRUE,  verbose = TRUE){
  stopifnot(c("doc_id", "text") %in% colnames(x))
  
  Annotator <- if (inherits(pipe, "AnnotationPipeline")){
    pipe
  } else {
    StanfordCoreNLP$new(output_format = "conll", corenlp_dir = corenlp_dir, properties = pipe)
  }
  
  if (threads == 1L){
    if (progress) pb <- txtProgressBar(min = 0, max = uniqueN(x[["doc_id"]]), style = 3)
    retval <- x[, {if (progress) setTxtProgressBar(pb, value = .GRP); Annotator$process(.SD[["text"]], purge = purge)}, by = "doc_id"]
    
    if (Annotator$output_format == "xml")
      setnames(retval, old = "V1", new = "xml")
    
    if (progress) close(pb)
  } else {
    if (isTRUE(inmemory)){
      annolist <- AnnotationList$new(x[["text"]])
      Annotator$annotate(annolist, verbose = verbose)
      if (verbose) message("... get results")
      annotated <- lapply(
        0L:(annolist$obj$size() - 1L),
        function(i) Annotator$outputter$print(annolist$obj$get(i))
      )
      if (verbose) message("... parse conll")
      if (Annotator$output_format == "conll"){
        retval <- rbindlist(
          mclapply(
            seq_along(annotated),
            function(i)
              corenlp_parse_conll(annotated[[i]])[, "doc_id" := x[["doc_id"]][i]],
            mc.cores = threads
          )
        )
      } else if (Annotator$output_format == "xml"){
        retval <- data.table(
          doc_id = seq_along(annotated),
          xml = annotated
        )
      } else {
        stop(sprintf("'%s' outputtter not supported", Annotator$output_format))
      }
      if (verbose) message("... finished")
    } else if (isFALSE(inmemory)){
      if (verbose) message("... write documents to disk")
      Annotator$verbose(x = FALSE)
      chunkdir <- file.path(tempdir(), "chunks")
      if (!dir.exists(chunkdir)) dir.create(path = chunkdir)
      debris <- list.files(chunkdir, full.names = TRUE)
      if (length(debris) >= 0L) unlink(debris, recursive = TRUE)
      segdirs <- segment(
        x = x, dir = chunkdir, chunksize = 40L,
        progress = FALSE, purge = purge
      )
      
      if (verbose) message("... process documents")
      fnames <- lapply(segdirs, Annotator$process_files)
      while(length(Sys.glob(sprintf("%s/*.%s", segdirs, Annotator$output_format))) < nrow(x)){
        Sys.sleep(0.1)
      }
      
      if (Annotator$output_format == "conll"){
        if (verbose) message("... parse CoNLL files")
        retval <- corenlp_parse_conll(fnames, progress = FALSE, threads = threads)
      } else if (Annotator$output_format == "xml"){
        retval <- data.table(
          doc_id = seq_along(unlist(fnames)),
          xml = sapply(
            unlist(fnames),
            function(fname) paste(readLines(fname), collapse = "\n")
          )
        )
      } else {
        stop(sprintf("output format '%s' not supported", Annotator$output_format))
      }

      if (verbose) message("... remove temporary files")
      unlink(x = chunkdir, recursive = TRUE)
    }
  }
  retval
})

#' @rdname corenlp_annotate
#' @examples
#' xml_files <- list.files(system.file(package = "bignlp", "extdata", "xml"))
setMethod("corenlp_annotate", "character", function(x, corenlp_dir = getOption("bignlp.corenlp_dir"), pipe, byline = NULL, output_format = "json", threads = 1L, progress = TRUE,  preclean = TRUE, verbose = TRUE){
  stop("not implemented")
})


#' @details If `x` is an `xml_document`, text nodes of the input XML document
#'   will be replaced by the annotated content of the text nodes. Note that this
#'   is an in-place operation, i.e. the input XML document will be changed.
#' @param xpath An XPath expression for looking up nodes with text that shall be
#'   annotated.
#' @param cols Columns of the parsed CoNLL output of annotation to be kept.
#' @param sentences A `logical` value - whether to wrap annotated
#'   sentences in s tags.
#' @param ne A `logical` value, whether to turn column 'ner' into XML annotation 
#'   of named entities.
#' @param fmt A format string for styling the output of applying the annotation
#'   pipeline. Defaults to a concatenating annotations seperated by tabs. Can be
#'   used to generate XML output.
#' @param opts Passed into `read_xml()` as argument `options`. Defaults to a 
#'   options that make XML processing as robust as possible.
#' @param xsl An XSLT style sheet parsed as `xml_document` that will be applied
#'   before assembling the final document. Useful for transforming CoreNLP XML
#'   output efficiently to a lean target format.
#' @rdname corenlp_annotate
#' @importFrom xml2 read_xml xml_find_all xml_text xml_set_text xml_add_child
#'   xml_text<- write_xml
#' @importFrom xml2 xml_find_first read_html xml_name xml_replace
#' @importFrom cli cli_alert_info cli_progress_step
#' @importFrom xslt xml_xslt
#' @examples
#' xml_dir <- system.file(package = "bignlp", "extdata", "xml")
#' xml_files <- list.files(xml_dir, full.names = TRUE)
#' 
#' # Scenario 1: Insert CoNLL output into XML
#' 
#' doc <- xml2::read_xml(x = xml_files[[1]])
#' Pipe <- StanfordCoreNLP$new(
#'   output_format = "conll",
#'   properties = corenlp_get_properties_file(lang = "en", fast = TRUE)
#' )
#' corenlp_annotate(x = doc, pipe = Pipe, sentences = TRUE) # changes inplace
#' 
#' # Write annotated document to disc
#' xml_tmp <- tempfile(fileext = ".xml")
#' xml2::write_xml(x = doc, file = xml_tmp, options = NULL)
#' 
#' # Scenario 2a: Insert CoreNLP XML output into XML (without transformation)
#' 
#' doc <- xml2::read_xml(x = xml_files[[1]]) # read anew
#' Pipe <- StanfordCoreNLP$new(
#'   output_format = "xml", # triggers XML output
#'   properties = corenlp_get_properties_file(lang = "en", fast = TRUE)
#' )
#' corenlp_annotate(x = doc, verbose = TRUE, pipe = Pipe) # inplace!
#' 
#' # Scenario 2b: Insert transformed CoreNLP XML output
#' 
#' stylesheet <- xml2::read_xml(
#'   x = system.file(package = "bignlp", "extdata", "xsl", "sample.xsl")
#' )
#' doc <- xml2::read_xml(x = xml_files[[1]]) # read anew
#' corenlp_annotate(
#'   x = doc,
#'   pipe = Pipe,
#'   xpath = "//p",
#'   xsl = stylesheet,
#'   verbose = TRUE
#' )
setMethod("corenlp_annotate", "xml_document", function(x, xpath = "//p", pipe, threads = 1L, cols = c("word", "lemma", "pos"), fmt = paste(rep("%s", times = length(cols)), collapse = "\t"), sentences = TRUE, ne = FALSE, xsl = NULL, inmemory = FALSE, purge = TRUE, opts =  c("RECOVER", "NOERROR", "NOBLANKS", "HUGE"), verbose = TRUE, progress = FALSE){
  
  text_nodes <- xml2::xml_find_all(x = x, xpath)
  text_nodes_text <- xml_text(text_nodes)
  if (verbose)
    cli_alert("found {.emphf {length(text_nodes)}} text node{?s} to process")

  if (isTRUE(purge)){
    if (verbose) cli_progress_step("denoise text")
    text_nodes_text <- sapply(
      text_nodes_text,
      purge, replacements = corenlp_preprocessing_replacements,
      progress = FALSE
    )
  }
  
  # Purge first, because nodes may be empty only after purge
  empty_nodes <- grep("^\\s*$", text_nodes_text)
  if (length(empty_nodes) > 0L){
    cli_alert_info("remove {length(empty_nodes)} empty text nodes")
    for (i in rev(empty_nodes)) xml2::xml_remove(text_nodes[[i]])
    text_nodes <- xml2::xml_find_all(x = x, xpath = xpath)
    text_nodes_text <- sapply(text_nodes, xml_text)
    if (isTRUE(purge)){
      text_nodes_text <- sapply(
        text_nodes_text,
        purge,
        replacements = corenlp_preprocessing_replacements,
        progress = FALSE
      )
    }
    empty_nodes <- grep("^\\s*$", text_nodes_text)
    if (length(empty_nodes) > 0L) stop("non-empty node which should be gone!")
  }
  
  if (pipe$output_format == "conll"){
    if (verbose) cli_progress_step("run annotation pipeline (CoNLL Outputter)")
    dt <- corenlp_annotate(
      x = data.table(doc_id = 1L:length(text_nodes), text = text_nodes_text),
      pipe = pipe,
      threads = threads, inmemory = inmemory,
      purge = FALSE, # we've done this already
      verbose = FALSE, progress = progress
    )
    
    tokenline <- do.call(sprintf, c(fmt, lapply(cols, function(col) dt[[col]])))
    
    if (ne){
      if (verbose) cli_progress_step("wrap named entities into XML tags")
      ne <- ifelse(dt[["ner"]] == "O", NA, dt[["ner"]])
      ne_begin <- ifelse(!is.na(ne), sprintf('<ne type="%s">\n', dt[["ner"]]), "")
      ne_end <- ifelse(!is.na(ne), "\n</ne>\n", "")
      multiline_ne <- c(ifelse(ne[2:length(ne)] == ne[1:(length(ne) - 1)], TRUE, FALSE), FALSE)
      multiline_ne <- ifelse(is.na(multiline_ne), FALSE, multiline_ne)
      
      # Next line is new sentence? Keep closing tag
      multiline_ne <- ifelse(c(dt[["idx"]][2:nrow(dt)], 1L) == 1L, FALSE, multiline_ne)
      ne_end <- ifelse(multiline_ne, "", ne_end)
      ne_begin <- ifelse(c(FALSE, multiline_ne[1:(nrow(dt) - 1L)]), "", ne_begin)
      tokenline <-  sprintf("%s%s%s", ne_begin, tokenline, ne_end)
    }
    
    
    if (sentences){
      if (verbose) cli_progress_step("wrap sentences into XML tags")
      s_begin <- ifelse(dt[["idx"]] == 1L, "<s>\n", "")
      s_end <- ifelse(c(dt[["idx"]][2:nrow(dt)], 1L) == 1L, "\n</s>", "")
      tokenline <-sprintf("%s%s%s", s_begin, tokenline, s_end)
    }
    
    dt[, "tokenline" := tokenline]
    dt_docs <- split(dt, f = dt[["doc_id"]])
    
    if (verbose) cli_progress_step("transform annotated content")
    newnodes <- mclapply(
      seq_along(dt_docs),
      function(i){
        el <- xml_name(text_nodes[[i]])
        sprintf(
          "<%s>\n%s\n</%s>",
          el, paste(dt_docs[[i]][["tokenline"]], collapse = "\n"), el
        )
      },
      mc.cores = threads
    )
    
  } else if (pipe$output_format == "xml"){
    if (verbose) cli_progress_step("run annotation pipeline (XML Outputter)")
    dt <- corenlp_annotate(
      x = data.table(doc_id = seq_along(text_nodes), text = text_nodes_text),
      pipe = pipe,
      threads = threads, inmemory = inmemory,
      purge = FALSE, # we've done this already
      verbose = FALSE, progress = progress
    )
    
    if (verbose) cli_progress_step("transform annotated content")
    nodes <- sapply(text_nodes, xml_name)
    docs <- gsub("</document>.*?$", "", gsub("^.*?<document>", "", dt[["xml"]]))
    newnodes <- sprintf("<%s>%s</%s>", nodes, docs, nodes)
  } else if (pipe$output_format == "json"){
    stop("`corenlp_annotate()` not implemented for json outputter")
  }
  
  if (verbose) cli_progress_step("insert annotated content into XML")
  xml_doc_char <- sprintf(
    "<root>%s</root>",
    paste(unlist(newnodes), collapse = "\n")
  )
  xml_doc_tmp <- read_xml(
    x = charToRaw(enc2utf8(xml_doc_char)),
    encoding = "UTF-8",
    as_html = FALSE,
    options = opts
  )
  
  if (!is.null(xsl)){
    if (verbose) cli_progress_step("transformation using XSL")
    xml_doc_tmp <- xslt::xml_xslt(doc = xml_doc_tmp, stylesheet = xsl)
  }

  new_nodes <- xml_find_all(xml_doc_tmp, xpath = xpath)

  dummy <- mapply(xml_replace, text_nodes, new_nodes)
  
  return( invisible(x) )
})
