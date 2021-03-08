#' Annotate a string.
#' 
#' Use CoreNLP to annotate strings.
#' 
#' If argument \code{threads} is 1, the tagging result is returned, if output is NULL.
#' If \code{threads} is higher than 1, \code{output} should be a directory where tagging
#' results will be stored as NDJSON files.
#' 
#' @param x Either a \code{data.table} (required to have the columns 'doc_id' and
#'   'text'), or a character vector with input file(s), or a directory. If
#'   \code{input} is a directory, all files in the directory are processed. Files
#'   are assumed to be tsv files with two columns ('doc_id' and 'text').
#' @param pipe A Pipe object or a properties file to configure annotator.
#' @param threads An integer value.
#' @param corenlp_dir The directory where corenlp resides.
#' @param preclean Logical, whether to preprocess string.
#' @param purge A `logical` value, whether to preprocess input.
#' @param byline Logical, whether to process files in a line-by-line manner.
#' @param output_format The output generated, either "json" (default), "txt", or "xml".
#' @param inmemory If `TRUE`, documents are processed in-memory using
#'   `AnnotationPipeline$annotate()`, if `FALSE`, documents written to disk
#'   temporarily are used as input for `StanfordCoreNLP$process_files()`.
#' @param progress Logical, whether to show progress bar.
#' @param verbose Logical, whether to output messages.
#' @param ... Further arguments.
#' @return The target files will be returned, so that they can serve as input to
#'   \code{corenlp_parse_ndjson}.
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
    if (progress) close(pb)
  } else {
    if (isTRUE(inmemory)){
      annolist <- AnnotationList$new(x[["text"]])
      Annotator$annotate(annolist, verbose = verbose)
      conll_outputter <- rJava::.jnew("edu.stanford.nlp.pipeline.CoNLLOutputter")
      retval <- rbindlist(
        lapply(
          0L:(annolist$obj$size() - 1L),
          function(i){
            corenlp_parse_conll(
              conll_outputter$print(annolist$obj$get(i))
            )[, "doc_id" := x[["doc_id"]][i + 1L]]
          }
        )
      )
    } else if (isFALSE(inmemory)){
      if (verbose) message("... create empty chunkdir")
      Annotator$verbose(x = FALSE)
      chunkdir <- file.path(tempdir(), "chunks")
      if (!dir.exists(chunkdir)) dir.create(path = chunkdir)
      debris <- list.files(chunkdir, full.names = TRUE)
      if (length(debris) >= 0L) unlink(debris, recursive = TRUE)
      
      if (verbose) message("... segment")
      segdirs <- segment(x = x, dir = chunkdir, chunksize = 40L, progress = FALSE)
      
      if (verbose) message("... process files")
      conll_files <- lapply(segdirs, Annotator$process_files)
      Sys.sleep(0.5)
      
      if (verbose) message("... parse CoNLL")
      retval <- corenlp_parse_conll(conll_files, progress = FALSE, threads = threads)

      if (verbose) message("... cleaning up")
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


#' @param xpath An XPath expression for looking up nodes with text that shall be
#'   annotated.
#' @param cols Columns of the parsed CoNLL output of annotation to be kept.
#' @param sentences A `logical` value - whether to wrap annotated annotated
#'   sentences in s element.
#' @rdname corenlp_annotate
#' @importFrom xml2 read_xml xml_find_all xml_text xml_set_text xml_add_child xml_text<- write_xml
#' @examples
#' xml_dir <- system.file(package = "bignlp", "extdata", "xml")
#' xml_files <- list.files(xml_dir, full.names = TRUE)
#' xml_doc <- xml2::read_xml(x = xml_files[[1]])
#' 
#' Pipe <- StanfordCoreNLP$new(
#'   output_format = "conll",
#'   properties = corenlp_get_properties_file(lang = "en", fast = TRUE)
#' )
#' 
#' xml_doc2 <- corenlp_annotate(x = xml_doc, pipe = Pipe, sentences = TRUE)
#' 
#' # Write annotated document to disc
#' y <- tempfile(fileext = ".xml")
#' xml2::write_xml(x = xml_doc, file = y, options = NULL)
setMethod("corenlp_annotate", "xml_document", function(x, xpath = "//p", pipe, threads = 1L, cols = c("word", "lemma", "pos"), sentences = TRUE, inmemory = FALSE, verbose = TRUE, progress = FALSE){
  if (verbose) message("... get text nodes")
  text_nodes <- xml2::xml_find_all(x = x, xpath)
  dt <- data.table(doc_id = 1L:length(text_nodes), text = sapply(text_nodes, xml_text))
  
  if (verbose) message("... annotate text")
  dt_annotated <- corenlp_annotate(x = dt, pipe = pipe, threads = threads, inmemory = inmemory, verbose = verbose, progress = progress)
  
  if (verbose) message("... enhance xml")
  dt_docs <- split(dt_annotated, f = dt_annotated[["doc_id"]])
  if (isFALSE(sentences)){
    lapply(
      dt_docs,
      function(dt){
        node_id <- unique(dt[["doc_id"]])
        m <- as.matrix(dt[, cols, with = FALSE])
        txt <- paste(apply(m, 1, function(row) paste(row, collapse = "\t")), collapse = "\n")
        xml_set_text(x = text_nodes[[ node_id ]], value = sprintf("\n%s\n", txt))
        invisible(NULL)
      }
    )
  } else {
    if (verbose) message("... creating content")
    sentences_list <- mclapply(
      dt_docs,
      function(dt){
        node_id <- unique(dt[["doc_id"]])
        if (nrow(dt) > 1L){
          f <- cut(
            1L:nrow(dt),
            unique(c(which(dt[, "idx"] == 1L), nrow(dt))),
            include.lowest = TRUE, right = FALSE
          )
          sentences <- split(x = dt, f = f)
          lapply(
            sentences,
            function(s){
              txt <- paste(apply(as.matrix(s[, cols, with = FALSE]), 1, function(row) paste(row, collapse = "\t")), collapse = "\n")
              sprintf("\n%s\n", txt)
            }
          )
        } else {
          sprintf("\n%s\n", paste(unlist(dt[, cols, with = FALSE]), collapse = "\t"))
        }
      },
      mc.cores = threads
    )
    
    if (verbose) message("... creating nodes")
    lapply(
      1L:length(sentences_list),
      function(i){
        lapply(sentences_list[[i]], function(s) xml_add_child(.x = text_nodes[[i]], .value = "s", s))
        xml_text(text_nodes[[i]]) <- ""
        invisible(NULL)
      }
    )
  }
  invisible(x)
})
