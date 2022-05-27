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
      if (verbose) message("... get results")
      conll_output <- lapply(
        0L:(annolist$obj$size() - 1L),
        function(i)conll_outputter$print(annolist$obj$get(i))
      )
      if (verbose) message("... parse conll")
      retval <- rbindlist(
        mclapply(
          1L:length(conll_output),
          function(i)
            corenlp_parse_conll(conll_output[[i]])[, "doc_id" := x[["doc_id"]][i]],
          mc.cores = threads
        )
      )
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
      conll_files <- lapply(segdirs, Annotator$process_files)
      while(length(Sys.glob(sprintf("%s/*.conll", segdirs))) < nrow(x)){
        Sys.sleep(0.1)
      }
      
      if (verbose) message("... parse CoNLL files")
      retval <- corenlp_parse_conll(
        conll_files,
        progress = FALSE, threads = threads
      )

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


#' @param xpath An XPath expression for looking up nodes with text that shall be
#'   annotated.
#' @param cols Columns of the parsed CoNLL output of annotation to be kept.
#' @param sentences A `logical` value - whether to wrap annotated annotated
#'   sentences in s element.
#' @param ne A `logical` value, whether to turn column 'ner' into XML annotation 
#'   of named entities.
#' @rdname corenlp_annotate
#' @importFrom xml2 read_xml xml_find_all xml_text xml_set_text xml_add_child xml_text<- write_xml
#' @importFrom xml2 xml_find_first read_html
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
setMethod("corenlp_annotate", "xml_document", function(x, xpath = "//p", pipe, threads = 1L, cols = c("word", "lemma", "pos"), sentences = TRUE, ne = FALSE, inmemory = FALSE, purge = TRUE, verbose = TRUE, progress = FALSE){
  
  if (ne){
    if (isFALSE("ner" %in% cols))
      stop("Argument 'ne' is TRUE but 'ner' is not a column stated in argument 'cols'.")
    
    if (grep("ner", cols) == 1L)
      stop("column 'ner' may not be first column that is stated.")
    
    ne_types <- c("PERSON", "LOCATION", "ORGANIZATION", "MISC")
    ne_regex <- sprintf(
      c("\n(%s)(%s)(%s)", "\n(%s)(%s|O)(%s)"), 
      paste(rep(x =  ".*?\t", times = grep("ner", cols) - 1L), collapse = ""),
      paste(ne_types, collapse = "|"),
      if (grep("ner", cols) < length(cols)){
        paste("\t", paste(rep(x =  ".*?", times = length(cols) - grep("ner", cols)), collapse = "\t"), sep ="")
      } else {
        ""
      }
    )
  }
  
  if (verbose) message("... get text nodes ", appendLF = FALSE)
  text_nodes <- xml2::xml_find_all(x = x, xpath)
  if (verbose) message(sprintf("(%d)", length(text_nodes)))
  text_nodes_text <- sapply(text_nodes, xml_text)
  
  if (verbose) message("... preprocessing text", appendLF = TRUE)
  if (isTRUE(purge)){
    text_nodes_text <- sapply(
      text_nodes_text,
      function(txt)
        purge(txt, replacements = corenlp_preprocessing_replacements, progress = FALSE)
    )
  }
  
  if (verbose) message("... remove empty text nodes", appendLF = TRUE)
  empty_nodes <- grep("^\\s*$", text_nodes_text)
  if (length(empty_nodes) > 0L){
    for (i in rev(empty_nodes)) xml2::xml_remove(text_nodes[[i]])
    text_nodes <- xml2::xml_find_all(x = x, xpath = xpath)
    text_nodes_text <- sapply(text_nodes, xml_text)
    if (isTRUE(purge)){
      text_nodes_text <- sapply(
        text_nodes_text,
        function(txt)
          purge(txt, replacements = corenlp_preprocessing_replacements, progress = FALSE)
      )
    }
    empty_nodes <- grep("^\\s*$", text_nodes_text)
    if (length(empty_nodes) > 0L) stop("non-empty node which should be gone!")
  }

  dt <- data.table(doc_id = 1L:length(text_nodes), text = text_nodes_text)
  dt_annotated <- corenlp_annotate(
    x = dt, pipe = pipe,
    threads = threads, inmemory = inmemory,
    purge = purge,
    verbose = verbose, progress = progress
  )
  
  fmt <- paste(rep("%s", times = length(cols)), collapse = "\t")
  tokenline <- do.call(
    sprintf,
    c(fmt, lapply(cols, function(col) dt_annotated[[col]]))
  )
  dt_annotated[, "tokenline" := tokenline]
  dt_docs <- split(dt_annotated, f = dt_annotated[["doc_id"]])
  if (isFALSE(sentences)){
    if (verbose) message("... enhance XML")
    if (ne) warning("sentences is FALSE, but `ne` is TRUE - `ne` skipped")
    lapply(
      dt_docs,
      function(dt){
        xml_set_text(
          x = text_nodes[[dt[["doc_id"]][1]]],
          value = sprintf(
            "\n%s\n",
            paste(dt_docs[["tokenline"]], collapse = "\n")
          )
        )
        invisible(NULL)
      }
    )
  } else {
    
    if (verbose) message("... generate sentence annotation ", appendLF = TRUE)
    chunks <- mclapply(
      dt_docs,
      function(dt){
        if (nrow(dt) > 1L){
          f <- cut(
            1L:nrow(dt),
            unique(c(which(dt[, "idx"] == 1L), nrow(dt))),
            include.lowest = TRUE, right = FALSE
          )
          s_list <- split(x = dt, f = f)
          lapply(
            s_list,
            function(s){
              sprintf(
                "\n<s>\n%s\n</s>\n",
                paste(s[["tokenline"]], collapse = "\n")
              )
            }
          )
        } else {
          sprintf("\n<s>\n%s\n</s>\n", dt[["tokenline"]])
        }
      },
      mc.cores = threads
    )

    if (verbose) message("... generate named entity annotation")
    if (isTRUE(ne)){
      chunks <- mclapply(
        chunks,
        function(chunk){
          s2 <- gsub(
            ne_regex[1],
            '\n<ne type="\\2">\n\\1\\2\\3\n</ne type="\\2">',
            chunk,
            perl = TRUE
          )
          # Remove closing and opening tag of the same type so that one multi-word ne is wrapped
          # into one element
          s3 <- gsub('</ne\\stype="(.*?)">\n<ne\\stype="\\1">\n', "", s2, perl = TRUE)
          s4 <- gsub('</ne\\stype=".*?">', "</ne>", s3)
          s5 <- gsub(ne_regex[2], '\n\\1\\3', s4, perl = TRUE) # Remove ner column
          paste(s5, collapse = "\n")
        },
        mc.cores = threads
      )
    }

    if (verbose) message("... insert annotation into XML document")
    foo <- lapply(
      seq_along(chunks),
      function(i){
        element <- xml_name(text_nodes[[i]])
        x <- sprintf("<%s>\n%s\n</%s>", element, chunks[[i]], element)
        doc <- read_xml(
          x = charToRaw(enc2utf8(x)),
          encoding = "UTF-8",
          as_html = FALSE,
          options = c("RECOVER", "NOERROR", "NOBLANKS")
        )
        # doc <- read_xml(xml, options = )
        xml_replace(.x = text_nodes[[i]], .value = doc)
      }
    )
  }
  invisible(x)
})
