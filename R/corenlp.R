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
#' @param properties A properties file to configure annotator.
#' @param threads An integer value.
#' @param corenlp_dir The directory where corenlp resides.
#' @param preclean Logical, whether to preprocess string.
#' @param purge A `logical` value, whether to preprocess input.
#' @param byline Logical, whether to process files in a line-by-line manner.
#' @param output_format The output generated, either "json" (default), "txt", or "xml".
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
#'   properties = props,
#'   corenlp_dir = corenlp_get_jar_dir(),
#'   progress = FALSE
#' )
#' @export corenlp_annotate
#' @include bignlp-package.R
setGeneric("corenlp_annotate", function(x, ...) standardGeneric("corenlp_annotate"))


#' @rdname corenlp_annotate
setMethod("corenlp_annotate", "data.table", function(x, corenlp_dir = getOption("bignlp.corenlp_dir"), properties, purge = TRUE, threads = 1L, progress = TRUE,  verbose = TRUE){
  stopifnot(c("doc_id", "text") %in% colnames(x))
  Annotator <- StanfordCoreNLP$new(output_format = "conll", corenlp_dir = corenlp_dir, properties = properties)
  if (progress) pb <- txtProgressBar(min = 0, max = uniqueN(x[["doc_id"]]), style = 3)
  retval <- x[, {if (progress) setTxtProgressBar(pb, value = .GRP); Annotator$process(.SD[["text"]], purge = purge)}, by = "doc_id"]
  if (progress) close(pb)
  retval
})

#' @rdname corenlp_annotate
#' @examples
#' xml_files <- list.files(system.file(package = "bignlp", "extdata", "xml"))
setMethod("corenlp_annotate", "character", function(x, corenlp_dir = getOption("bignlp.corenlp_dir"), properties, byline = NULL, output_format = "json", threads = 1L, progress = TRUE,  preclean = TRUE, verbose = TRUE){
  stop("not implemented")
})


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
setMethod("corenlp_annotate", "xml_document", function(x, xpath = "//p", pipe, cols = c("word", "lemma", "pos"), sentences = TRUE, verbose = TRUE){
  text_nodes <- xml2::xml_find_all(x = x, xpath)
  text <- lapply(text_nodes, xml_text)
  annolist <- AnnotationList$new(text)
  
  pipe$annotate(annolist, verbose = verbose)
  
  conll_outputter <- rJava::.jnew("edu.stanford.nlp.pipeline.CoNLLOutputter")
  lapply(
    0L:(annolist$obj$size() - 1L),
    function(i){
      tab <- read.table(
        text = conll_outputter$print(annolist$obj$get(i)),
        blank.lines.skip = TRUE, header = FALSE, sep = "\t", quote = ""
      )
      colnames(tab) <- c("idx", "word", "lemma", "pos", "ner", "headidx", "deprel")
      if (isFALSE(sentences)){
        m <- as.matrix(tab[, cols])
        txt <- paste(apply(m, 1, function(row) paste(row, collapse = "\t")), collapse = "\n")
        xml_set_text(x = text_nodes[[i + 1L]], value = sprintf("\n%s\n", txt))
      } else {
        if (nrow(tab) > 1L){
          f <- cut(
            1L:nrow(tab),
            unique(c(which(tab[, "idx"] == 1L), nrow(tab))),
            include.lowest = TRUE, right = FALSE
          )
          sentences <- split(x = tab, f = f)
          lapply(
            sentences,
            function(s){
              txt <- paste(apply(as.matrix(s[, cols]), 1, function(row) paste(row, collapse = "\t")), collapse = "\n")
              xml_add_child(.x = text_nodes[[i + 1L]], .value = "s", sprintf("\n%s\n", txt))
            }
          )
          xml_text(text_nodes[[i + 1L]]) <- ""
        } else {
          xml_add_child(
          .x = text_nodes[[i + 1L]],
          .value = "s",
          sprintf("\n%s\n", paste(unlist(tab[, cols]), collapse = "\t"))
        )
          xml_text(text_nodes[[i + 1L]]) <- ""
        }
      }
      invisible(NULL)
    }
  )
  x
})
