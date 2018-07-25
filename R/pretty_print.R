#' Parse CoreNLP pretty print output.
#' 
#' The pretty print output of Stanford CoreNLP may be an alternative to the 
#' NDJSON output. The \code{corenlp_parse_pretty_print} is a residual, kept 
#' in the package for future use.
#' 
#' @param x A character vector or filename
#' @param mc Number of cores to use.
#' @export corenlp_parse_pretty_print
#' @importFrom pbapply pblapply
#' @rdname pretty_print
corenlp_parse_pretty_print <- function(x, mc = 1){
  
  if (file.exists(x)){
    if (length(x) > 1) stop("")
    if (file.info(x)[["isdir"]] == FALSE){
      x <- readLines(x)
    } else {
      stop("x is not a single file; processing multiple files is not yet implemented")
    }
  }
  
  chunks <- cut(
    1L:length(x), grep("^Sentence\\s#\\d+", x), length(x),
    include.lowest = TRUE, right = FALSE
  )
  
  pblapply(
    split(x, f = chunks),
    function(chunk){
      txt <- chunk[grepl("^\\[.*\\]$", chunk)] # get lines with annotation
      regex <- "^.*?Text=(.*?)\\s.*\\sPartOfSpeech=(.*?)\\sNamedEntityTag=(.*?)\\]"
      df <- stringi::stri_match(txt, regex = regex)
      dt <- as.data.table(df)[,2L:4L, with = FALSE]
      colnames(dt) <- c("token", "pos", "ner")
      dt
    },
    cl = mc
  )
}
