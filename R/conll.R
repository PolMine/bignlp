#' @title Parse CoreNLP CoNLL string output.
#' @description Parse the CoNLL output from CoreNLP in text files
#' @details `corenlp_parse_conll` is based on `data.table::fread()`
#' and supplies settings that prevent undesired behaviour.
#' @param x A filename, or filenames.
#' @param progress logical 
#' @importFrom data.table fread rbindlist
#' @importFrom jsonlite fromJSON
#' @export corenlp_parse_conll
#' @rdname corenlp_parse_conll
corenlp_parse_conll = function(x, progress = TRUE){
  if (length(x) == 1L){
    return(fread(x, na.strings = NULL, blank.lines.skip = TRUE, quote = ""))
  } else if (length(x) > 1L){
    .parse <- function(f) corenlp_parse_conll(f, progress = FALSE)
    dts <- if (progress) pblapply(x, .parse) else lapply(x, .parse)
    return(rbindlist(dts))
  }
}
