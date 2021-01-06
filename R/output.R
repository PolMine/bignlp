#' @title Parse CoreNLP output formats.
#' @description Parse output from CoreNLP.
#' @param x character vector, the JSON string(s) to be parsed
#' @param cols_to_keep columns to keep
#' @param output a destfile
#' @param logfile a character string naming the file to an error log to; if
#'   provided, json strings will be written to this file if parsing the json
#'   string string fails
#' @param progress logical 
#' @details The JSON results of applying the Stanford CoreNLP annotators can be
#'   written to a streaming JSON file (ndjson format). \code{corenlp_parse_json}
#'   will parse a json string to a data.frame. If output is specified, the
#'   output will be appended to the file provided. If \code{output} is \code{NULL}, a
#'   data.frame is returned. Strings that cannot be parsed are written to the
#'   logfile, if it is defined. If \code{filename} is present, the function will
#'   process one or more files with the output of Stanford CoreNLP in a NDJSON
#'   format. If the argument \code{output} has been defined during initialization, results are
#'   written/appended to that file. Otherwise, a \code{data.frame} is returned.
#' @importFrom data.table fread rbindlist as.data.table setkeyv setorderv
#' @importFrom jsonlite fromJSON
#' @importFrom utils write.table
#' @export corenlp_parse_json
#' @rdname parse
corenlp_parse_json = function(x, cols_to_keep = c("sentence", "index", "word", "pos", "ner"), output = NULL, logfile = NULL, progress = TRUE){
  if (length(x) == 1L){
    # run the parsing within try - coding issues may cause problems
    json_parsed <- try( jsonlite::fromJSON(x) )
    if (class(json_parsed)[1] == "try-error"){
      warning("Cannot parse character vector: ", x)
      if (!is.null(logfile)) cat(x, file = logfile, append = TRUE)
      return( NULL )
    }
    
    # to cope with '{"chunk": 2859285,  "sentences": [ ] }'
    if (length(json_parsed$sentences$tokens) == 0L){
      warning("JSON string without tokens: ", x)
      if (!is.null(logfile)) cat(x, file = logfile, append = TRUE)
      return( NULL )
    }
    
    dfs <- lapply(
      1L:length(json_parsed$sentences$tokens),
      function(i){
        if (ncol(json_parsed$sentences$tokens[[i]]) > 0){
          return( data.frame(sentence = i, json_parsed$sentences$tokens[[i]]))
        } else {
          return( NULL )
        }
      }
    )
    y <- do.call(rbind, dfs)
    
    # output
    if (!is.null(output)){
      write.table(
        y, file = output,
        sep = "\t",
        append = if (file.exists(output)) TRUE else FALSE,
        row.names = FALSE,
        col.names = if (file.exists(output)) FALSE else TRUE
      )
      return( NULL )
    } else {
      return( y )
    }
  } else if (length(x) > 1L){
    .parse <- function(line) corenlp_parse_json(line, cols_to_keep = cols_to_keep, output = output, logfile = logfile, progress = FALSE)
    dfs <- if (progress) pblapply(x, .parse) else lapply(x, .parse)
    if (is.null(output)) return( do.call(rbind, dfs) ) else return( invisible( NULL ) )
  }
}


#' @details `corenlp_parse_conll` is based on `data.table::fread()`
#' and supplies settings that prevent undesired behaviour.
#' @param x A filename, or filenames.
#' @param progress logical 
#' @importFrom data.table fread rbindlist
#' @importFrom jsonlite fromJSON
#' @export corenlp_parse_conll
#' @rdname parse
corenlp_parse_conll = function(x, progress = TRUE){
  if (length(x) == 1L){
    return(fread(x, na.strings = NULL, blank.lines.skip = TRUE, quote = ""))
  } else if (length(x) > 1L){
    .parse <- function(f) corenlp_parse_conll(f, progress = FALSE)
    dts <- if (progress) pblapply(x, .parse) else lapply(x, .parse)
    return(rbindlist(dts))
  }
}

#' @rdname parse
#' @export corenlp_parse_xml
#' @importFrom coreNLP loadXMLAnnotation getToken
corenlp_parse_xml <- function(x){
  cat(x, file = (xmlfile <- tempfile()))
  a <- loadXMLAnnotation(xmlfile)
  unlink(xmlfile)
  getToken(a)
}
