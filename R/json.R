#' @title Parse CoreNLP json string output.
#' @description Parse the json output from CoreNLP, either text files, or 
#' NDJSON written to one or multiple files.
#' @param input character vector, the JSON string(s) to be parsed
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
#' @rdname corenlp_json
corenlp_parse_json = function(input, cols_to_keep = c("sentence", "index", "word", "pos", "ner"), output = NULL, logfile = NULL, progress = TRUE){
  if (length(input) == 1L){
    # run the parsing within try - coding issues may cause problems
    json_parsed <- try( jsonlite::fromJSON(input) )
    if (class(json_parsed)[1] == "try-error"){
      warning("Cannot parse character vector: ", input)
      if (!is.null(logfile)) cat(input, file = logfile, append = TRUE)
      return( NULL )
    }
    
    # to cope with '{"chunk": 2859285,  "sentences": [ ] }'
    if (length(json_parsed$sentences$tokens) == 0L){
      warning("JSON string without tokens: ", input)
      if (!is.null(logfile)) cat(input, file = logfile, append = TRUE)
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
    df <- do.call(rbind, dfs)
    
    # add chunk number, if present, and 
    if ("id" %in% names(json_parsed)){
      df[["id"]] <- json_parsed[["id"]]
      cols <- c("id", cols_to_keep)
    } else {
      cols <- cols_to_keep
    }
    y <- df[, cols]
    
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
  } else if (length(input) > 1L){
    .parse <- function(line) corenlp_parse_json(line, cols_to_keep = cols_to_keep, output = output, logfile = logfile, progress = FALSE)
    dfs <- if (progress) pblapply(input, .parse) else lapply(input, .parse)
    if (is.null(output)) return( do.call(rbind, dfs) ) else return( invisible( NULL ) )
  }
}
