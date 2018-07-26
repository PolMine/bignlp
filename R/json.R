#' Parse json or ndjson output.
#' 
#' Turn json or ndjson output from Stanford CoreNLP into tabular format
#' 
#' @param x character vector, the JSON string(s) to be parsed
#' @param cols_to_keep columns to keep
#' @param destfile a character string naming the file to write to
#' @param logfile a character string naming the file to an error log to; if
#'   provided, json strings will be written to this file if parsing the json
#'   string string fails
#' @param threads integer
#' @param progress logical 
#' @param verbose logical
#' @return A character vector with the target files.
#' @export corenlp_parse_ndjson
#' @rdname corenlp_json
#' @examples
#' ndjson_file <- system.file(package = "bignlp", "extdata", "ndjson", "reuters_1.ndjson")
#' json_string <- readLines(ndjson_file)
#' dt <- corenlp_parse_json(json_string, progress = FALSE)
#' 
#' dt <- corenlp_parse_ndjson(x = ndjson_file, destfile = NULL, progress = FALSE)
#' dt <- corenlp_parse_ndjson(x = ndjson_file, destfile = NULL, progress = TRUE)
#' 
#' tsv_file <- tempfile()
#' dt <- corenlp_parse_ndjson(x = ndjson_file, destfile = tsv_file, progress = FALSE)
#' y <- data.table::fread(tsv_file)
#' dt <- corenlp_parse_ndjson(x = ndjson_file, destfile = tsv_file, progress = TRUE)
#' y <- data.table::fread(tsv_file)
#' 
#' ndjson_dir <- system.file(package = "bignlp", "extdata", "ndjson")
#' tsv_file <- tempfile()
#' corenlp_parse_ndjson(x = ndjson_dir, destfile = tsv_file, threads = 1L)
#' y <- data.table::fread(tsv_file)
#' tsv_dir <- tempdir()
#' tsv_files <- sprintf("%s/reuters_annotated_%d.tsv", tsv_dir, 1L:4L)
#' corenlp_parse_ndjson(
#'   x = list.files(ndjson_dir, full.names = TRUE),
#'   destfile = tsv_files,
#'   threads = 1L
#'   )
#' dt <- data.table::rbindlist(lapply(tsv_files, data.table::fread))
corenlp_parse_ndjson = function(x, cols_to_keep = c("sentence", "index", "word", "pos", "lemma"), destfile = NULL, logfile = NULL, threads = 1L, progress = TRUE, verbose = TRUE){
  if (file.info(x[1])[["isdir"]]){
    started <- Sys.time()
    ndjson_files <- Sys.glob(sprintf("%s/*.ndjson", x))
    if (length(ndjson_files) == 0L) stop("no ndjson files in directory x")
    message("... number of files to process: ", length(ndjson_files))
    # ensure that filenames are processed in the correct order
    if (all(grepl("\\d+", basename(ndjson_files)))){
      ndjson_files <- ndjson_files[order(as.integer(gsub("^.*?(\\d+).*?$", "\\1", basename(ndjson_files))))]
    }
    if (as.integer(threads) == 1L){
      corenlp_parse_ndjson(x = ndjson_files, cols_to_keep = cols_to_keep, destfile = destfile, logfile = logfile, threads = threads, progress = progress)
    } else {
      ndjson_files_list <- text2vec::split_into(ndjson_files, n = threads)
      tmpdir <- tempdir()
      message("... using temporary directory: ", tmpdir)
      # removing potentially remaining files in tmpdir is necessary, because the NDJSON
      # class would just add new output to an already existing file
      unlink(Sys.glob(sprintf("%s/*.tok", tmpdir)))
      unlink(Sys.glob(sprintf("%s/*.log", tmpdir)))
      dtList <- parallel::mclapply(
        1L:length(ndjson_files_list),
        function(i){
          tmpdestfile <- file.path(tmpdir, paste(i, "tok", sep = "."))
          tmplogfile <- file.path(tmpdir, paste(i, "log", sep = "."))
          
          corenlp_parse_ndjson(x = ndjson_files_list[[i]], cols_to_keep = cols_to_keep, destfile = tmpdestfile, logfile = tmplogfile, threads = 1L, progress = FALSE)
          data.table::fread(tmpdestfile, sep = "\t", header = TRUE, showProgress = FALSE)
        },
        mc.cores = threads
      )
      dt <- data.table::rbindlist(dtList)
      setkeyv(dt, cols = "id")
      setorderv(dt, cols = "id")
      data.table::fwrite(dt, file = destfile, sep = "\t", row.names = FALSE, col.names = TRUE)
      log_list <- lapply(
        file.path(tmpdir, paste(1L:length(ndjson_files_list), "log", sep = ".")),
        function(x) if (file.exists(x)) readLines(x, warn = FALSE) else NULL
      )
      writeLines(unlist(log_list), con = logfile)
    }
    return( Sys.time() - started )
  } else {
    if (!progress){
      if (is.null(destfile)){
        i <- 1L
        y <- list()
      }
      for (filename in x){
        if (verbose) message("... processing ndjson file: ", filename)
        con <- file(filename, "r")
        while ( TRUE ) {
          line <- readLines(con, n = 1L)
          if ( length(line) == 0 ) break
          y_tmp <- corenlp_parse_json(x = line, cols_to_keep = cols_to_keep, destfile = destfile, logfile = logfile)
          if (is.null(destfile)){
            y[[i]] <- y_tmp
            i <- i + 1L
          }
        }
        close(con)
      }
      if (!is.null(destfile)) return( destfile ) else return( do.call(rbind, y) )
    } else {
      if (is.null(destfile)){
        destfile <- sapply(1L:length(x), function(i) NULL)
      } else {
        if (length(destfile) != length(x)) destfile <- rep(destfile, times = length(x))
      }
      y <- pbapply::pblapply(
        1L:length(x),
        function(i){
          con <- file(x[i], "r")
          lines <- readLines(con)
          df <- corenlp_parse_json(x = lines, cols_to_keep, destfile = destfile[[i]], logfile = logfile, progress = FALSE)
          close(con)
          df
        }
      )
      if (!all(is.null(unlist(destfile)))) return( destfile ) else return(do.call(rbind, y))
    }
  }
}


#' @title Parse CoreNLP json string output.
#' @description Parse the json output from CoreNLP, either text files, or 
#' NDJSON written to one or multiple files.
#' @details The JSON results of applying the Stanford CoreNLP annotators can be
#'   written to a streaming JSON file (ndjson format). \code{corenlp_parse_json}
#'   will parse a json string to a data.frame. If a destfile is specified, the
#'   output will be appended to the file provided. Without a destfile, a
#'   data.frame is returned. Strings that cannot be parsed are written to the
#'   logfile, if it is defined. If \code{filename} is present, the function will
#'   process one or more files with the output of Stanford CoreNLP in a NDJSON
#'   format. If a destfile has been defined during initialization, results are
#'   written/appended to that file. Otherwise, a \code{data.frame} is returned.
#' @importFrom data.table fread rbindlist as.data.table setkeyv setorderv
#' @importFrom jsonlite fromJSON
#' @importFrom utils write.table
#' @export corenlp_parse_json
#' @rdname corenlp_json
corenlp_parse_json = function(x, cols_to_keep = c("sentence", "index", "word", "pos", "ner"), destfile = NULL, logfile = NULL, progress = TRUE){
  if (length(x) == 1){
    # run the parsing within try - coding issues may cause problems
    json_parsed <- try( jsonlite::fromJSON(x) )
    if (class(json_parsed)[1] == "try-error"){
      warning("Cannot parse character vector: ", x)
      if (!is.null(logfile)) cat(x, file = logfile, append = TRUE)
      return( NULL )
    }
    
    # to cope with '{"chunk": 2859285,  "sentences": [ ] }'
    if (length(json_parsed$sentences$tokens) == 0){
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
    if (!is.null(destfile)){
      write.table(
        y, file = destfile,
        sep = "\t",
        append = if (file.exists(destfile)) TRUE else FALSE,
        row.names = FALSE,
        col.names = if (file.exists(destfile)) FALSE else TRUE
      )
      return( NULL )
    } else {
      return( y )
    }
  } else if (length(x) > 1){
    .parse <- function(line) corenlp_parse_json(line, cols_to_keep = cols_to_keep, destfile = destfile, logfile = logfile, progress = FALSE)
    dfs <- if (progress) pblapply(x, .parse) else lapply(x, .parse)
    if (is.null(destfile)) return( do.call(rbind, dfs) ) else return( invisible( NULL ) )
  }
}
