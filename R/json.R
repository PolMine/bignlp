#' Parse NDJSON output.
#' 
#' Turn json or ndjson output from Stanford CoreNLP into tabular format
#' 
#' @param input character vector, the JSON string(s) to be parsed
#' @param cols_to_keep columns to keep
#' @param output a character string naming the file to write to
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
#' # parse an ndjson string
#' ndjson_file <- system.file(package = "bignlp", "extdata", "ndjson", "reuters_1.ndjson")
#' ndjson_string <- readLines(ndjson_file)
#' dt <- corenlp_parse_json(ndjson_string, cols = c("word", "pos", "lemma"), progress = FALSE)
#' 
#' # parse ndjson file with / without progress bar
#' destfile <- corenlp_parse_ndjson(input = ndjson_file, output = tempfile(), progress = FALSE)
#' dt <- data.table::fread(destfile)
#' destfile <- corenlp_parse_ndjson(input = ndjson_file, output = tempfile(), progress = TRUE)
#' dt <- data.table::fread(destfile)
#' 
#' # parse all ndjson files in a directory
#' ndjson_dir <- system.file(package = "bignlp", "extdata", "ndjson")
#' tsv_file <- tempfile()
#' destfile <- corenlp_parse_ndjson(input = ndjson_dir, output = tempfile(), threads = 1L)
#' dt <- data.table::fread(destfile)
#' 
#' # parse a set files with specified output files
#' destfiles <- corenlp_parse_ndjson(
#'   input = list.files(ndjson_dir, full.names = TRUE),
#'   output = sprintf("%s/reuters_annotated_%d.tsv", tempdir(), 1L:4L),
#'   threads = 1L
#'   )
#' dt <- data.table::rbindlist(lapply(destfiles, data.table::fread))
#' 
#' if (requireNamespace("jobstatus") && requireNamespace("future")){
#' destfiles <- corenlp_parse_ndjson(
#'   input = list.files(ndjson_dir, full.names = TRUE),
#'   output = sprintf("%s/reuters_annotated_%d.tsv", tempdir(), 1L:4L),
#'   threads = 4L,
#'   byline = TRUE,
#'   progress = TRUE
#' )
#' }
corenlp_parse_ndjson = function(input, cols_to_keep = c("sentence", "index", "word", "pos", "lemma"), output = tempfile(), logfile = NULL, byline = FALSE, threads = 1L, progress = TRUE, verbose = TRUE){
  if (file.info(input[1])[["isdir"]]){
    ndjson_files <- Sys.glob(sprintf("%s/*.ndjson", input))
    if (length(ndjson_files) == 0L) stop("no ndjson files in directory input")
    message("... number of files to process: ", length(ndjson_files))
    # ensure that filenames are processed in the correct order
    if (all(grepl("\\d+", basename(ndjson_files)))){
      ndjson_files <- ndjson_files[order(as.integer(gsub("^.*?(\\d+).*?$", "\\1", basename(ndjson_files))))]
    }
    if (as.integer(threads) == 1L){
      corenlp_parse_ndjson(
        input = ndjson_files,
        cols_to_keep = cols_to_keep,
        output = output,
        logfile = logfile,
        threads = threads,
        progress = progress
      )
    } else {
      message("... using temporary directory: ", tempdir())
      # removing potentially remaining files in tmpdir is necessary, because the NDJSON
      # class would just add new output to an already existing file
      unlink(Sys.glob(sprintf("%s/*.tok", tempdir())))
      unlink(Sys.glob(sprintf("%s/*.log", tempdir())))
      ndjson_files_list <- text2vec::split_into(ndjson_files, n = threads)
      if (!progress){
        output <- parallel::mclapply(
          seq_along(ndjson_files_list),
          function(i){
            tmpdestfile <- file.path(tempdir(), paste(i, "tok", sep = "."))
            corenlp_parse_ndjson(
              input = ndjson_files_list[[i]],
              cols_to_keep = cols_to_keep,
              output = tmpdestfile,
              logfile = logfile,
              threads = 1L,
              progress = FALSE
            )
            tmpdestfile
          },
          mc.cores = threads
        )
      } else {
        stop("processing all files in a directory with progress report not yet supported")
      }
    }
    return(output)
  } else { # input is not a directory
    
    if (length(output) == 1L) output <- rep(output, times = length(input))
    if (length(output) != length(input)) stop("number of input files needs to be identical with number of output files") 
    
    .fn_byline <- function(infile, outfile, verbose = FALSE, progress = FALSE, multicore = FALSE){
      if (verbose) message("... processing ndjson file: ", infile)
      if (multicore){
        f <- file(infile, open = "rb")
        nlines <- 0L
        while (length(chunk <- readBin(f, "raw", 65536)) > 0) nlines <- nlines + sum(chunk == as.raw(10L))
        close(f)
      }
      if (multicore) status <- jobstatus::jobstatus$new(nlines)
      con <- file(infile, "r")
      while (length(line <- readLines(con, n = 1L)) > 0L){
        corenlp_parse_json(
          input = line,
          cols_to_keep = cols_to_keep,
          output = outfile,
          logfile = logfile
        )
        if (multicore) status$tick()
      }
      if (multicore) status$finish()
      close(con)
    }
    
    .fn_all <- function(infile, outfile, verbose, progress){
      if (verbose) message("... processing ndjson file: ", infile)
      con <- file(infile, "r")
      lines <- readLines(con)
      corenlp_parse_json(
        input = lines,
        cols_to_keep,
        output = output[[i]],
        logfile = logfile,
        progress = progress
      )
      close(con)
    }
    
    if (!progress){
      if (any(file.exists(output))) file.remove(output)
      if (threads == 1L){
        if (byline){
          for (i in seq_along(input)) .fn_byline(infile = input[[i]], outfile = output[[i]], verbose = verbose, progress = FALSE)
        } else {
          for (i in seq_along(input)) .fn_all(infile = input[[i]], outfile = output[[i]], verbose = verbose, progress = FALSE)
        }
      } else {
        if (byline){
          mclapply(
            seq_along(input),
            function(i) .fn_byline(infile = input[[i]], outfile = output[[i]], verbose = FALSE, progress = FALSE),
            mc.cores = threads
          )
        } else {
          mclapply(
            seq_along(input),
            function(i) .fn_all(infile = input[[i]], outfile = output[[i]], verbose = FALSE, progress = FALSE),
            mc.cores = threads
          )
        }
      }
      return( unique(output) )
    } else {
      if (threads == 1L){
        if (byline){
          pblapply(
            seq_along(input),
            function(i) .fn_byline(infile = input[[i]], outfile = output[[i]], verbose = verbose, progress = TRUE),
            cl = 1L
          )
        } else {
          pblapply(
            seq_along(input),
            function(i) .fn_all(infile = input[[i]], outfile = output[[i]], verbose = verbose, progress = TRUE),
            cl = 1L
          )
        }
      } else {
        if (!requireNamespace("jobstatus", quietly = TRUE))
          stop("Package 'jobstatus' required but not available. Install it from ",
               'GitHub:\ndevtools::install_github("ropenscilabs/jobstatus")')
        
        if (!requireNamespace("future", quietly = TRUE)) stop("Package 'future' required but not available.")
        
        library(future) # how to omit this?
        library(jobstatus) # how to omit this?
        library(progress) # how to omit this?
        
        future::plan(multiprocess) # in package 'future'

        if (byline){
          
          .fn <- function(infile, outfile){
            f <- file(infile, open = "rb")
            nlines <- 0L
            while (length(chunk <- readBin(f, "raw", 65536)) > 0) nlines <- nlines + sum(chunk == as.raw(10L))
            close(f)
            
            status <- jobstatus$new(nlines)
            con <- file(infile, "r")
            while (length(line <- readLines(con, n = 1L)) > 0L){
              Sys.sleep(1)
              corenlp_parse_json(input = line, cols_to_keep = cols_to_keep, output = outfile, logfile = logfile)
              status$tick()
            }
            status$finish()
            close(con)
          }

        } else {
          
          .fn <- function(infile, outfile){
            con <- file(infile, "r")
            lines <- readLines(con)
            close(con)
            status <- jobstatus$new(length(lines))
            lapply(
              lines,
              function(line){
                corenlp_parse_json(
                  input = line,
                  cols_to_keep = cols_to_keep,
                  output = outfile,
                  logfile = logfile,
                  progress = progress
                )
                Sys.sleep(1)
                status$tick()
              }
            )
            status$finish()
          }
        }
        
        y <- jobstatus::with_jobstatus({
          fns <- sprintf(
            "fn%d <- jobstatus::subjob_future(expr = .fn(infile = '%s', outfile = '%s'))",
            1L:length(input), input, output
          )
          eval(parse(text = paste(fns, collapse = ";")))
          
        }, display = jobstatus::percentage
        )
        
      }
      return(output)
    }
  }
}


#' @title Parse CoreNLP json string output.
#' @description Parse the json output from CoreNLP, either text files, or 
#' NDJSON written to one or multiple files.
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
