#' Get number of rows for chunk table on disk.
#' 
#' Auxiliary function to get the number of rows of a file as fast as possible.
#' The implementation of \code{chunk_table_get_nrow} follows closely the fastest
#' pure R solution suggested in a [discussion at Stack
#' Overflow](https://stackoverflow.com/questions/23456170/get-the-number-of-lines-in-a-text-file-using-r).
#' @param filename Name of a file (full path).
#' @export chunk_table_get_nrow
#' @examples
#' library(data.table)
#' 
#' # First, generate a chunk_table file
#' reuters_chunk_table <- file.path(tempdir(), "reuters_chunk_table.tsv")
#' reuters_txt <- readLines(system.file(package = "bignlp", "extdata", "txt", "reuters.txt"))
#' reuters_dt <- data.table(id = 1L:length(reuters_txt), text = reuters_txt)
#' data.table::fwrite(x = reuters_dt, file = reuters_chunk_table)
#' 
#' # Get nrow of the file. Note that the file includes colnames, so there is
#' # one row in addition to the nrow of the original chunk_table
#' n <- chunk_table_get_nrow(reuters_chunk_table)
chunk_table_get_nrow <- function(filename){
  f <- file(filename, open = "rb")
  nlines <- 0L
  while (length(chunk <- readBin(f, "raw", 65536)) > 0) nlines <- nlines + sum(chunk == as.raw(10L))
  close(f)
  nlines
}


#' Split up file into subsets.
#' 
#' Split up a chunk table file into equally sized files for parallel processing.
#'
#' The file specified by \code{input} needs to be a single existing file. It
#' will be splitted into \code{n} roughly equally sized files. If \code{output} is
#' \code{NULL}, the files will be generated in the session temporary directory.
#' 
#' If \code{byline} is \code{TRUE} (default), the input file is processed in a 
#' line-by-line mode, to keep memory consumption minimal. (Note that only byline
#' mode is implemented for this function at this stage.)
#'
#' The return value is the files that have been written, so that the
#' \code{chunk_table_split}-function can be used in a pipe.
#' 
#' Note that the input file is assumed to have a header (first line with
#' colnames). This header will be prefixed to all output files.
#' 
#' @param input A length-one \code{character} vector, path name (full path) of a
#'   chunk table file.
#' @param n An \code{integer} value, the number of (rougly equally sized) files
#'   to generate.
#' @param output The output file(s). If \code{NULL}, files will be written to
#'   the session temporary directory.
#' @param byline A \code{logical} value, whether to process files in a
#'   line-by-line mode.
#' @param verbose A \code{logical} value, whether to print messages.
#' @examples
#' library(data.table)
#' infile <- system.file(package = "bignlp", "extdata", "tsv", "unga.tsv")
#' outfiles <- chunk_table_split(infile, n = 3L)
#' 
#' n_output_tables <- sapply(lapply(outfiles, data.table::fread), nrow)
#' sum(n_output_tables) == nrow(fread(infile))
#' @export chunk_table_split
chunk_table_split <- function(input, output = NULL, n, byline = TRUE, verbose = TRUE){
  
  # input needs to be a filename (not a directory) of an existing file
  stopifnot(
    length(input) == 1L,
    is.character(input),
    file.exists(input),
    file.info(input)$isdir == FALSE
    )
  
  if (n == 1L){
    message("... with n = 1, no splitting required: returning input filename")
    return(input)
  }
  
  if (verbose) message("... number of rows in input file: ", appendLF = FALSE)
  n_lines_input <- chunk_table_get_nrow(input)
  if (verbose) message(sprintf("%d (including header)", n_lines_input))
  
  subsets <- do.call(
    rbind,
    lapply(text2vec::split_into(2L:n_lines_input, n = n), function(x) c(min(x), max(x)))
  )
  
  if (is.null(output)){
    output <- file.path(
      tempdir(),
      paste(
        gsub("^(.*?)\\..*?$", "\\1", basename(input)),
        "_", 1L:n,
        gsub("^.*(\\..*?)$", "\\1", basename(input)),
        sep = ""
      )
    )
  } else {
    if (length(output) != n){
      stop("The number of output files needs to be identical with the number of subsets (argument n) to be generated")
    }
  }
  
  if (byline){
    file_input <- file(input, open = "r")
    header <- readLines(file_input, n = 1L)
    for (i in 1L:n){
      if (verbose) message(
        sprintf("... writing %d lines (excluding header) to file:\n\t %s", subsets[i,2] - subsets[i,1] + 1, output[i])
      )
      y <- file(output[i], open = "w")
      writeLines(text = header, con = y)
      for (j in subsets[i,1]:subsets[i,2])
        writeLines(text = readLines(file_input, n = 1L), con = y)
      close(y)
    }
    close(file_input)
  } else {
    stop("Argument byline is FALSE, but function chunk_table_split only implemented for byline mode at this stage.")
  }
  output
}