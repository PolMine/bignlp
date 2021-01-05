#' Annotate a string.
#' 
#' Use CoreNLP to annotate strings.
#' 
#' If argument \code{threads} is 1, the tagging result is returned, if output is NULL.
#' If \code{threads} is higher than 1, \code{output} should be a directory where tagging
#' results will be stored as NDJSON files.
#' 
#' @param input Either a \code{data.table} (required to have the columns 'id' and
#'   'text'), or a character vector with input file(s), or a directory. If
#'   \code{input} is a directory, all files in the directory are processed. Files
#'   are assumed to be tsv files with two columns ('id' and 'text').
#' @param output An output file, if threads > 1, a directory where ndjson files will be stored.
#' @param properties_file A properties file to configure annotator.
#' @param logfile A logfile for progress information.
#' @param gc_interval Frequency of garbage collection.
#' @param report_interval When to report progress report to logfile.
#' @param threads An integer.
#' @param corenlp_dir The directory where corenlp resides.
#' @param preclean Logical, whether to preprocess string.
#' @param byline Logical, whether to process files in a line-by-line manner.
#' @param method The output generated, either "json" (default), "txt", or "xml".
#' @param progress Logical, whether to show progress bar.
#' @param verbose Logical, whether to output messages.
#' @param ... Further arguments.
#' @return The target files will be returned, so that they can serve as input to
#'   \code{corenlp_parse_ndjson}.
#' @importFrom pbapply pblapply
#' @importFrom parallel mclapply
#' @importFrom text2vec split_into
#' @importFrom data.table data.table rbindlist fread fwrite uniqueN is.data.table
#' @importFrom R6 R6Class
#' @importFrom cleanNLP cnlp_download_corenlp
#' @importFrom stats setNames
#' @importFrom progress progress_bar
#' @rdname corenlp_annotate
#' @examples 
#' library(data.table)
#' reuters_txt <- readLines(system.file(package = "bignlp", "extdata", "txt", "reuters.txt"))
#' reuters_dt <- data.table(id = 1L:length(reuters_txt), text = reuters_txt)
#' 
#' options(java.parameters = "-Xmx4g")
#' 
#' y <- corenlp_annotate(
#'   input = reuters_dt,
#'   output = NULL,
#'   properties_file = corenlp_get_properties_file(lang = "en", fast = "TRUE"),
#'   corenlp_dir = corenlp_get_jar_dir(),
#'   threads = 1L,
#'   progress = FALSE
#'   )
#' 
#' \dontrun{
#' reuters_dt <- rbindlist(lapply(1:100, function(i) reuters_dt))
#' reuters_dt[["id"]] <- 1L:nrow(reuters_dt)
#' 
#' if (requireNamespace("jobstatus", quietly = TRUE)){
#'   y <- corenlp_annotate(
#'     input = reuters_dt,
#'     output = NULL,
#'     properties_file = corenlp_get_properties_file(lang = "en", fast = "TRUE"),
#'     corenlp_dir = corenlp_get_jar_dir(),
#'     threads = 2L,
#'     progress = TRUE
#'   )
#' }
#' 
#' }
#' @export corenlp_annotate
#' @include bignlp.R
setGeneric("corenlp_annotate", function(input, ...) standardGeneric("corenlp_annotate"))


#' @rdname corenlp_annotate
setMethod("corenlp_annotate", "data.table", function(input, output = NULL, corenlp_dir = getOption("bignlp.corenlp_dir"), properties_file = getOption("bignlp.properties_file"), logfile = NULL, report_interval = 1L, gc_interval = 100L, method = "json", threads = 1L, progress = TRUE,  preclean = TRUE, verbose = TRUE){
  stopifnot(c("id", "text") %in% colnames(input))
  
  if (threads == 1L){
    return_string <- if (is.null(output)) {output <- tempfile(); TRUE} else FALSE
    if (file.exists(output)) file.remove(output)
    Annotator <- StanfordCoreNLP$new(
      method = method,
      destfile = output,
      corenlp_dir = corenlp_dir,
      properties_file = properties_file,
      logfile = logfile,
      target = nrow(input),
      report_interval = report_interval,
      gc_interval = gc_interval
    )
    .annotate <- function(i) Annotator$annotate(input[["text"]][i], id = input[i][["id"]], current = i)
    if (progress) pblapply(1L:nrow(input), .annotate) else lapply(1L:nrow(input), .annotate)
    if (return_string) return( readLines(output) ) else return( output )
    
  } else if (threads > 1L){
    stop("Processing a data.table using multiple threads is not implemented.")
  }
})

#' @details The \code{corenlp_annotate}-method can be used with in two different
#'   ways. If \code{byline} is \code{FALSE}, the method will read in the chunk
#'   table specified by argument \code{input}, of the files in a directory
#'   specified by \code{input} and hand over to the
#'   \code{corenlp_annotate}-method for \code{data.table} objects. If
#'   \code{byline} is \code{TRUE}, the input is processed in a line-by-line
#'   manner. If \code{output} is specified, it is required that the number of
#'   output files is identical with the number of input files. If \code{output}
#'   is \code{NULL}, as many temporary files will be generated, as there are
#'   number of threads.
#' @examples
#' # Illustration of byline-mode, with and without progress bar.
#' # Single-threaded
#' 
#' chunk_table <- system.file(package = "bignlp", "extdata", "tsv", "unga.tsv")
#' options("bignlp.properties_file" = corenlp_get_properties_file(lang = "en", fast = "TRUE"))
#' tmp_outfile <- corenlp_annotate(input = chunk_table, byline = TRUE, progress = FALSE)
#' tmp_ndjsonoutfile <- corenlp_annotate(input = chunk_table, byline = TRUE, progress = TRUE)
#' 
#' # Multi-threading
#' \dontrun{
#' unga_subsets <- chunk_table_split(chunk_table, output = NULL, n = 2L, verbose = TRUE)
#' unga_ndjson_files <- corenlp_annotate(
#'   input = unga_subsets, threads = 2L, byline = TRUE, progress = FALSE
#' )
#' unga_ndjson_files <- corenlp_annotate(
#'   input = unga_subsets, threads = 2L, byline = TRUE, progress = TRUE
#' )
#' }
#' @importFrom progress progress_bar
#' @rdname corenlp_annotate
setMethod("corenlp_annotate", "character", function(input, output = NULL, corenlp_dir = getOption("bignlp.corenlp_dir"), properties_file = getOption("bignlp.properties_file"), byline = NULL, method = "json", threads = 1L, progress = TRUE,  preclean = TRUE, verbose = TRUE){
  if (!all(file.exists(input))) stop("Stopping - all input files need to exist!")
  if (!is.null(byline)){
    stop("The argument 'byline' is deprecated.")
  }
  
  if (threads == 1L){
    if (file.info(input)$isdir == TRUE) stop("The input is a directory, a single file is expected.")
    if (is.null(output)){
      output <- file.path(
        tempdir(),
        paste(
          gsub("^(.*)\\..*?$", "\\1", basename(input)),
          method,
          sep = "."
        )
      )
      if (file.exists(output)) file.remove(output)
    }
    if (progress) chunks_total <- chunk_table_get_nrow(input) - 1L # leave header out of calculation
    Annotator <- StanfordCoreNLP$new(
      method = method, destfile = output,
      corenlp_dir = corenlp_dir,
      properties_file = properties_file
    )
    if (progress) pb <- progress_bar$new(total = chunks_total)
    f <- file(input, open = "r")
    readLines(f, n = 1L) # skip header
    while(length(line_to_process <- readLines(f, n = 1L)) > 0){
      chunk_data <- setNames(strsplit(x = line_to_process, split = "\\t")[[1]], c("id", "text"))
      # If chunk data has been saved using data.table::fwrite(), argument 'quote = "auto"' may
      # have as a consequence that the chunk data text is wrapped into quotes - remove quotes if 
      # necessary
      if (grepl('^".*"$', chunk_data[["text"]])) chunk_data[["text"]] <- gsub('^"(.*)"$', "\\1", chunk_data[["text"]])
      Annotator$annotate(txt = chunk_data[["text"]], id = as.integer(chunk_data[["id"]]))
      if (progress) pb$tick()
    }
    close(f)
    if (progress) pb$terminate()
    return(output)
  } else if (threads >= 2L){
    
    jvm_is_initialized()
    if (Sys.getenv("RSTUDIO") == "1") warning("Parallel byline processing with progress very likely to file in RStudio, try running it from command line.")
    
    if (is.null(output)){
      output <- file.path(
        tempdir(),
        paste(gsub("^(.*)\\..*?$", "\\1", basename(input)), method, sep = ".")
      )
      if (any(file.exists(output))) file.remove(output)
    }
    
    fn <- function(i){
      options(java.parameters = "-Xmx4g")
      Annotator <- StanfordCoreNLP$new(
        method = method, destfile = output[[i]],
        corenlp_dir = corenlp_dir,
        properties_file = properties_file
      )
      # if (progress){
      #   chunks_total <- chunk_table_get_nrow(input[[i]]) - 1L # leave header out of calculation
      #   # status <- jobstatus::jobstatus$new(chunks_total)
      # }
      f <- file(input[[i]], open = "r")
      readLines(f, n = 1L) # skip header
      while(length(line_to_process <- readLines(f, n = 1L)) > 0){
        chunk_data <- setNames(strsplit(x = line_to_process, split = "\\t")[[1]], c("id", "text"))
        Annotator$annotate(txt = chunk_data[["text"]], id = as.integer(chunk_data[["id"]]))
        # if (progress) status$tick()
      }
      close(f)
      # if (progress) status$finish()
      return( output[[i]] )
    }
    
    if (progress){
    } else {
      retval <- mclapply(1L:threads, fn, mc.cores = threads)
      return(unlist(retval))
    }
  }
})



#' @name corenlp_install
#' @title Install Stanford CoreNLP.
#' @description The function provides an installation mechanism to download and
#'   install Stanford CoreNLP within the bignlp package or externally.
#' @param lang Languages to install.
#' @param loc Directory where to put jar files. If missing, the files will be
#'   placed in the bignlp package.
#' @export corenlp_install
#' @rdname corenlp_install
#' @importFrom utils download.file unzip zip
corenlp_install <- function(lang = "de", loc){
  # create necessary directories
  if (missing(loc)) loc <- system.file(package = "bignlp", "extdata")
  exttools_dir <- loc
  if (!file.exists(exttools_dir)) dir.create(exttools_dir)
  corenlp_dir <- file.path(exttools_dir, "corenlp")
  if (!file.exists(corenlp_dir)) dir.create(corenlp_dir)
  
  corenlp_url <- "http://nlp.stanford.edu/software/stanford-corenlp-4.2.0.zip"
  zipfile <- file.path(corenlp_dir, basename(corenlp_url))
  download.file(url = corenlp_url, destfile = zipfile)
  unzip(zipfile = zipfile, exdir = corenlp_dir)
  file.remove(zipfile)
  
  options(
    bignlp.corenlp_dir = system.file(
      package = "bignlp", "extdata", "corenlp", "stanford-corenlp-4.2.0"
      )
    )
  
  languages <- list(
    de = function(){
      message("... installing model files for: German")
      german_jar_url <- "http://nlp.stanford.edu/software/stanford-corenlp-4.2.0-models-german.jar"
      german_jar <- file.path(corenlp_dir, "stanford-corenlp-4.2.0", basename(german_jar_url))
      download.file(url = german_jar_url, destfile = german_jar)
      unzip(german_jar, files = "StanfordCoreNLP-german.properties")
      zip(zipfile = german_jar, files = "StanfordCoreNLP-german.properties", flags = "-d")
    },
    en = function(){
      message("... installing model files for: English")
      english_jar_url <- "http://nlp.stanford.edu/software/stanford-corenlp-4.2.0-models-english.jar"
      english_jar <- file.path(corenlp_dir, "stanford-corenlp-4.2.0", basename(english_jar_url))
      download.file(url = english_jar_url, destfile = english_jar)
      unzip(english_jar, files = "StanfordCoreNLP.properties")
      zip(zipfile = english_jar, files = "StanfordCoreNLP.properties", flags = "-d")
    }
  )
  for (language in lang) languages[[lang]]()
}

#' Get path to properties file.
#' 
#' @param lang the language
#' @param fast fast property file?
#' @export corenlp_get_properties_file
corenlp_get_properties_file <- function(lang = c("en", "de"), fast = TRUE){
  
  if (lang == "en" && fast == TRUE)
    return(system.file(package = "bignlp", "extdata", "properties_files", "StanfordCoreNLP-english-fast.properties"))
  
  if (lang == "de" && fast == TRUE)
    return(system.file(package = "bignlp", "extdata", "properties_files", "corenlp-german-fast.properties"))
  
}

#' Get directory with CoreNLP jar files.
#' 
#' @export corenlp_get_jar_dir
corenlp_get_jar_dir <- function(){
  if (nchar(Sys.getenv("CORENLP_DIR")) > 0L){
    Sys.getenv("CORENLP_DIR")
  } else if (file.exists(system.file(package = "bignlp", "extdata", "corenlp", "stanford-corenlp-4.2.0"))){
    system.file(package = "bignlp", "extdata", "corenlp", "stanford-corenlp-4.2.0")
  } else if (file.exists(system.file(package = "cleanNLP", "extdata", "stanford-corenlp-full-2016-10-31"))){
    system.file(package = "cleanNLP", "extdata", "stanford-corenlp-full-2016-10-31")
  } else {
    ""
  }
}


