#' Annotate a string.
#' 
#' Use CoreNLP to annotate strings.
#' 
#' If argument \code{threads} is 1, the tagging result is returned, if destfile is NULL.
#' If \code{threads} is higher than 1, \code{destfile} should be a directory where tagging
#' results will be stored as NDJSON files.
#' 
#' @param x A \code{data.table} (required to have the columns 'id' and 'text'),
#'   or a character vector with input file(s). Files are assumed to be tsv files
#'   with two columns, 'id' and 'text'.
#' @param destfile An output file, if threads > 1, a directory where ndjson files will be stored.
#' @param properties_file A properties file to configure annotator.
#' @param threads An integer.
#' @param corenlp_dir The directory where corenlp resides.
#' @param preclean Logical, whether to preprocess string.
#' @param method The output generated, either "json" (default), "txt", or "xml".
#' @param progress Logical, whether to show progress bar.
#' @param verbose Logical, whether to output messages.
#' @return The target files will be returned, so that they can serve as input to
#'   \code{corenlp_parse_ndjson}.
#' @importFrom pbapply pblapply
#' @importFrom parallel mclapply
#' @importFrom text2vec split_into
#' @importFrom data.table data.table rbindlist fread fwrite uniqueN is.data.table
#' @importFrom R6 R6Class
#' @importFrom cleanNLP cnlp_download_corenlp
#' @rdname corenlp_annotate
#' @examples 
#' library(data.table)
#' reuters_dt <- data.table(
#'   text = readLines(system.file(package = "bignlp", "extdata", "txt", "reuters.txt"))
#' )
#' reuters_dt[["id"]] <- 1L:nrow(reuters_dt)
#' 
#' destfile <- tempfile()
#' corenlp_dir <- system.file(package = "cleanNLP", "extdata", "stanford-corenlp-full-2016-10-31")
#' properties_english_fast <- system.file(
#'   package = "cleanNLP", "extdata", "StanfordCoreNLP-english-fast.properties"
#' )
#' options(java.parameters = "-Xmx4g")
#' y <- corenlp_annotate(
#'   x = reuters_dt,
#'   destfile = NULL,
#'   properties_file = properties_english_fast,
#'   corenlp_dir = corenlp_dir,
#'   threads = 1L,
#'   progress = FALSE
#'   )
#' @export corenlp_annotate
#' @include bignlp.R
corenlp_annotate <- function(x, destfile = NULL, corenlp_dir = getOption("bignlp.corenlp_dir"), properties_file = getOption("bignlp.properties_file"), method = "json", threads = 1L, progress = TRUE,  preclean = TRUE, verbose = TRUE){
  
  if (is.character(x)){
    if (all(file.exists(x))){
      if (length(x) == 1L){
        if (file.info(x)$isdir == FALSE){
          x <- data.table::fread(x, showProgress = progress)
        } else {
          stop("x is a directory, no procedure to process files in directory implemented")
        }
      } else {
        x <- rbindlist(lapply(x, fread))
      }
    }
  }
  stopifnot(is.data.table(x) == TRUE)
  stopifnot(c("id", "text") %in% colnames(x))
  started <- Sys.time()
  if (threads == 1L){
    return_string <- if (is.null(destfile)) {destfile <- tempfile(); TRUE} else FALSE
    Annotator <- AnnotatorCoreNLP$new(
      method = method,
      destfile = destfile,
      corenlp_dir = corenlp_dir,
      properties_file = properties_file
    )
    .annotate <- function(i) Annotator$annotate(x[["text"]][i], id = x[i][["id"]])
    if (progress) pblapply(1L:nrow(x), .annotate) else lapply(1L:nrow(x), .annotate)
    if (return_string) return( readLines(destfile) ) else return( destfile )
  } else if (threads > 1L){
    java_status <- try(rJava::.jcheck(), silent = TRUE)
    if (class(java_status)[1] != "try-error")
      message("JVM already up and running - parallelisation very likely to fail!")
    if (Sys.getenv("RSTUDIO") == "1")
      warning("for some unknown reason, parallelization does not work when RStudio is running")
    
    chunks <- text2vec::split_into(1L:nrow(x), n = threads)
    outfiles <- sprintf(file.path(destfile, "corenlp_%d.ndjson"), 1L:threads)
    parallel::mclapply(
      1L:length(chunks),
      function(i){
        options(java.parameters = "-Xmx4g")
        Annotator <- AnnotatorCoreNLP$new(
          method = method, destfile = outfiles[i],
          corenlp_dir = corenlp_dir,
          properties_file = properties_file
        )
        lapply(chunks[[i]], function(j) Annotator$annotate(x[["text"]][j], id = x[j][["id"]]))
        return( outfiles[i] )
      }, mc.cores = threads
    )
    return( destfile )
  }
  invisible( NULL )
}


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
  
  corenlp_url <- "http://nlp.stanford.edu/software/stanford-corenlp-full-2017-06-09.zip"
  zipfile <- file.path(corenlp_dir, basename(corenlp_url))
  download.file(url = corenlp_url, destfile = zipfile)
  unzip(zipfile = zipfile, exdir = corenlp_dir)
  file.remove(zipfile)
  
  options(
    bignlp.corenlp_dir = system.file(
      package = "bignlp", "extdata", "corenlp", "stanford-corenlp-full-2017-06-09"
      )
    )
  
  languages <- list(
    de = function(){
      message("... installing model files for: German")
      german_jar_url <- "http://nlp.stanford.edu/software/stanford-german-corenlp-2017-06-09-models.jar"
      german_jar <- file.path(corenlp_dir, "stanford-corenlp-full-2017-06-09", basename(german_jar_url))
      download.file(url = german_jar_url, destfile = german_jar)
      unzip(german_jar, files = "StanfordCoreNLP-german.properties")
      zip(zipfile = german_jar, files = "StanfordCoreNLP-german.properties", flags = "-d")
    },
    en = function(){
      message("... installing model files for: English")
      english_jar_url <- "http://nlp.stanford.edu/software/stanford-english-corenlp-2018-02-27-models.jar"
      english_jar <- file.path(corenlp_dir, "stanford-corenlp-full-2017-06-09", basename(english_jar_url))
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
  } else if (file.exists(system.file(package = "bignlp", "extdata", "corenlp", "stanford-corenlp-full-2017-06-09"))){
    system.file(package = "bignlp", "extdata", "corenlp", "stanford-corenlp-full-2017-06-09")
  } else if (file.exists(system.file(package = "cleanNLP", "extdata", "stanford-corenlp-full-2016-10-31"))){
    system.file(package = "cleanNLP", "extdata", "stanford-corenlp-full-2016-10-31")
  } else {
    ""
  }
}
