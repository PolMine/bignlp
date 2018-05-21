#' Annotate a string.
#' 
#' Use CoreNLP to annotate strings.
#' 
#' @param x input file(s)
#' @param destfile output file, or directory, if multiple files are processed in parallel mode
#' @param properties_file a properties file to configure annotator
#' @param threads integer
#' @param corenlp_dir directory where corenlp resides
#' @param preclean whether to preprocess string
#' @param progress logical
#' @param verbose logical
#' @importFrom pbapply pblapply
#' @importFrom parallel mclapply
#' @importFrom text2vec split_into
#' @importFrom data.table data.table rbindlist fread fwrite uniqueN is.data.table
#' @importFrom R6 R6Class
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
corenlp_annotate <- function(x, destfile = NULL, properties_file, corenlp_dir, method = "json", threads = 1L, progress = TRUE,  preclean = TRUE, verbose = TRUE){
  
  if (is.character(x)){
    if (file.exists(x)){
      if (file.info(x)$isdir == FALSE){
        x <- data.table::fread(x, showProgress = progress)
      } else {
        stop("x is a directory, no procedure to process files in directory implemented")
      }
    }
  }
  stopifnot(is.data.table(x) == TRUE)
  stopifnot(c("id", "text") %in% colnames(x))
  started <- Sys.time()
  if (threads == 1L){
    return_string <- if (is.null(destfile)) {destfile <- tempfile(); TRUE} else FALSE
    Annotator <- AnnotatorCoreNLP$new(
      method = "json",
      destfile = destfile,
      corenlp_dir = corenlp_dir,
      properties_file = properties_file
    )
    .annotate <- function(i) Annotator$annotate(x[["text"]][i], id = x[i][["id"]])
    if (progress) pblapply(1L:nrow(x), .annotate) else lapply(1L:nrow(x), .annotate)
    if (return_string) return(readLines(destfile)) else return(Sys.time() - started)
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
          method = "json", destfile = outfiles[i],
          corenlp_dir = corenlp_dir,
          properties_file = properties_file
        )
        lapply(chunks[[i]], function(j) Annotator$annotate(x[["text"]][j], id = x[j][["id"]]))
        return( outfiles[i] )
      }, mc.cores = threads
    )
  }
  NULL
}





