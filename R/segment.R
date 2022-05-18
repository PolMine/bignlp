#' Split table into directories with text segments.
#' 
#' Multithreaded processing using the StanfordCoreNLP class requires splitting
#' up input data into pieces of text ("chunks") available as files that are
#' processed in parallel. The `segment()` function performs this split operation,
#' i.e. it creates directories with chunks within a superdirectory.
#' 
#' @param x A `data.table` with columns 'doc_id' (`integer` values) and 'text'.
#'   Further columns are ignored.
#' @param dir Superdirectory for directories with segments that will be 
#'   processed sequentially.
#' @param chunksize An `integer` value, the number of strings that will reside 
#'   in the chunk directories.
#' @param purge A `logical` value. If `TRUE` (default), potentially disruptive
#'   characters in segments of text are replaced as defined by
#'   `corenlp_preprocessing_replacements`.
#' @param progress A `logical` value, whether to show progress bar.
#' @return The function returns a `character` vector with the directories that 
#'   contain files with text segments.
#' @export segment
#' @examples 
#' library(data.table)
#' reuters_txt <- readLines(system.file(package = "bignlp", "extdata", "txt", "reuters.txt"))
#' dt <- data.table(doc_id = 1L:length(reuters_txt), text = reuters_txt)
#' segdir <- tempdir()
#' dirs <- segment(x = dt, dir = segdir, chunksize = 10L)
segment <- function(x, dir, chunksize = 10L, purge = TRUE, progress = interactive()){
  
  # Check that object x meets requirements ----------------------------
  
  if (isFALSE(is.data.table(x))) stop("Argument 'x' is required to be a data.table object.")
  if (isFALSE("doc_id" %in% colnames(x))) stop("Column 'doc_id' is required.")
  if (isFALSE(is.integer(x[["doc_id"]]))) stop("Column 'doc_id' is required to be an integer vector.")
  if (isFALSE("text" %in% colnames(x))) stop("Column 'text' is required.")
  
  if (nrow(x) > 1L){
    chunk_factor <- cut(
      1L:nrow(x),
      breaks = unique(c(1L, cumsum(rep(chunksize, floor(nrow(x) / chunksize))), nrow(x))),
      include.lowest = TRUE, right = FALSE
    )
  } else {
    chunk_factor <- as.factor(1L)
  }
  chunks <- split(x, f = chunk_factor)
  max_id <- max(x[["doc_id"]])
  
  .fn <- function(i){
    outdir <- file.path(dir, i)
    if (!dir.exists(outdir)) dir.create(outdir)
    
    for (j in 1:nrow(chunks[[i]])){
      f <- file.path(file.path(dir, i, sprintf("%0*d.txt", nchar(max_id),  chunks[[i]][["doc_id"]][j])))
      txt <- chunks[[i]][["text"]][j]
      if (isTRUE(purge)) txt <- purge(txt, replacements = corenlp_preprocessing_replacements, progress = FALSE)
      cat(txt, file = f)
    }
    outdir
  }
  
  chunkdirs <- if (progress) pblapply(1L:length(chunks), .fn) else lapply(1L:length(chunks), .fn)
  unlist(chunkdirs)
}
