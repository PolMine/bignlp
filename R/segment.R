#' Split table into directories with text segments.
#' 
#' Multithreaded processing using the StanfordCoreNLP class requires splitting
#' up input data into pieces of text ("chunks") available as files that are
#' processed in parallel. The `segment()` function performs this split operation,
#' i.e. it creates directories with chunks within a superdirectory.
#' 
#' @param x A `data.table` with columns 'id' and 'text'. Further columns are
#'   ignored.
#' @param dir Superdirectory for directories with segments that will be 
#'   processed sequentially.
#' @param chunksize An `integer` value, the number of strings that will reside 
#'   in the chunk directories.
#' @param progress A `logical` value, whether to show progress bar.
#' @return The function returns a `character` vector with the directories that 
#'   contain files with text segments.
#' @export segment
#' @examples 
#' library(data.table)
#' reuters_txt <- readLines(system.file(package = "bignlp", "extdata", "txt", "reuters.txt"))
#' dt <- data.table(id = 1L:length(reuters_txt), text = reuters_txt)
#' segdir <- tempdir()
#' dirs <- segment(x = dt, dir = segdir, chunksize = 10L)
segment <- function(x, dir, chunksize = 10L, progress = interactive()){
  
  # Check that object x meets requirements ----------------------------
  
  if (isFALSE(is.data.table(x))) stop("Argument 'x' is required to be a data.table object.")
  if (isFALSE("id" %in% colnames(x))) stop("Column 'id' is required.")
  if (isFALSE("text" %in% colnames(x))) stop("Column 'text' is required.")
  
  chunk_factor <- cut(
    1:nrow(x),
    breaks = unique(c(1L, cumsum(rep(chunksize, floor(nrow(x) / chunksize))), nrow(x))),
    include.lowest = TRUE, right = FALSE
  )
  chunks <- split(x, f = chunk_factor)
  
  .fn <- function(i){
    outdir <- file.path(dir, i)
    if (!dir.exists(outdir)) dir.create(outdir)
    
    for (j in 1:nrow(chunks[[i]])){
      f <- file.path(file.path(dir, i, sprintf("%d.txt", chunks[[i]][["id"]][j])))
      cat(chunks[[i]][["text"]][j], file = f)
    }
    outdir
  }
  
  chunkdirs <- if (progress) pblapply(1L:length(chunks), .fn) else lapply(1L:length(chunks), .fn)
  unlist(chunkdirs)
}