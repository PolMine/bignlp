#' Get number of rows of a table on disk.
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
#' reuters_dt <- data.table(doc_id = 1L:length(reuters_txt), text = reuters_txt)
#' data.table::fwrite(x = reuters_dt, file = reuters_chunk_table)
#' 
#' # Get nrow of the file. Note that the file includes colnames, so there is
#' # one row in addition to the nrow of the original chunk_table
#' n <- chunk_table_get_nrow(reuters_chunk_table)
chunk_table_get_nrow <- function(filename){
  f <- file(filename, open = "rb")
  nlines <- 0L
  while (length(chunk <- readBin(f, "raw", 65536)) > 0)
    nlines <- nlines + sum(chunk == as.raw(10L))
  close(f)
  nlines
}


#' Get directory with CoreNLP jar files.
#' 
#' This utility function checks three options where CoreNLP jars might be: (a) The environment
#' variable 'CORENLP_DIR', then in the directory 'extdata/corenlp' of the bignlp, and finally,
#' in the package cleanNLP.
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


#' Get path to properties file.
#' 
#' The package includes minimal, adapted versions of the properties files for
#' English and German for testing purposes.
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


#' Get version of CoreNLP
#' @export corenlp_get_version
corenlp_get_version <- function(){
  as.numeric_version(
    gsub(
      "^.*?(\\d+\\.\\d+\\.\\d+).*?$", "\\1",
      grep(
        "^stanford-corenlp-\\d+\\.\\d+\\.\\d\\.jar$",
        basename(rJava::.jclassPath()),
        value = TRUE),
      perl = TRUE
    )
  )
}

# Get version of latest CoreNLP release
#' @export
corenlp_latest <- function(){
  trystatus <- try({
    page <- xml2::read_html("https://stanfordnlp.github.io/CoreNLP/history.html")
  })
  if (is(trystatus)[[1]] == "try-error") return(NULL)
  v_node <- xml2::xml_find_first(page, xpath = "//table/tbody/tr/td")
  if (length(v_node) > 0L){
    return(as.numeric_version(xml2::xml_text(v_node)))
  } else {
    return(NULL)
  }
}
