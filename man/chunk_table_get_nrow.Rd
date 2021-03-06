% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{chunk_table_get_nrow}
\alias{chunk_table_get_nrow}
\title{Get number of rows of a table on disk.}
\usage{
chunk_table_get_nrow(filename)
}
\arguments{
\item{filename}{Name of a file (full path).}
}
\description{
Auxiliary function to get the number of rows of a file as fast as possible.
The implementation of \code{chunk_table_get_nrow} follows closely the fastest
pure R solution suggested in a \href{https://stackoverflow.com/questions/23456170/get-the-number-of-lines-in-a-text-file-using-r}{discussion at Stack Overflow}.
}
\examples{
library(data.table)

# First, generate a chunk_table file
reuters_chunk_table <- file.path(tempdir(), "reuters_chunk_table.tsv")
reuters_txt <- readLines(system.file(package = "bignlp", "extdata", "txt", "reuters.txt"))
reuters_dt <- data.table(doc_id = 1L:length(reuters_txt), text = reuters_txt)
data.table::fwrite(x = reuters_dt, file = reuters_chunk_table)

# Get nrow of the file. Note that the file includes colnames, so there is
# one row in addition to the nrow of the original chunk_table
n <- chunk_table_get_nrow(reuters_chunk_table)
}
