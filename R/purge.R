#' Purge character vector from villain characters.
#' 
#' Both preprocessing and postprocessing required!
#' 
#' @param x character vector to purge
#' @param replacements ...
#' @param progress logical
#' @export purge
#' @rdname purge
#' @importFrom utils txtProgressBar setTxtProgressBar
purge <- function(x, replacements, progress = TRUE){
  if (progress) pb <- txtProgressBar(min = 1L, max = length(replacements), style = 3)
  for (i in 1L:length(replacements)){
    if (progress) setTxtProgressBar(pb, i)
    x <- gsub(replacements[[i]][1], replacements[[i]][2], x)
  }
  if (progress) close(pb)
  x
}

#' @export corenlp_postprocessing_replacements
#' @rdname purge
#' @examples 
#' library(data.table)
#' options(java.parameters = "-Xmx4g")
#' txt <- "Karadžiš!"
#' corenlp_dir <- system.file(package = "cleanNLP", "extdata", "stanford-corenlp-full-2016-10-31")
#' corenlp_dir <- "/opt/stanford-corenlp/stanford-corenlp-full-2018-10-05"
#' properties_german_fast <- corenlp_get_properties_file(lang = "de")
#' y <- corenlp_annotate(
#'   input = data.table(id = 1L:length(txt), text = txt),
#'   properties_file = properties_german_fast, corenlp_dir = corenlp_dir,
#'   )
corenlp_postprocessing_replacements <- list(
  c("a<`", "\uE0"), c("e<'", "\uE9"), c("o<\\^", "\u00F4"), c("<vs", "s"),
  c("<'c", "c"), c("s<v", "\u0161"), c("a<'", "\uE1"), c("<vs", "\u0161"),
  c("<i<'", "\uED"), c("e<`", "\uE8"), c("o<'", "\uF3"), c("z<v", "\u17E"), c("c<'", "\u107"),
  c("<vz", "\u17E")
)

#' @export corenlp_preprocessing_replacements
#' @rdname purge
corenlp_preprocessing_replacements <- list(c("\u202F", ""), c("\uFFFD", ""), c("\u2011", "-"))