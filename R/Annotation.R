#' CoreNLP Annotation class.
#' 
#' Generate Annotation class from tokenized data (tabular format).
#' 
#' @param x A `data.frame`.
#' @examples
#' \dontrun{
#' library(polmineR)
#' 
#' p_attrs <- c("word", "pos")
#' x <- data.frame(lapply(
#'   setNames(p_attrs, p_attrs),
#'   function(p_attr) corpus("GERMAPARLMINI") %>%
#'     subset(speaker = "Angela Dorothea Merkel") %>%
#'     get_token_stream(p_attribute = p_attr)
#' ))
#' 
#' a <- as.Annotation(x)
#' 
#' json_outputter <- .jnew("edu.stanford.nlp.pipeline.JSONOutputter")
#' cat(.jcall(json_outputter, "Ljava/lang/String;", "print", a))
#' 
#' pl <- .jnew("edu.stanford.nlp.pipeline.AnnotationPipeline")
#' pl$addAnnotator(.jnew("edu.stanford.nlp.pipeline.WordsToSentencesAnnotator", FALSE))
#' pl$annotate(a)
#' 
#' cat(.jcall(conll_outputter, "Ljava/lang/String;", "print", a))
#' }
#' @export as.Annotation
as.Annotation <- function(x){
  al <- .jnew("java.util.ArrayList")
  chars <- unname(sapply(x[["word"]], nchar))
  begin <- c(0L, cumsum(chars + 1L)[1L:(length(chars) - 1L)])
  end <- begin + chars
  annlist <- pblapply(
    1L:nrow(x),
    function(i){
      cl <- .jnew(
        "edu/stanford/nlp/ling/CoreLabel",
        .jarray(colnames(x)),
        .jarray(as.character(x[i,]))
      )
      cl$setValue(x[i, "word"])
      cl$setOriginalText(x[i, "word"])
      cl$setBeginPosition(begin[i])
      cl$setEndPosition(end[i])
      cl$setBefore("")
      cl$setAfter(" ")
      cl$setIsNewline(FALSE)
      cl$setIndex(i)
      
      al$add(cl)
      NULL
    }
  )
  anno <- .jnew(
    "edu/stanford/nlp/pipeline/Annotation",
    paste(x[["word"]], collapse = " ")
  )
  anno$set(.jnew("edu.stanford.nlp.ling.CoreAnnotations")$TokensAnnotation$class, al)
  anno
}
